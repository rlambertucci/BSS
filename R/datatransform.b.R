dataTransformClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "dataTransformClass",
  inherit = dataTransformBase,
  private = list(
    
    .to_numeric_vec = function(x) {
      if (is.null(x)) return(NULL)
      if (is.numeric(x)) return(as.numeric(x))
      if (is.factor(x)) {
        num_try <- suppressWarnings(as.numeric(as.character(x)))
        if (!all(is.na(num_try)) || all(is.na(x))) return(num_try)
        return(as.numeric(x))
      }
      if (is.character(x)) {
        return(suppressWarnings(as.numeric(x)))
      }
      suppressWarnings(as.numeric(x))
    },
    
    .calc_skewness = function(x) {
      n <- length(x)
      if (n < 3) return(NA_real_)
      m <- mean(x)
      m2 <- mean((x - m)^2)
      if (m2 == 0) return(0)
      m3 <- mean((x - m)^3)
      m3 / (m2^(1.5))
    },
    
    .calc_kurtosis = function(x) {
      n <- length(x)
      if (n < 4) return(NA_real_)
      m <- mean(x)
      m2 <- mean((x - m)^2)
      if (m2 == 0) return(0)
      m4 <- mean((x - m)^4)
      (m4 / (m2^2)) - 3
    },
    
    .normality_result = function(x, test_type = "shapiro") {
      n <- length(x)
      minimum <- switch(test_type, anderson_darling = 8L, shapiro_francia = 5L, 3L)
      label <- switch(test_type, anderson_darling = "Anderson-Darling", shapiro_francia = "Shapiro-Francia", "Shapiro-Wilk")
      if (n < minimum || any(!is.finite(x))) return(list(stat = NA_real_, p = NA_real_, note = paste0(label, " requires at least ", minimum, " finite observations.")))
      if (stats::sd(x) == 0) return(list(stat = NA_real_, p = NA_real_, note = paste0(label, " is unavailable for constant data.")))
      data_sample <- x
      note <- NULL
      if (test_type %in% c("shapiro", "shapiro_francia") && n > 5000) {
        seed_present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        previous_seed <- if (seed_present) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
        on.exit({
          if (seed_present) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(list = ".Random.seed", envir = .GlobalEnv)
        }, add = TRUE)
        set.seed(12345L)
        data_sample <- sample(x, 5000)
        note <- paste0(label, " uses a deterministic random subsample of 5000 from N = ", n, " (seed 12345); the caller RNG state is restored.")
      }
      error_message <- NULL
      result <- tryCatch(
        switch(test_type,
          anderson_darling = nortest::ad.test(data_sample),
          shapiro_francia = nortest::sf.test(data_sample),
          stats::shapiro.test(data_sample)
        ), error = function(e) { error_message <<- conditionMessage(e); NULL }
      )
      if (is.null(result) || !is.finite(result$p.value)) return(list(stat = NA_real_, p = NA_real_, note = paste0(label, " could not be computed. ", error_message)))
      list(stat = as.numeric(result$statistic), p = as.numeric(result$p.value), note = note)
    },

    .get_normality_p = function(x, test_type = "shapiro") {
      private$.normality_result(x, test_type)$p
    },

    .fit_johnson = function(x) {
      tryCatch(SuppDists::JohnsonFit(x, moment = "quant"),
        error = function(e) tryCatch(SuppDists::JohnsonFit(x, moment = "find"), error = function(e2) NULL))
    },

    .bounded_values = function(values) {
      scale <- if (identical(self$options$bounded_scale, "percentage")) 100 else 1
      if (length(values) == 0 || any(!is.finite(values)) || any(values < 0 | values > scale)) return(NULL)
      values / scale
    },

    .update_help = function() {
      if (!is.null(self$results$transformInfo)) self$results$transformInfo$setVisible(length(self$options$vars) == 0)
      scale_notice <- if (length(self$options$vars) > 0 && isTRUE(self$options$method %in% c("boxcox", "yeojohnson"))) "Box-Cox and Yeo-Johnson values are centered and scaled to sample mean zero and sample standard deviation one (bestNormalize default standardize=TRUE). Saved values are not on the raw power-transformation scale." else NULL
      if (!is.null(self$results$lambdaTable)) self$results$lambdaTable$setNote("transformation_scale", scale_notice)
    },

    .init = function() {
      private$.update_help()
      
      html <- self$results$transformInfo
      html$setContent("<h3>Data Transformation</h3><p>Power, bounded, and rank-based inverse normal transformations for Gaussian normalization. Missing values (NAs) are preserved.</p><p>Box-Cox and Yeo-Johnson also center and scale the power-transformed analysis values to sample mean zero and sample standard deviation one (bestNormalize default: standardize = TRUE). Saved values are on this standardized scale, not the raw power-transformation scale.</p><p>Logit uses the Smithson-Verkuilen (2006) compression p_adj = (p*(n-1)+0.5)/n, followed by log(p_adj/(1-p_adj)); this is not a count-based empirical logit.</p>")
      if (is.null(self$options$vars) || length(self$options$vars) == 0) return()
      
      if (self$options$show_lambda_table) {
        tbl <- self$results$lambdaTable
        tbl$setNote(
          "citation",
          "Box-Cox and Yeo-Johnson transformations are performed using bestNormalize. Johnson curves follow SuppDists. Arcsine applies to proportions or rates. Logit uses Smithson-Verkuilen (2006) compression p_adj = (p*(n-1)+0.5)/n before log(p_adj/(1-p_adj)); no epsilon parameter is used. Rank-based inverse normal transformations (INT) use the selected plotting-position formula."
        )
        for (var in self$options$vars) {
          if (self$options$method == "johnson") {
            tbl$addRow(rowKey=paste0(var, "_type"), values=list(variable=var, method="Johnson", parameter="System (1=SU, 2=SB, 3=SL, 4=SN)"))
            tbl$addRow(rowKey=paste0(var, "_gamma"), values=list(variable=var, method="Johnson", parameter="Gamma"))
            tbl$addRow(rowKey=paste0(var, "_delta"), values=list(variable=var, method="Johnson", parameter="Delta"))
            tbl$addRow(rowKey=paste0(var, "_xi"), values=list(variable=var, method="Johnson", parameter="Xi"))
            tbl$addRow(rowKey=paste0(var, "_lambda"), values=list(variable=var, method="Johnson", parameter="Lambda"))
          } else if (self$options$method == "arcsine") {
            tbl$addRow(rowKey=var, values=list(variable=var, method="Arcsine", parameter="Scale Factor"))
          } else if (self$options$method == "logit") {
            tbl$addRow(rowKey=var, values=list(variable=var, method="Logit (Smithson-Verkuilen)", parameter="Compression sample size (n)"))
          } else if (self$options$method == "int") {
            int_name <- switch(
              self$options$int_formula,
              "blom" = "Blom (c = 3/8)",
              "vdw" = "Van der Waerden (c = 0)",
              "tukey" = "Tukey (c = 1/3)",
              "rankit" = "Rankit (c = 1/2)",
              "Blom (c = 3/8)"
            )
            tbl$addRow(rowKey=var, values=list(variable=var, method="Rank-Based INT", parameter=int_name))
          } else {
            method_name <- ifelse(self$options$method == "boxcox", "Box-Cox", "Yeo-Johnson")
            tbl$addRow(rowKey=var, values=list(variable=var, method=method_name, parameter="Lambda (\u03bb)"))
          }
        }
      }
      
      if (self$options$show_normality_table) {
        ntbl <- self$results$normalityTable
        ntbl$setNote("decision", "Labels compare the p-value with the selected significance level; p > alpha does not establish normality. Unavailable tests are reported as 'Not available'.")
        norm_method <- if (!is.null(self$options$normality_test)) self$options$normality_test else "shapiro"
        if (norm_method == "anderson_darling") {
          ntbl$setNote(
            "citation",
            "Anderson-Darling normality test evaluates departures from a normal distribution."
          )
        } else if (norm_method == "shapiro_francia") {
          ntbl$setNote(
            "citation",
            "Shapiro-Francia normality test evaluates departures from a normal distribution."
          )
        } else {
          ntbl$setNote(
            "citation",
            "Shapiro-Wilk normality test is performed using stats::shapiro.test."
          )
        }
        for (var in self$options$vars) {
          ntbl$addRow(rowKey=paste0(var, "_before"), values=list(variable=var, moment="Before"))
          ntbl$addRow(rowKey=paste0(var, "_after"), values=list(variable=var, moment="After"))
        }
      }
      
      if (self$options$show_recommendation_table) {
        rec_tbl <- self$results$recommendationTable
        rec_tbl$setNote(
          "recommender_note",
          "Methods are evaluated and ranked by minimizing composite deviation from Gaussian normality: Criterion = |Skewness| + 0.5 \u00d7 |Excess Kurtosis|. Lower criterion scores indicate shapes closer to the standard normal bell curve."
        )
      }
    },
    
    .run = function() {
      private$.update_help()
      self$results$transformStatus$setContent("")
      vars <- self$options$vars
      if (is.null(vars) || length(vars) == 0) return()
      
      data <- self$data
      method <- self$options$method
      auto_shift <- self$options$auto_shift
      alpha_sw <- self$options$alpha_sw
      norm_test <- if (!is.null(self$options$normality_test)) self$options$normality_test else "shapiro"
      int_form <- if (!is.null(self$options$int_formula)) self$options$int_formula else "blom"
      all_treated_vectors <- list()
      skipped_vars <- character()
      
      for (var in vars) {
        x <- private$.to_numeric_vec(data[[var]])
        if (is.null(x)) {
          skipped_vars <- c(skipped_vars, paste0(var, " (no numeric data)"))
          next
        }
        is_valid <- !is.na(x)
        x_clean <- x[is_valid]
        
        if (length(x_clean) < 3 || any(!is.finite(x_clean)) || stats::var(x_clean) == 0) {
          reason <- if (length(x_clean) < 3) "fewer than 3 valid values" else if (any(!is.finite(x_clean))) "non-finite values" else "zero variance"
          skipped_vars <- c(skipped_vars, paste0(var, " (", reason, ")"))
          next
        }
        
        shift_val <- 0
        x_trans_clean <- NULL
        
        if (method == "boxcox") {
          min_x <- min(x_clean)
          if (min_x <= 0) {
            if (auto_shift) { 
              shift_val <- abs(min_x) + 0.001
              x_clean <- x_clean + shift_val
            } else {
              if (self$options$show_lambda_table) {
                self$results$lambdaTable$setNote(
                  "boxcox_warning",
                  "Warning: Box-Cox transformation requires strictly positive values (X > 0). Enable 'Automatic Shift' or select Yeo-Johnson."
                )
                self$results$lambdaTable$setRow(
                  rowKey=var,
                  values=list(value=NA, shift_applied="No (Error: non-positive values)", shift_value=0)
                )
              }
              next
            }
          }
          bc <- bestNormalize::boxcox(x_clean)
          x_trans_clean <- bc$x.t
          shift_applied <- ifelse(shift_val > 0, "Yes", "No")
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=bc$lambda, shift_applied=shift_applied, shift_value=shift_val))
          }
        } else if (method == "yeojohnson") {
          yj <- bestNormalize::yeojohnson(x_clean)
          x_trans_clean <- yj$x.t
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=yj$lambda, shift_applied="No", shift_value=0))
          }
        } else if (method == "arcsine") {
          scale_val <- if (identical(self$options$bounded_scale, "percentage")) 100 else 1
          p_bounded <- private$.bounded_values(x_clean)
          if (is.null(p_bounded)) {
            skipped_vars <- c(skipped_vars, paste0(var, " (requires finite values in [0, ", scale_val, "]; select the correct input scale)"))
            next
          }
          x_trans_clean <- asin(sqrt(p_bounded))
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=scale_val, shift_applied="No", shift_value=0))
          }
        } else if (method == "logit") {
          scale_val <- if (identical(self$options$bounded_scale, "percentage")) 100 else 1
          p_bounded <- private$.bounded_values(x_clean)
          if (is.null(p_bounded)) {
            skipped_vars <- c(skipped_vars, paste0(var, " (requires finite values in [0, ", scale_val, "]; select the correct input scale)"))
            next
          }
          p_adj <- (p_bounded * (length(p_bounded) - 1) + 0.5) / length(p_bounded)
          x_trans_clean <- log(p_adj / (1 - p_adj))
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=length(p_bounded), shift_applied="No", shift_value=0))
          }
        } else if (method == "int") {
          c_val <- switch(
            int_form,
            "blom" = 0.375,
            "vdw" = 0.0,
            "tukey" = 1 / 3,
            "rankit" = 0.5,
            0.375
          )
          n_c <- length(x_clean)
          r_c <- rank(x_clean, ties.method = "average")
          p_ranks <- (r_c - c_val) / (n_c - 2 * c_val + 1)
          p_ranks <- pmin(pmax(p_ranks, 1e-09), 1 - 1e-09)
          x_trans_clean <- stats::qnorm(p_ranks)
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(
              rowKey=var,
              values=list(value=c_val, shift_applied="No", shift_value=0)
            )
          }
        } else if (method == "johnson") {
          jt <- private$.fit_johnson(x_clean)
          
          if (is.null(jt)) {
            if (self$options$show_lambda_table) {
              self$results$lambdaTable$setNote(
                "johnson_warning",
                "Warning: Johnson curve estimation failed to converge for one or more variables. Consider Yeo-Johnson transformation."
              )
            }
            next
          }
          
          y <- (x_clean - jt$xi) / jt$lambda
          if (jt$type == "SL") {
            y <- pmax(y, 1e-09)
          } else if (jt$type == "SB") {
            y <- pmin(pmax(y, 1e-09), 1 - 1e-09)
          }
          x_trans_clean <- switch(jt$type, "SN"=jt$gamma+jt$delta*y, "SL"=jt$gamma+jt$delta*log(y), "SU"=jt$gamma+jt$delta*asinh(y), "SB"=jt$gamma+jt$delta*log(y/(1-y)))
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=paste0(var, "_type"), values=list(value=switch(jt$type,"SU"=1,"SB"=2,"SL"=3,"SN"=4,0)))
            self$results$lambdaTable$setRow(rowKey=paste0(var, "_gamma"), values=list(value=jt$gamma))
            self$results$lambdaTable$setRow(rowKey=paste0(var, "_delta"), values=list(value=jt$delta))
            self$results$lambdaTable$setRow(rowKey=paste0(var, "_xi"), values=list(value=jt$xi))
            self$results$lambdaTable$setRow(rowKey=paste0(var, "_lambda"), values=list(value=jt$lambda))
          }
        }
        
        if (!is.null(x_trans_clean)) {
          x_trans_full <- rep(NA, length(x))
          x_trans_full[is_valid] <- x_trans_clean
          all_treated_vectors[[var]] <- x_trans_full
          
          if (self$options$show_normality_table) {
            private$runNormality(var, x_clean, "before", alpha_sw, norm_test)
            private$runNormality(var, x_trans_clean, "after", alpha_sw, norm_test)
          }
          if (self$options$show_qq_plot || self$options$show_hist_plot || self$options$show_density_plot) {
            image <- self$results$distributionPlots$get(key = var)
            image$setState(data.frame(before = x_clean, after = x_trans_clean))
            image$setSize(650, 400 * sum(c(self$options$show_qq_plot, self$options$show_hist_plot, self$options$show_density_plot)))
          }
        }
        
        if (self$options$show_recommendation_table) {
          rec_tbl <- self$results$recommendationTable
          cands <- list()
          
          cands[["Original (No Transform)"]] <- x_clean
          
          yj_try <- tryCatch(bestNormalize::yeojohnson(x_clean)$x.t, error = function(e) NULL)
          if (!is.null(yj_try)) cands[["Yeo-Johnson"]] <- yj_try
          
          bc_in <- x_clean
          if (min(bc_in) <= 0) bc_in <- bc_in + abs(min(bc_in)) + 0.001
          bc_try <- tryCatch(bestNormalize::boxcox(bc_in)$x.t, error = function(e) NULL)
          if (!is.null(bc_try)) cands[["Box-Cox"]] <- bc_try
          
          n_c <- length(x_clean)
          r_c <- rank(x_clean, ties.method = "average")
          p_blom <- (r_c - 0.375) / (n_c + 0.25)
          p_blom <- pmin(pmax(p_blom, 1e-09), 1 - 1e-09)
          cands[["Rank-Based INT (Blom)"]] <- stats::qnorm(p_blom)
          
          p_vdw <- r_c / (n_c + 1)
          p_vdw <- pmin(pmax(p_vdw, 1e-09), 1 - 1e-09)
          cands[["Rank-Based INT (Van der Waerden)"]] <- stats::qnorm(p_vdw)
          
          j_try <- tryCatch({
            jt_fit <- private$.fit_johnson(x_clean)
            if (is.null(jt_fit)) stop("Johnson fitting failed")
            y_j <- (x_clean - jt_fit$xi) / jt_fit$lambda
            if (jt_fit$type == "SL") y_j <- pmax(y_j, 1e-09)
            if (jt_fit$type == "SB") y_j <- pmin(pmax(y_j, 1e-09), 1 - 1e-09)
            switch(jt_fit$type,
              "SN" = jt_fit$gamma + jt_fit$delta * y_j,
              "SL" = jt_fit$gamma + jt_fit$delta * log(y_j),
              "SU" = jt_fit$gamma + jt_fit$delta * asinh(y_j),
              "SB" = jt_fit$gamma + jt_fit$delta * log(y_j / (1 - y_j))
            )
          }, error = function(e) NULL)
          if (!is.null(j_try) && !any(is.na(j_try))) cands[["Johnson Curve"]] <- j_try
          
          min_val <- min(x_clean)
          shift_log <- if (min_val <= 0) abs(min_val) + 0.001 else 0
          cands[["Natural Log (ln)"]] <- log(x_clean + shift_log)
          
          shift_sqrt <- if (min_val < 0) abs(min_val) else 0
          cands[["Square Root"]] <- sqrt(x_clean + shift_sqrt)
          
          p_unit <- private$.bounded_values(x_clean)
          if (!is.null(p_unit)) {
            cands[["Arcsine Square Root"]] <- asin(sqrt(p_unit))
            p_logit <- (p_unit * (n_c - 1) + 0.5) / n_c
            cands[["Logit (Smithson-Verkuilen)"]] <- log(p_logit / (1 - p_logit))
          }
          
          cand_results <- list()
          for (c_name in names(cands)) {
            vec <- cands[[c_name]]
            if (is.null(vec) || length(vec) < 3 || stats::var(vec, na.rm=TRUE) == 0) next
            sk <- private$.calc_skewness(vec)
            kt <- private$.calc_kurtosis(vec)
            if (is.na(sk) || is.na(kt)) next
            score_val <- abs(sk) + 0.5 * abs(kt)
            norm_p <- private$.get_normality_p(vec, norm_test)
            cand_results[[length(cand_results) + 1]] <- list(
              method = c_name,
              skew = sk,
              kurt = kt,
              score = score_val,
              p = norm_p
            )
          }
          
          if (length(cand_results) > 0) {
            scores <- vapply(cand_results, function(cr) cr$score, numeric(1))
            ord <- order(scores)
            cand_results <- cand_results[ord]
            
            for (rk in seq_along(cand_results)) {
              cr <- cand_results[[rk]]
              row_k <- paste0(var, "_rec_", rk)
              rec_tbl$addRow(
                rowKey = row_k,
                values = list(
                  variable = var,
                  rank = as.integer(rk),
                  method = cr$method,
                  skewness = cr$skew,
                  kurtosis = cr$kurt,
                  score = cr$score,
                  normality_p = cr$p,
                  recommended = if (rk == 1) "Recommended (*)" else ""
                )
              )
            }
          }
        }
      }
      
      if (length(skipped_vars) > 0) self$results$transformStatus$setContent(paste0("Variables not transformed: ", paste(skipped_vars, collapse = "; "), "."))
      if (length(all_treated_vectors) > 0 && self$results$transformedVarsOutput$isNotFilled()) {
        keys <- names(all_treated_vectors)
        titles <- paste0(keys, self$options$suffix)
        self$results$transformedVarsOutput$set(
          keys=keys, 
          titles=titles, 
          descriptions=rep("Transformed variable", length(keys)), 
          measureTypes=rep("continuous", length(keys))
        )
        for (var in keys) self$results$transformedVarsOutput$setValues(key=var, values=all_treated_vectors[[var]])
        self$results$transformedVarsOutput$setRowNums(rownames(data))
      }
    },
    
    runNormality = function(var, data_clean, moment, alpha, test_type="shapiro") {
      result <- private$.normality_result(data_clean, test_type)
      self$results$normalityTable$setNote("sample_size_note", NULL)
      self$results$normalityTable$setNote(paste0("normality_", var, "_", moment), result$note)
      self$results$normalityTable$setRow(
        rowKey = paste0(var, "_", moment),
        values = list(
          W = result$stat,
          p = result$p,
          result = if (is.na(result$p)) "Not available" else if (result$p > alpha) "p > \u03b1" else "p \u2264 \u03b1"
        )
      )
    },

    .distributionPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      panels <- list()
      if (self$options$show_qq_plot) panels[[length(panels) + 1L]] <- private$.qqPlot(image, ggtheme)
      if (self$options$show_hist_plot) panels[[length(panels) + 1L]] <- private$.histPlot(image, ggtheme)
      if (self$options$show_density_plot) panels[[length(panels) + 1L]] <- private$.densityPlot(image, ggtheme)
      if (length(panels) == 0) return(FALSE)
      grid::grid.draw(gridExtra::arrangeGrob(grobs = panels, ncol = 1))
      TRUE
    },

    .qqPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      df <- image$state
      p1 <- ggplot2::ggplot(df, ggplot2::aes(sample=before)) + ggplot2::stat_qq(color="#336699") + ggplot2::stat_qq_line(color="red") + ggplot2::labs(title="Before Transformation") + ggtheme
      p2 <- ggplot2::ggplot(df, ggplot2::aes(sample=after)) + ggplot2::stat_qq(color="#339966") + ggplot2::stat_qq_line(color="red") + ggplot2::labs(title="After Transformation") + ggtheme
      gridExtra::arrangeGrob(p1, p2, ncol = 2)
    },
    
    .histPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      df <- image$state
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x=before)) + ggplot2::geom_histogram(fill="#336699", bins=30) + ggplot2::labs(title="Before Transformation") + ggtheme
      p2 <- ggplot2::ggplot(df, ggplot2::aes(x=after)) + ggplot2::geom_histogram(fill="#339966", bins=30) + ggplot2::labs(title="After Transformation") + ggtheme
      gridExtra::arrangeGrob(p1, p2, ncol = 2)
    },
    
    .densityPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      df <- image$state
      
      m1 <- mean(df$before, na.rm=TRUE)
      s1 <- stats::sd(df$before, na.rm=TRUE)
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x=before)) +
        ggplot2::geom_density(fill="#336699", alpha=0.4, color="#336699") +
        ggplot2::stat_function(fun=stats::dnorm, args=list(mean=m1, sd=s1), color="red", linetype="dashed", linewidth=0.8) +
        ggplot2::labs(title="Before (Empirical vs Normal)", x="Value", y="Density") +
        ggtheme
      
      m2 <- mean(df$after, na.rm=TRUE)
      s2 <- stats::sd(df$after, na.rm=TRUE)
      p2 <- ggplot2::ggplot(df, ggplot2::aes(x=after)) +
        ggplot2::geom_density(fill="#339966", alpha=0.4, color="#339966") +
        ggplot2::stat_function(fun=stats::dnorm, args=list(mean=m2, sd=s2), color="red", linetype="dashed", linewidth=0.8) +
        ggplot2::labs(title="After (Empirical vs Normal)", x="Value", y="Density") +
        ggtheme
      
      gridExtra::arrangeGrob(p1, p2, ncol = 2)
    }
  )
)
