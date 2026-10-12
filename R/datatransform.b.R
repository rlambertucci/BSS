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
    
    .get_normality_p = function(x, test_type = "shapiro") {
      n <- length(x)
      if (n < 3) return(NA_real_)
      data_sample <- x
      if (test_type == "shapiro" && n > 5000) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
          on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
        }
        set.seed(12345)
        data_sample <- sample(x, 5000)
      }
      
      if (test_type == "anderson_darling") {
        if (requireNamespace("nortest", quietly = TRUE)) {
          res <- tryCatch(nortest::ad.test(data_sample), error = function(e) NULL)
          if (!is.null(res)) return(as.numeric(res$p.value))
        }
        return(private$.calc_anderson_darling(data_sample)$p)
      } else if (test_type == "shapiro_francia") {
        if (requireNamespace("nortest", quietly = TRUE)) {
          res <- tryCatch(nortest::sf.test(data_sample), error = function(e) NULL)
          if (!is.null(res)) return(as.numeric(res$p.value))
        }
        return(private$.calc_shapiro_francia(data_sample)$p)
      } else {
        res <- tryCatch(stats::shapiro.test(data_sample), error = function(e) NULL)
        if (!is.null(res)) return(as.numeric(res$p.value))
        return(NA_real_)
      }
    },
    
    .init = function() {
      if (is.null(self$options$vars) || length(self$options$vars) == 0) return()
      
      html <- self$results$transformInfo
      html$setContent("<h3>Data Transformation</h3><p>Power, bounded, and rank-based inverse normal transformations for Gaussian normalization. Missing values (NAs) are preserved.</p>")
      
      if (self$options$show_lambda_table) {
        tbl <- self$results$lambdaTable
        tbl$setNote(
          "citation",
          "Note. Box-Cox and Yeo-Johnson transformations are performed using bestNormalize. Johnson curves follow SuppDists. Arcsine and Logit follow Bartlett (1937) and Cox (1970). Rank-based inverse normal transformations (INT) follow Blom (1958) and van der Waerden (1952). Citations: (1) Peterson (2021); (2) Wheeler (2020); (3) Box & Cox (1964); (4) Yeo & Johnson (2000); (5) Johnson (1949); (6) Bartlett (1937); (7) Blom (1958); (8) van der Waerden (1952)."
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
            tbl$addRow(rowKey=var, values=list(variable=var, method="Empirical Logit", parameter="Epsilon (\u03b5)"))
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
        norm_method <- if (!is.null(self$options$normality_test)) self$options$normality_test else "shapiro"
        if (norm_method == "anderson_darling") {
          ntbl$setNote(
            "citation",
            "Note. Anderson-Darling normality test is performed based on Stephens (1974). Citation: Stephens, M. A. (1974). EDF statistics for goodness of fit and some comparisons. JASA, 69(347), 730-737."
          )
        } else if (norm_method == "shapiro_francia") {
          ntbl$setNote(
            "citation",
            "Note. Shapiro-Francia normality test is performed based on Royston (1993). Citation: Royston, P. (1993). A pocket-calculator algorithm for the Shapiro-Francia test for non-normality. Statistics in Medicine, 12(2), 181-184."
          )
        } else {
          ntbl$setNote(
            "citation",
            "Note. Shapiro-Wilk normality test is performed using stats::shapiro.test. Citation: Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). Biometrika, 52(3/4), 591-611."
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
          "Note. Methods are evaluated and ranked by minimizing composite deviation from Gaussian normality: Criterion = |Skewness| + 0.5 \u00d7 |Excess Kurtosis|. Lower criterion scores indicate shapes closer to the standard normal bell curve. Citations: Blom (1958); van der Waerden (1952); Peterson (2021)."
        )
      }
    },
    
    .run = function() {
      vars <- self$options$vars
      if (is.null(vars) || length(vars) == 0) return()
      
      data <- self$data
      method <- self$options$method
      auto_shift <- self$options$auto_shift
      alpha_sw <- self$options$alpha_sw
      norm_test <- if (!is.null(self$options$normality_test)) self$options$normality_test else "shapiro"
      int_form <- if (!is.null(self$options$int_formula)) self$options$int_formula else "blom"
      all_treated_vectors <- list()
      
      for (var in vars) {
        x <- private$.to_numeric_vec(data[[var]])
        if (is.null(x)) next
        is_valid <- !is.na(x)
        x_clean <- x[is_valid]
        
        if (length(x_clean) < 3 || stats::var(x_clean) == 0) next
        
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
          scale_val <- 1
          if (max(x_clean, na.rm=TRUE) > 1.05 && max(x_clean, na.rm=TRUE) <= 100 && min(x_clean, na.rm=TRUE) >= 0) {
            scale_val <- 100
          }
          p_val <- x_clean / scale_val
          p_bounded <- pmin(pmax(p_val, 0), 1)
          x_trans_clean <- asin(sqrt(p_bounded))
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=scale_val, shift_applied="No", shift_value=0))
          }
        } else if (method == "logit") {
          scale_val <- 1
          if (max(x_clean, na.rm=TRUE) > 1.05 && max(x_clean, na.rm=TRUE) <= 100 && min(x_clean, na.rm=TRUE) >= 0) {
            scale_val <- 100
          }
          p_val <- x_clean / scale_val
          p_bounded <- pmin(pmax(p_val, 0), 1)
          eps <- 0.001
          p_adj <- (p_bounded * (length(p_bounded) - 1) + 0.5) / length(p_bounded)
          x_trans_clean <- log(p_adj / (1 - p_adj))
          if (self$options$show_lambda_table) {
            self$results$lambdaTable$setRow(rowKey=var, values=list(value=eps, shift_applied="No", shift_value=0))
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
          jt <- tryCatch(
            SuppDists::JohnsonFit(x_clean, moment="quant"),
            error = function(e) {
              tryCatch(SuppDists::JohnsonFit(x_clean, moment="use"), error = function(e2) NULL)
            }
          )
          
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
          if (self$options$show_qq_plot) self$results$qqPlots$get(key=var)$setState(data.frame(before=x_clean, after=x_trans_clean))
          if (self$options$show_hist_plot) self$results$histPlots$get(key=var)$setState(data.frame(before=x_clean, after=x_trans_clean))
          if (self$options$show_density_plot) self$results$densityPlots$get(key=var)$setState(data.frame(before=x_clean, after=x_trans_clean))
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
            jt_fit <- SuppDists::JohnsonFit(x_clean, moment = "quant")
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
          
          if (min_val >= 0 && max(x_clean) <= 100) {
            scale_prop <- ifelse(max(x_clean) > 1.05, 100, 1)
            p_unit <- x_clean / scale_prop
            cands[["Arcsine Square Root"]] <- asin(sqrt(pmin(pmax(p_unit, 0), 1)))
            p_logit <- (pmin(pmax(p_unit, 0), 1) * (n_c - 1) + 0.5) / n_c
            cands[["Empirical Logit"]] <- log(p_logit / (1 - p_logit))
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
      }
    },
    
    runNormality = function(var, data_clean, moment, alpha, test_type="shapiro") {
      n <- length(data_clean)
      if (n < 3) return()
      
      data_sample <- data_clean
      if (test_type == "shapiro" && n > 5000) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
          on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
        }
        set.seed(12345)
        data_sample <- sample(data_clean, 5000)
        self$results$normalityTable$setNote(
          "sample_size_note",
          "Note. For sample sizes N > 5000, Shapiro-Wilk statistic is computed on a representative random subsample of N = 5000."
        )
      }
      
      stat_val <- NA
      p_val <- NA
      
      if (test_type == "anderson_darling") {
        if (requireNamespace("nortest", quietly = TRUE)) {
          res <- tryCatch(nortest::ad.test(data_sample), error=function(e) NULL)
          if (!is.null(res)) {
            stat_val <- as.numeric(res$statistic)
            p_val <- as.numeric(res$p.value)
          }
        }
        if (is.na(stat_val)) {
          ad_res <- private$.calc_anderson_darling(data_sample)
          stat_val <- ad_res$stat
          p_val <- ad_res$p
        }
      } else if (test_type == "shapiro_francia") {
        if (requireNamespace("nortest", quietly = TRUE)) {
          res <- tryCatch(nortest::sf.test(data_sample), error=function(e) NULL)
          if (!is.null(res)) {
            stat_val <- as.numeric(res$statistic)
            p_val <- as.numeric(res$p.value)
          }
        }
        if (is.na(stat_val)) {
          sf_res <- private$.calc_shapiro_francia(data_sample)
          stat_val <- sf_res$stat
          p_val <- sf_res$p
        }
      } else {
        res <- tryCatch(stats::shapiro.test(data_sample), error=function(e) NULL)
        if (!is.null(res)) {
          stat_val <- as.numeric(res$statistic)
          p_val <- as.numeric(res$p.value)
        }
      }
      
      self$results$normalityTable$setRow(
        rowKey = paste0(var, "_", moment),
        values = list(
          W = stat_val,
          p = p_val,
          result = ifelse(!is.na(p_val) && p_val > alpha, "Normal", "Non-Normal")
        )
      )
    },
    
    .calc_anderson_darling = function(x) {
      n <- length(x)
      s <- stats::sd(x)
      if (n < 5 || is.na(s) || s == 0) return(list(stat=NA, p=NA))
      y <- sort((x - mean(x)) / s)
      p <- stats::pnorm(y)
      p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
      h <- (2 * seq_len(n) - 1) * (log(p) + log(1 - rev(p)))
      A <- -n - mean(h)
      AA <- A * (1 + 0.75 / n + 2.25 / (n^2))
      
      pval <- if (AA >= 0.60) {
        exp(1.2937 - 5.709 * AA + 0.0186 * (AA^2))
      } else if (AA > 0.34) {
        exp(0.9177 - 4.279 * AA - 1.38 * (AA^2))
      } else if (AA > 0.20) {
        1 - exp(-8.318 + 42.796 * AA - 59.938 * (AA^2))
      } else {
        1 - exp(-13.436 + 101.14 * AA - 223.73 * (AA^2))
      }
      pval <- max(0, min(1, pval))
      list(stat = A, p = pval)
    },
    
    .calc_shapiro_francia = function(x) {
      n <- length(x)
      if (n < 5 || stats::sd(x) == 0) return(list(stat=NA, p=NA))
      y <- sort(x)
      m <- stats::qnorm((seq_len(n) - 0.375) / (n + 0.25))
      w <- (sum(m * y)^2) / (sum(m^2) * sum((y - mean(y))^2))
      
      u <- log(log(n)) - log(n)
      mu <- -1.2725 + 1.0521 * u
      sig <- 1.0308 - 0.26758 * u
      z <- (log(1 - pmin(w, 0.99999)) - mu) / sig
      pval <- stats::pnorm(z, lower.tail = FALSE)
      list(stat = w, p = pval)
    },
    
    .qqPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      df <- image$state
      p1 <- ggplot2::ggplot(df, ggplot2::aes(sample=before)) + ggplot2::stat_qq(color="#336699") + ggplot2::stat_qq_line(color="red") + ggplot2::labs(title="Before Transformation") + ggtheme
      p2 <- ggplot2::ggplot(df, ggplot2::aes(sample=after)) + ggplot2::stat_qq(color="#339966") + ggplot2::stat_qq_line(color="red") + ggplot2::labs(title="After Transformation") + ggtheme
      print(gridExtra::grid.arrange(p1, p2, ncol=2)); TRUE
    },
    
    .histPlot = function(image, ggtheme, ...) {
      if (is.null(image$state) || nrow(image$state) == 0) return(FALSE)
      df <- image$state
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x=before)) + ggplot2::geom_histogram(fill="#336699", bins=30) + ggplot2::labs(title="Before Transformation") + ggtheme
      p2 <- ggplot2::ggplot(df, ggplot2::aes(x=after)) + ggplot2::geom_histogram(fill="#339966", bins=30) + ggplot2::labs(title="After Transformation") + ggtheme
      print(gridExtra::grid.arrange(p1, p2, ncol=2)); TRUE
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
      
      print(gridExtra::grid.arrange(p1, p2, ncol=2))
      TRUE
    }
  )
)
