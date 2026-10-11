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
    
    .init = function() {
      if (is.null(self$options$vars) || length(self$options$vars) == 0) return()
      
      html <- self$results$transformInfo
      html$setContent("<h3>Data Transformation</h3><p>Power and bounded transformations for normality. NAs are preserved.</p>")
      
      if (self$options$show_lambda_table) {
        tbl <- self$results$lambdaTable
        tbl$setNote(
          "citation",
          "Note. Box-Cox and Yeo-Johnson transformations are performed using the bestNormalize package. Johnson curve transformations are performed using the SuppDists package. Arcsine and Logit transformations follow Bartlett (1937) and Cox (1970). Citations: (1) Peterson, R. A. (2021). Finding optimal normalizing transformations via bestNormalize. The R Journal, 13(1), 310-329. (2) Wheeler, B. (2020). SuppDists: Supplementary Distributions. (3) Box, G. E., & Cox, D. R. (1964). An analysis of transformations. JRSS-B, 26(2), 211-243. (4) Yeo, I. K., & Johnson, R. A. (2000). Biometrika, 87(4), 955-969. (5) Johnson, N. L. (1949). Biometrika, 36(1/2), 149-176. (6) Bartlett, M. S. (1937). Supplement to JRSS, 4(1), 137-183."
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
    },
    
    .run = function() {
      vars <- self$options$vars
      if (is.null(vars) || length(vars) == 0) return()
      
      data <- self$data
      method <- self$options$method
      auto_shift <- self$options$auto_shift
      alpha_sw <- self$options$alpha_sw
      norm_test <- if (!is.null(self$options$normality_test)) self$options$normality_test else "shapiro"
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
        # Default Shapiro-Wilk
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
      
      # Before plot with normal curve overlay
      m1 <- mean(df$before, na.rm=TRUE)
      s1 <- stats::sd(df$before, na.rm=TRUE)
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x=before)) +
        ggplot2::geom_density(fill="#336699", alpha=0.4, color="#336699") +
        ggplot2::stat_function(fun=stats::dnorm, args=list(mean=m1, sd=s1), color="red", linetype="dashed", linewidth=0.8) +
        ggplot2::labs(title="Before (Empirical vs Normal)", x="Value", y="Density") +
        ggtheme
      
      # After plot with normal curve overlay
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
