.bss_seed_modes <- c(seed_mice_cs = "use_seed_mice_cs", seed_mice_long = "use_seed_mice_long", seed_mr = "use_seed_mr")

.bss_seed_arguments <- function(arguments) {
  for (mode in intersect(unname(.bss_seed_modes), names(arguments))) {
    value <- arguments[[mode]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) jmvcore::reject("Fixed-seed mode must be one Boolean value.", code = "invalid_seed")
  }
  for (name in intersect(names(.bss_seed_modes), names(arguments))) {
    value <- arguments[[name]]
    mode <- .bss_seed_modes[[name]]
    empty <- is.null(value) || (is.character(value) && length(value) == 1L && !is.na(value) && !nzchar(trimws(value)))
    if (empty) {
      if (isTRUE(arguments[[mode]])) jmvcore::reject(paste0(name, " requires an integer when a fixed seed is enabled."), code = "invalid_seed")
      arguments[[name]] <- 0L
      if (!mode %in% names(arguments)) arguments[[mode]] <- FALSE
      next
    }
    if (!(is.numeric(value) || is.character(value)) || is.complex(value) || length(value) != 1L) jmvcore::reject(paste0(name, " must be one integer between -2147483647 and 2147483647."), code = "invalid_seed")
    numeric_value <- suppressWarnings(as.numeric(value))
    if (!is.finite(numeric_value) || numeric_value != trunc(numeric_value) || numeric_value < -2147483647 || numeric_value > 2147483647) jmvcore::reject(paste0(name, " must be one integer between -2147483647 and 2147483647. Legacy fractional or invalid seeds must be corrected explicitly; they are not rounded or replaced."), code = "invalid_seed")
    arguments[[name]] <- as.integer(numeric_value)
    if (!mode %in% names(arguments)) arguments[[mode]] <- TRUE
  }
  arguments
}

dataimputationOptions <- if (requireNamespace("jmvcore", quietly = TRUE)) local({
  generated <- dataimputationOptions
  R6::R6Class(
    "dataimputationOptions",
    inherit = generated,
    public = list(
      initialize = function(...) {
        do.call(super$initialize, .bss_seed_arguments(list(...)))
      },
      fromProtoBuf = function(pb) {
        super$fromProtoBuf(private$.seed_proto(pb))
      },
      compProtoBuf = function(pb) {
        super$compProtoBuf(private$.seed_proto(pb))
      }
    ),
    private = list(
      .seed_proto = function(pb) {
        arguments <- list()
        for (name in intersect(names(.bss_seed_modes), pb$names)) {
          positions <- which(pb$names == name)
          if (length(positions) != 1L) jmvcore::reject("Saved seed options must have unique names.", code = "invalid_seed")
          option <- pb$options[[positions]]
          value <- if (option$has("s")) option$s else if (option$has("i")) option$i else if (option$has("d")) option$d else NULL
          if (is.null(value) && !(option$has("o") && option$o == 2L)) jmvcore::reject(paste0(name, " is not a saved integer or numeric string."), code = "invalid_seed")
          arguments[name] <- list(value)
        }
        for (mode in intersect(unname(.bss_seed_modes), pb$names)) {
          positions <- which(pb$names == mode)
          if (length(positions) > 1L) jmvcore::reject("Saved seed modes must have unique names.", code = "invalid_seed")
          if (length(positions) == 1L) {
            option <- pb$options[[positions]]
            if (!option$has("o") || !option$o %in% c(0L, 1L)) jmvcore::reject("Saved seed mode must be Boolean.", code = "invalid_seed")
            arguments[[mode]] <- option$o == 1L
          }
        }
        if (length(arguments) == 0L) return(pb)
        arguments <- .bss_seed_arguments(arguments)
        result <- pb$clone()
        for (name in names(arguments)) {
          positions <- which(result$names == name)
          option <- result$options[[1L]]$clone()
          option$clear()
          if (name %in% names(.bss_seed_modes)) option$i <- arguments[[name]]
          else option$o <- if (arguments[[name]]) "TRUE" else "FALSE"
          if (length(positions) == 1L) result$options[[positions]] <- option
          else {
            result$add("names", name)
            result$add("options", option)
          }
        }
        result
      }
    )
  )
})

dataimputation <- if (requireNamespace("jmvcore", quietly = TRUE)) local({
  generated <- dataimputation
  wrapper <- function(...) {
    call <- match.call()
    seed_names <- intersect(c(names(.bss_seed_modes), unname(.bss_seed_modes)), names(call))
    arguments <- lapply(as.list(call)[seed_names], eval, envir = parent.frame())
    arguments <- .bss_seed_arguments(arguments)
    for (name in names(arguments)) call[[name]] <- arguments[[name]]
    call[[1L]] <- generated
    eval(call, envir = parent.frame())
  }
  formals(wrapper) <- formals(generated)
  wrapper
})
