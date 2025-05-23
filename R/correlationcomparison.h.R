
# This file is automatically generated, you probably don't want to edit this

correlationComparisonOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "correlationComparisonOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            correlation_type = "independent",
            independent_n1 = 20,
            independent_n2 = 20,
            independent_r1 = 0,
            independent_r2 = 0,
            dependent_n = 20,
            dependent_r12 = 0,
            dependent_r13 = 0,
            dependent_r23 = 0, ...) {

            super$initialize(
                package="BSS",
                name="correlationComparison",
                requiresData=FALSE,
                ...)

            private$..correlation_type <- jmvcore::OptionList$new(
                "correlation_type",
                correlation_type,
                options=list(
                    "independent",
                    "dependent"),
                default="independent")
            private$..independent_n1 <- jmvcore::OptionInteger$new(
                "independent_n1",
                independent_n1,
                default=20)
            private$..independent_n2 <- jmvcore::OptionInteger$new(
                "independent_n2",
                independent_n2,
                default=20)
            private$..independent_r1 <- jmvcore::OptionNumber$new(
                "independent_r1",
                independent_r1,
                default=0)
            private$..independent_r2 <- jmvcore::OptionNumber$new(
                "independent_r2",
                independent_r2,
                default=0)
            private$..dependent_n <- jmvcore::OptionInteger$new(
                "dependent_n",
                dependent_n,
                default=20)
            private$..dependent_r12 <- jmvcore::OptionNumber$new(
                "dependent_r12",
                dependent_r12,
                default=0)
            private$..dependent_r13 <- jmvcore::OptionNumber$new(
                "dependent_r13",
                dependent_r13,
                default=0)
            private$..dependent_r23 <- jmvcore::OptionNumber$new(
                "dependent_r23",
                dependent_r23,
                default=0)

            self$.addOption(private$..correlation_type)
            self$.addOption(private$..independent_n1)
            self$.addOption(private$..independent_n2)
            self$.addOption(private$..independent_r1)
            self$.addOption(private$..independent_r2)
            self$.addOption(private$..dependent_n)
            self$.addOption(private$..dependent_r12)
            self$.addOption(private$..dependent_r13)
            self$.addOption(private$..dependent_r23)
        }),
    active = list(
        correlation_type = function() private$..correlation_type$value,
        independent_n1 = function() private$..independent_n1$value,
        independent_n2 = function() private$..independent_n2$value,
        independent_r1 = function() private$..independent_r1$value,
        independent_r2 = function() private$..independent_r2$value,
        dependent_n = function() private$..dependent_n$value,
        dependent_r12 = function() private$..dependent_r12$value,
        dependent_r13 = function() private$..dependent_r13$value,
        dependent_r23 = function() private$..dependent_r23$value),
    private = list(
        ..correlation_type = NA,
        ..independent_n1 = NA,
        ..independent_n2 = NA,
        ..independent_r1 = NA,
        ..independent_r2 = NA,
        ..dependent_n = NA,
        ..dependent_r12 = NA,
        ..dependent_r13 = NA,
        ..dependent_r23 = NA)
)

correlationComparisonResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "correlationComparisonResults",
    inherit = jmvcore::Group,
    active = list(
        comparisonTable = function() private$.items[["comparisonTable"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Comparison of Correlations")
            self$add(jmvcore::Table$new(
                options=options,
                name="comparisonTable",
                title="Correlation Comparison Results",
                rows=1,
                columns=list(
                    list(
                        `name`="comparisonType", 
                        `title`="Type", 
                        `type`="text"),
                    list(
                        `name`="n_group1", 
                        `title`="N (Group 1)", 
                        `type`="integer", 
                        `visible`="(correlation_type:independent)"),
                    list(
                        `name`="n_group2", 
                        `title`="N (Group 2)", 
                        `type`="integer", 
                        `visible`="(correlation_type:independent)"),
                    list(
                        `name`="r_group1", 
                        `title`="r (Group 1)", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(correlation_type:independent)"),
                    list(
                        `name`="r_group2", 
                        `title`="r (Group 2)", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(correlation_type:independent)"),
                    list(
                        `name`="n_dep", 
                        `title`="N", 
                        `type`="integer", 
                        `visible`="(correlation_type:dependent)"),
                    list(
                        `name`="r12_dep", 
                        `title`="r12", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(correlation_type:dependent)"),
                    list(
                        `name`="r13_dep", 
                        `title`="r13", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(correlation_type:dependent)"),
                    list(
                        `name`="r23_dep", 
                        `title`="r23", 
                        `type`="number", 
                        `format`="zto", 
                        `visible`="(correlation_type:dependent)"),
                    list(
                        `name`="statistic", 
                        `title`="Statistic", 
                        `type`="number", 
                        `format`="zto"),
                    list(
                        `name`="p_value", 
                        `title`="p", 
                        `type`="number", 
                        `format`="zto,pvalue"))))}))

correlationComparisonBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "correlationComparisonBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "BSS",
                name = "correlationComparison",
                version = c(0,0,1),
                options = options,
                results = correlationComparisonResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'na')
        }))

#' Compare correlations
#'
#' 
#' @param correlation_type .
#' @param independent_n1 .
#' @param independent_n2 .
#' @param independent_r1 .
#' @param independent_r2 .
#' @param dependent_n .
#' @param dependent_r12 .
#' @param dependent_r13 .
#' @param dependent_r23 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$comparisonTable} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$comparisonTable$asDF}
#'
#' \code{as.data.frame(results$comparisonTable)}
#'
#' @export
correlationComparison <- function(
    correlation_type = "independent",
    independent_n1 = 20,
    independent_n2 = 20,
    independent_r1 = 0,
    independent_r2 = 0,
    dependent_n = 20,
    dependent_r12 = 0,
    dependent_r13 = 0,
    dependent_r23 = 0) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("correlationComparison requires jmvcore to be installed (restart may be required)")


    options <- correlationComparisonOptions$new(
        correlation_type = correlation_type,
        independent_n1 = independent_n1,
        independent_n2 = independent_n2,
        independent_r1 = independent_r1,
        independent_r2 = independent_r2,
        dependent_n = dependent_n,
        dependent_r12 = dependent_r12,
        dependent_r13 = dependent_r13,
        dependent_r23 = dependent_r23)

    analysis <- correlationComparisonClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

