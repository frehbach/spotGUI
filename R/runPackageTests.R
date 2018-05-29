#' runTests
#'
#' Runs package tests including many GUI scenarios
#' @keywords internal
runTests <- function(){
    requireNamespace("testthat")
    requireNamespace("shinytest")
    requireNamespace("tools")
    requireNamespace("devtools")

    devtools::test(reporter=testthat::SummaryReporter)
}
