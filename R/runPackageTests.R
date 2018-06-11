#' runTests
#'
#' Runs package tests via devtools::test
#' @keywords internal
runTests <- function(){
    requireNamespace("testthat")
    requireNamespace("shinytest")
    requireNamespace("tools")
    requireNamespace("devtools")

    devtools::test(reporter=testthat::SummaryReporter)
}
