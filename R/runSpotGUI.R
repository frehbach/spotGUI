#' runSpotGUI
#'
#' Run the starting command of the SPOT-GUI. Opens the graphical shiny application through which the user
#' can acess the SPO Toolbox.
#'
#' @usage
#' runSpotGUI()
#'
#' @import shiny
#' @import shinydashboard
#' @import SPOT
#' @import gridExtra
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @import rhandsontable
#' @import XML
#' @import rclipboard
#' @import plotly
#' @import shinyBS
#' @importFrom tools Rd_db
#' @importFrom httpuv service
#' @importFrom utils capture.output
#' @importFrom utils installed.packages
#' @importFrom methods getPackageName
#'
#' @export
runSpotGUI <- function() {
    appDir <- system.file(package = "spotGUI")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `spotGUI`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
