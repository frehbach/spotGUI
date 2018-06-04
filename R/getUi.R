#' Define UI of SPOT-GUI
#'
#' Generates the UI part of the SPOT-GUI.
#' This method is used internally in the starting process of the GUI.
#' Manual use of this function is not advised.
#'
#' @export
getUIPage <- function(){
    dashboardPage(
        # Theme Color Setting
        skin = "green",
        dashboardHeader(title = "SPOT-GUI"),

        # Creation of Main Tabs
        dashboardSidebar(
            sidebarMenu(id = "tabs",
                        menuItem("Objective function", tabName = "objectiveFunction"),
                        menuItem("Spot Config", tabName = "spotConfig"),
                        menuItem("Run Spot", tabName = "runMode"),
                        menuItem("Exports", tabName = "exports")
            )
        ),

        # Body Contents of each tab
        dashboardBody(
            # Use Shinyjs for locking and unlocking buttons
            shinyjs::useShinyjs(),
            rclipboardSetup(),

            tabItems(
                # Objective function configuration tab
                tabItem(tabName = "objectiveFunction",
                        fluidRow(
                            wellPanel(
                                uiOutput("objectiveFunctionSelector")
                            ),
                            h3("Dimensions: "),
                            uiOutput("objectiveFunctionInputParameters"),
                            actionButton(inputId = "addDimension",
                                         label = "",icon = icon("plus-circle"))
                        )
                ),

                # Configuration Tab for spot settings
                tabItem(tabName = "spotConfig",
                        fluidRow(
                            column(6,
                                   wellPanel(
                                       h4("General Settings"),
                                       uiOutput("spotConfigUI")
                                   ),
                                   wellPanel(
                                       h4("Optimizer"),
                                       uiOutput("fieldOptimizerSelector"),
                                       uiOutput("optimizerUI")
                                   )
                            ),
                            column(6,
                                   wellPanel(
                                       h4("Modelling"),
                                       uiOutput("fieldModelSelector"),
                                       uiOutput("modelUI")
                                   ),
                                   wellPanel(
                                       h4("Design Settings"),
                                       uiOutput("fieldDesignSelector"),
                                       uiOutput("designUI")
                                   )
                            )
                        )
                ),

                tabItem(tabName = "runMode",
                        fluidRow(
                            wellPanel(
                                actionButton("runCreateDOE", "Create DOE"),
                                actionButton("evaluateData","Evaluate Data/Update Model"),
                                actionButton("runSpotIter", "Run SPOT", onclick="Shiny.onInputChange('spotInterrupted',false)"),
                                actionButton("proposeNewPoint", "Propose next Point"),
                                actionButton("resetData", "Reset"),
                                actionButton("interruptSpot","Interrupt Spot", onclick="Shiny.onInputChange('spotInterrupted',true)"),
                                checkboxInput("rLogMode",label = "Log Only")
                            )
                        ),
                        fluidRow(
                            uiOutput("bestFound"),
                            column(6,
                                   rHandsontableOutput('resultTable')
                            ),
                            column(6,
                                   uiOutput("slidersResult"),
                                   uiOutput("variableSelectors"),
                                   plotlyOutput("resultModelPlot")
                            )
                        )
                ),

                tabItem(tabName = "exports",
                        shiny::tags$head(shiny::tags$style(shiny::HTML(
                            "#rLog {height: 200px; overflow: auto; }"
                        ))),
                        wellPanel(
                            h4("R Recreation Log"),
                            verbatimTextOutput("rLog"),
                            checkboxInput("shortenLog", "Only Show Entrys after last data reset"),
                            uiOutput("clipButton")
                        )
                )
            )
        )
    )
}
