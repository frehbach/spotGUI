#' Runs on package attach
#'
#' The package startup message is defined here. It shall give a clue on how to
#' start the spotGUI and which suggests might want to be installed.
#'
#' @keywords internal
.onAttach <- function(libname, pkgname){
    #Smoof depends on other packages so it has to be attached
    #in order to load its depends
    if(0 == length(grep(paste("^package:", "smoof", "$", sep=""), search()))){
        suppressMessages(attachNamespace("smoof"))
    }

    #Non Interactive sessions should not see a welcome message
    if (!interactive()) return()

    packageStartupMessage("*** Welcome to the spotGUI package ***\n\n
                          There is only one command that you will need to use the spotGUI:\n
                          runSpotGUI()\n\n
                          ")
}
