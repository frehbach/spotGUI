#' List Items per row in R-Code Exports
#'
#' Specify how many items of a list a printed per line
#' @keywords internal
LIST_ITEMS_PER_ROW <- 1

#' Generate the Initial Code Block for the R-Export
#'
#' @return String, rExport initial code
#'
#' @keywords internal
rLogResetMessage <- function(){
    message <- paste(
        " ",
        "## -----------------------------------------------",
        "## -----------------------------------------------",
        "## Data Reset",
        "## -----------------------------------------------",
        "## -----------------------------------------------",
        " ",
        "library(SPOT)",
        "library(spotGUI)\n",
        "spotData <- NULL\n\n",
        sep="\n"
    )
}


#' Textoutput Field 'Best Solution'
#'
#' Generates the outputField to show the best till then found candidate solution
#'
#' @param input shiny input
#' @param data data.frame with all candidate solutions
#'
#' @return ouputField
getTextoutputBestSolution <- function(input, data){
    #get data index with best objective function value
    ind <- which.min(data[,ncol(data)])

    #Generate Text for each input parameter with its respective value
    xTextOut <- NULL
    for(i in 1:(ncol(data)-1)){
        xTextOut <- paste(xTextOut,"X",i,": ",round(data[ind,i],2), "    ",sep = "")
    }

    textOut <- wellPanel(
        h5("Best found candidate solution so far:", align="center"),
        h5(xTextOut, align="center"),
        h5(paste("With objective function value:",round(data[ind,ncol(data)],2)), align="center")
    )
    return(textOut)
}

generateTextSpotCall <- function(input, spotData){
    bounds <- getBounds(input)
    spotControl <- getSpotControl(input,asText = T)


    if(is.null(spotData$x)){
        spotText <- "spotData <- spot("
    }else{
        spotText <- "spotData <- spotLoop(x = spotData$x, y = spotData$y, "
    }
    fun <- getObjectiveFunction(input,asText=T)
    spotText <- paste0("#SPOT Call\n",spotText, "\n\t\tfun = " ,fun,", ")
    spotText <- paste0(spotText,"lower = ",paste(bounds)[1],", upper = ",
                      paste(bounds)[2], ",\n\t\tcontrol = ",
                      printList(spotControl), ")\n\n")
    spotText
}

printList <- function(listToPrint){
    listNames <- names(listToPrint)
    text <- "list("
    for(i in 1:length(listToPrint)){
        newLine <- ""
        if(((i%%LIST_ITEMS_PER_ROW) == 0) & (i != length(listToPrint))){
            newLine <- "\n\t\t\t"
        }
        listItem <- listToPrint[[i]]
        if(length(listItem) > 1 & !(typeof(listItem) == "list")){
            listItem <- capture.output(cat(paste0(list(listItem))))
        }else if(typeof(listItem) == "closure"){
            listItem <- as.character(substitute(listItem))
        }else if(typeof(listItem) == "list"){
            listItem <- printList(listItem)
        }else{
            listItem <- as.character(listItem)
        }

        if(!listNames[i] == ""){
            text <- paste0(text, listNames[i], " = ", listItem, ", ",newLine, collapse = ";")
        }else{
            text <- paste0(text, listItem, ", ",newLine, collapse = ";")
        }

    }
    return(paste0(substr(text, 1, nchar(text)-2),")"))
}

generateTextDOECreation <- function(input, spotData){
    bounds <- getBounds(input)
    gen <- getDesignGenerator(input)
    con <- getControlList(input,"design")

    if(!is.null(spotData$x)){
        dataText <- "x = spotData$x"
    }else{
        dataText <- "x = NULL"
    }
    paste0("#Generating DOE\n",
        "spotData$x <- ",
        gen,
        "(", dataText, ", lower = ", paste(bounds)[1],", upper = ",  paste(bounds)[2], ",\n\t\t\tcontrol = ",
        printList(con), ")\n\n")
}

generateTextEvaluation <- function(input){
    fun <- getObjectiveFunction(input,asText = T)
    text <- "#Evaluating Candidate Solutions\n"
    text <- paste0(text,"spotData$y <- evaluateMissingCandidateSolutions(currX = spotData$x, currY = spotData$y, fun = ",fun,")\n")

    modelFun <- getModel(input)
    paste0(text, "spotData$modelFit <- ", modelFun, "(as.matrix(spotData$x),as.matrix(spotData$y))", "\n\n")
}

generateTextNewPoint <- function(input){
    optimizer <- getOptimizer(input)
    fun <- getObjectiveFunction(input,asText = T)
    bounds <- getBounds(input)
    optimizerControl <- getControlList(input,"optimizer")

    text <- "#Generating Single Next Point on Surrogate Model\n"
    text <- paste0(text,"evaluateModelFun <- evaluateModel(spotData$modelFit)\n")
    text <- paste0(text,"spotData$x <- rbind(spotData$x," ,
                   optimizer,"(fun = evaluateModelFun, lower = ", paste(bounds)[1],"\n, upper = ",  paste(bounds)[2],
                   ", control = ",
                   printList(optimizerControl),
                   ")$xbest)\n\n")
    text
}
