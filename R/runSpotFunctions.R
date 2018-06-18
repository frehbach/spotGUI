# Takes the user input and if available the last spot iteration (otherwise NULL)
# Performs the next spot iteration dependent on the user settings
doSpotIter <- function(input,lastSpotIter,ctrl,bounds, doAll = F){
    #check if already data exists from a previous spot iteration
    if(!is.null(lastSpotIter)){
        lastX <- as.matrix(lastSpotIter$x)
        lastY <- as.matrix(lastSpotIter$y)
    }else{
        lastX <- NULL
        lastY <- NULL
    }

    controlList <- ctrl

    resultList <- NULL
    resultList$x <- lastX
    resultList$y <- lastY

    #If this is the first run of spot and there is no previous data then the DOE should be build
    if(length(lastX) == 0){
        designSize <- controlList$designControl$size
        localControlList <- controlList
        if(controlList$funEvals > designSize){
            localControlList$funEvals <- designSize + 1
        }else{
            localControlList$funEvals <- designSize
        }
        objFun <- getObjectiveFunction(input)
        resultList <- spot(lower = bounds[[1]], upper = bounds[[2]],
                           fun = objFun
                           ,control = localControlList)
        return(resultList)
    }

    #checkForInterruptSpotButton

    localControlList <- controlList
    if(!doAll){
        localControlList$funEvals <- nrow(resultList$x) + 1
    }
    resultList <- spotLoop(x = resultList$x, y = resultList$y, lower = bounds[[1]], upper = bounds[[2]],
                           fun = getObjectiveFunction(input)
                           ,control = localControlList)

    return(resultList)
}

#Build a surrogate model
buildModel <- function(input, data){
    #Get String-Name of Model. Get to transform to type closure
    modelFun <- get(getModel(input))

    #Build model on given data
    model <- modelFun(as.matrix(data$x),as.matrix(data$y))

    return(model)
}

# Create a DOE from scratch or expand existing DOE given by "lastDOE"
createDOE <- function(input, lastDOE = NULL){
    #Get Bounds of objective function
    bounds <- getBounds(input)

    previousDOE <- lastDOE$x

    #Get designGeneration control list
    control <- getControlList(input,"design")

    # Generate DOE
    lastDOE$x <- get(getDesignGenerator(input))(x = previousDOE,bounds[[1]],
                                                bounds[[2]], control = control)

    return(lastDOE)
}
