getAllSmoofFunctions <- function(){
    functionBlackList <- c("Shekel function","Hartmann")
    requireNamespace("smoof")
    smoofNames <- smoof::filterFunctionsByTags(tags = "single-objective")
    smoofNames <- smoofNames[-which(smoofNames %in% functionBlackList)]
    return(smoofNames)
}

getSmoofFunByName <- function(strName, input = NULL){
    #If input is not accessible return a 2-dimensional function
    if(is.null(input))
    {
        return(smoof::makeFunctionsByName(strName, dimensions = 2)[[1]])
    }else{
        smoof::makeFunctionsByName(strName, dimensions = getNDim(input))[[1]]
    }

}

getSmoofFunLowerBounds <- function(strName){
    fun <- getSmoofFunByName(strName)
    return(smoof::getLowerBoxConstraints(fun))
}

getSmoofFunUpperBounds <- function(strName){
    fun <- getSmoofFunByName(strName)
    return(smoof::getUpperBoxConstraints(fun))
}

checkSmoofFunScalibility <- function(strName,input){
    dims <- getNDim(input)
    fun <- smoof::makeFunctionsByName(strName, dimensions = dims)[[1]]
    return(dims == length(smoof::getLowerBoxConstraints(fun)))
}
