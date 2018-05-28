#' getMethodHelp
#'
#' Extracts help-files from a given package
#'
#' @param packageName Name of the package to extract the help files from
#' @param methodName Name of the method of which the help file should be read
#' @param getAll True if instead of a single one, all help-files of a given package should be read. Default = False
#'
#' @return An Rd Object with the package help file or a list of all Rd-Objects for a given package
#'
#' @examples
#' spotGUI:::getMethodHelp("SPOT","buildKriging")
#' spotGUI:::getMethodHelp("SPOT",NULL, TRUE)
#'
#' @keywords internal
getMethodHelp <- function(packageName, methodName, getAll = FALSE){
    if(is.element(packageName, installed.packages()[,1])){
        db <- Rd_db(packageName)
        if(!getAll){
            return(db[[paste0(methodName,".Rd")]])
        }else{
            return(db)
        }
    }
    return(NULL)
}

#' getMethodHelpArgument
#'
#' Extracts the "Arguments" section from a help file
#'
#' @param packageName Name of the package to extract the help file from
#' @param methodName Name of the method of which the help file should be read
#'
#' @return An Rd Object with the "Arguments" section of a given help file. If no "Arguments"
#' Section exists, this returns NULL
#'
#' @examples
#' spotGUI:::getMethodHelpArgument("SPOT","buildKriging")
#'
#' @keywords internal
getMethodHelpArgument <- function(packageName, methodName){
    rd <- getMethodHelp(packageName, methodName)
    if(!is.null(rd)){
        for(i in 1:length(rd)){
            if(attr(rd[[i]],"Rd_tag") == "\\arguments"){
                return(rd[[i]])
            }
        }
    }
    return(NULL)
}

#' getMethodHelpControlList
#'
#' Extracts the description of a control list of a specified method in the SPOT package.
#'
#' @param methodName Name of the method of which the help file should be read
#'
#' @return An Rd Object with the "control" section of a given help file. If no control list is found this returns NULL
#'
#' @examples
#' spotGUI:::getMethodHelpControlList("buildKriging")
#'
#' @keywords internal
getMethodHelpControlList <- function(methodName){
    packageName <- "SPOT"
    rd <- getMethodHelpArgument(packageName, methodName)
    if(!is.null(rd)){
        for(i in 1:length(rd)){
            if(rd[[i]][[1]][[1]][[1]] == "control"){
                return(rd[[i]])
            }
        }
    }
    return(NULL)
}

#' getAllHelpControlListParams
#'
#' Extracts all control list parameters of a given method in the SPOT-package by parsing it's help file.
#'
#' @param methodName Name of the method of which the help file should be read
#' @param directSpotControl Default NULL to read any other control list. If this isn't NULL, then methodName
#' is ignored and this parameter is handled as an rd list instead
#'
#' @return A list of all parameters in the control list
#'
#' @examples
#' spotGUI:::getAllHelpControlListParams("buildKriging")
#'
#' @keywords internal
getAllHelpControlListParams <- function(methodName, directSpotControl = NULL){
    if(is.null(directSpotControl)){
        ctrl <- getMethodHelpControlList(methodName)[[2]]
    }else{
        ctrl <- directSpotControl[[8]]
    }

    #This list should be somewhat controlled and or updated with the spot helpfiles
    hardCodedExceptionStrings <- c("control","optim")

    if(is.null(ctrl)){
        return(NULL)
    }

    hasEmptyLists <- F
    for(e in ctrl){
        if(length(e) == 0){
            hasEmptyLists <- T
            break
        }
    }

    if(!hasEmptyLists){
        operationApplied <- T
        while(operationApplied & length(ctrl) > 0){
            operationApplied <- F

            #Search for 1 element character lists (these can be removed)
            removes <- NULL
            for(i in 1:length(ctrl)){
                if(typeof(ctrl[[i]]) == "character"){
                    removes <- c(removes,i)
                    operationApplied <- T
                }
            }
            for(e in rev(removes)){
                ctrl[[e]] <- NULL
            }

            #if there is a list that only contains character control then remove it
            if(length(ctrl) > 0){
                for(i in length(ctrl):1){
                    if(typeof(ctrl[[i]]) == "list" & length(ctrl[[i]])==1){
                        if(ctrl[[i]][[1]] %in% hardCodedExceptionStrings){
                            ctrl[[i]] <- NULL
                        }
                    }
                }

                #if just a single list exists, it can be opened
                if(length(ctrl) == 1 & typeof(ctrl[[1]]) == "list")
                {
                    ctrl <- ctrl[[1]]
                    operationApplied <- T
                }
            }
        }
        paramList <- list()
        if(length(ctrl) > 0){
            for(i in 1:length(ctrl)){
                elements <- unlist(ctrl[[i]])
                paramList[[elements[1]]] <- paste(elements[2:length(elements)], collapse="")
            }
        }
    }else{
        #Erstes Element bei manuellen spot listen kann immer gelöscht werden
        ctrl[[1]] <- NULL
        for(i in length(ctrl):1){
            if(length(ctrl[[i]]) == 0){
                ctrl[[i]] <- NULL
            }
        }

        ctrl <- unlist(ctrl)
        endLines <- which(ctrl == "\n" | ctrl == " \n")
        entries <- (1:length(ctrl))[-endLines]

        currentEle <- NULL
        paramList <- list()
        isFirstEntry <- !c(F,entries[1:length(entries)-1] == entries[2:length(entries)]-1)
        for(i in 1:length(entries)){
            if(isFirstEntry[i]){
                name <- ctrl[entries[i]]
                i <- i+1
                while(!isFirstEntry[i] & i <= length(entries)){
                    paramList[[name]] <- paste(paramList[[name]], ctrl[entries[i]], collapse="")
                    i <- i+1
                }
            }
        }
    }
    return(paramList)
}

#' getHelpSpotControlParameters
#'
#' Extracts all Parameters specified in spotControl
#'
#' @return A list of all parameters in the control list
#'
#' @examples
#' spotGUI:::getMethodHelpArgument("SPOT","buildKriging")
#'
#' @keywords internal
getHelpSpotControlParameters <- function(){
    rd <- getMethodHelp("SPOT", "spotControl")
    if(!is.null(rd)){
        for(i in 1:length(rd)){
            if(attr(rd[[i]],"Rd_tag") == "\\description"){
                return(getAllHelpControlListParams(NULL,rd[[i]]))
            }
        }
    }
    return(NULL)
}
