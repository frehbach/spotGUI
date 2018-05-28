
compareHelpFilesWithXML <- function(){
    xmlConfig <- spotGUI:::xmlGetAllConfiguredControlElements()
    configComplete = T
    namesConfig <- names(xmlConfig)
    for(i in 1:length(namesConfig)){
        inXML <- xmlConfig[[i]]
        inHelp <- names(spotGUI:::getAllHelpControlListParams(namesConfig[i]))
        if("types" %in% inHelp){
            inXML <- c(inXML,"types")
        }
        if(!base::setequal(inXML,inHelp) & !(length(inHelp) == 0)){
            configComplete <- F
            cat(namesConfig[i],"\n###\n")
            cat(paste(base::setdiff(inXML,inHelp),base::setdiff(inHelp,inXML),"\n\n"))
        }
        if(length(inHelp) == 0){
            cat(namesConfig[i],"\n###\n")
            cat("is Empty!","\n\n")
        }
    }
    return(configComplete)
}

#test_that("XML File is complete compared with help files",{
    #expect_equal(compareHelpFilesWithXML(),T);
#})
