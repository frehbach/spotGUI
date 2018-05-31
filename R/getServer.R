spotGuiEnv <- new.env()

#' Generate Server Part of SPOT-GUI
#'
#' Generates the server part of the SPOT-GUI.
#' This method is used internally in the starting process of the GUI.
#' Manual use of this function is not advised.
#'
#' @param input shiny UI-input
#' @param output shiny UI-output
#' @param session shiny UI-session
#' @export
getServer <- function(input, output, session) {
    initVariables <- function(init){
        doeWasLast(T)
        spotResult(NULL)
        tableChangedByScript(F)
        rLogOutput(rLogResetMessage())

        if(init){
            shinyjs::disable("resetData")
            shinyjs::disable("evaluateData")
            shinyjs::disable("proposeNewPoint")
            shinyjs::enable("runCreateDOE")
            shinyjs::enable("runSpotIter")
        }
    }

    #initialization
    doeWasLast <- reactiveVal()
    tableChangedByScript <- reactiveVal()
    spotResult <- reactiveVal()
    rLogOutput <- reactiveVal()
    assign("inputDimensions", NULL, envir=spotGuiEnv)

    maxInputDimension <- reactiveVal(0)

    configInitiated <- reactiveVal(FALSE)

    initVariables(FALSE)

    #This list is used to reset all data if changes are made to the fitness function definition
    listObjectiveFunctionElements <- reactive(list(
        input$objectiveFunction
    ))

    ## En-/Disables
    shinyjs::disable("evaluateData")
    shinyjs::disable("resetData")
    shinyjs::disable("proposeNewPoint")

    shinyjs::disable("runCreateDOE")
    shinyjs::disable("runSpotIter")
    shinyjs::disable("interruptSpot")

    ## Events
    observeEvent(input$runCreateDOE,{
        if(is.null(input[["xml_designLHDcontrolreplicates"]])){
            showModal(modalDialog(title="Load Error",
                                  "Spot Config was not fully loaded, please revisit Spot Config tab"))
            return()
        }
        if(!checkInputCorrectness(input)){
            return()
        }
        if(getNDim(input) == 0){
            showModal(modalDialog(title="Configuration Error","You have to specify at least one
                                  input dimension before you can do this!"
                                  ,footer=NULL,easyClose=T))
            return()
        }
        if(!input$rLogMode){
            shinyjs::disable("runCreateDOE")
            shinyjs::disable("runSpotIter")
            tryCatch(expr = {
                spotResult(createDOE(input, spotResult()))}
                , error = function(cond) {
                    showModal(modalDialog(title="Configuration Error",HTML(paste("There seems to be an error in your configuration.<br>
                                                                                 SPOT was not able to run.<br>
                                                                                 Please check for typos/misconfigurations
                                                                                 in the SPOT Config Tab<br><br>Original spot error was:<br>",cond))
                                          ,footer=NULL,easyClose=T))
                    shinyjs::enable("runCreateDOE")
                    shinyjs::enable("runSpotIter")
                    return()
                })
            configInitiated(TRUE)
            shinyjs::enable("evaluateData")
            shinyjs::enable("resetData")
            doeWasLast(T)
        }else{
            showModal(modalDialog(title="Added Log Entry","Your action was logged into the export menu."
                                  ,footer=NULL,easyClose=T))
        }
        rLogOutput(paste0(rLogOutput(),generateTextDOECreation(input, spotResult())))
    })

    observeEvent(input$evaluateData,{
        req(spotResult())

        if(!checkInputCorrectness(input)){
            return()
        }

        localResult <- spotResult()
        x <- localResult$x
        y <- localResult$y

        if(input$objectiveFunction == "mInput"){
            if(any(is.na(y)) | nrow(x) > length(y)){
                showModal(modalDialog(title="Missing Data",
                                      "Please fill the result column before fitting a model!"
                                      ,footer=NULL,easyClose=T))
                return()
            }
        }else{
            if(!input$rLogMode){
                tryCatch(expr = {
                    fun <- getObjectiveFunction(input)
                    localResult$y <- evaluateMissingCandidateSolutions(x,y,fun)
                    localResult$modelFit <- buildModel(input,localResult)}
                    , error = function(cond) {
                        showModal(modalDialog(title="Configuration Error",
                                              HTML(paste("There seems to be an error in your configuration.<br>
                                                         There might be an issue in the configuration of your objective function<br>
                                                         or there exists a wrong entry in the data table.<br>
                                                         Please check for typos/misconfigurations
                                                         <br><br>Original error message was:<br>",cond))
                                              ,footer=NULL,easyClose=T))
                        return()
                    })
            }
        }
        rLogOutput(paste0(rLogOutput(),generateTextEvaluation(input)))
        if(!input$rLogMode){
            spotResult(localResult)
            shinyjs::enable("runCreateDOE")
            shinyjs::enable("runSpotIter")
            shinyjs::enable("resetData")
            shinyjs::enable("proposeNewPoint")
        }else{
            showModal(modalDialog(title="Added Log Entry","Your action was logged into the export menu."
                                  ,footer=NULL,easyClose=T))
        }
    })

    observeEvent(input$runSpotIter, {
        if(is.null(input[["xml_designLHDcontrolreplicates"]])){
            showModal(modalDialog(title="Load Error",
                                  "Spot Config was not fully loaded, please revisit Spot Config tab"))
            return()
        }
        if(input$objectiveFunction == "mInput"){
            showModal(modalDialog(title="Manual Mode",
                                  "Running SPOT automatically is not possible in Manual Mode.
                                  Please configure an objective function in the objective function menu."
                                  ,footer=NULL,easyClose=T))
            return()
        }
        if(!checkInputCorrectness(input)){
            return()
        }
        if(getNDim(input) == 0){
            showModal(modalDialog(title="Configuration Error","You have to specify at least one
                                  input dimension before you can do this!"
                                  ,footer=NULL,easyClose=T))
            return()
        }
        rLogOutput(paste0(rLogOutput(),generateTextSpotCall(input, spotResult())))
        if(!input$rLogMode){
            shinyjs::disable("runSpotIter")
            shinyjs::enable("interruptSpot")
            session <- shiny::getDefaultReactiveDomain()
            localSpotResult <- spotResult()
            tryCatch(expr = {
                ctrl <- getSpotControl(input)
                funEvals <- ctrl$funEvals
                bounds <- getBounds(input)
                while(T){
                    if(isTRUE(session$input$spotInterrupted)){
                        break
                    }
                    if(!is.null(localSpotResult)){
                        if(nrow(localSpotResult$x) >= funEvals){
                            break
                        }
                    }
                    localSpotResult <- doSpotIter(input,localSpotResult,ctrl,bounds)
                    httpuv::service()
                }
            }, error = function(cond) {
                showModal(modalDialog(title="Configuration Error",HTML(paste("There seems to be an error in your configuration.<br>
                                                                             SPOT was not able to run.<br>
                                                                             Please check for typos/misconfigurations
                                                                             in the SPOT Config Tab<br><br>Original spot error was:<br>",cond))
                                      ,footer=NULL,easyClose=T))
                shinyjs::enable("runSpotIter")
                return()
            })
            spotResult(localSpotResult)
            shinyjs::disable("interruptSpot")
            shinyjs::enable("runSpotIter")
            shinyjs::enable("resetData")
            shinyjs::enable("evaluateData")
            shinyjs::enable("proposeNewPoint")
            doeWasLast(F)
        }else{
            showModal(modalDialog(title="Added Log Entry","Your action was logged into the export menu."
                                  ,footer=NULL,easyClose=T))
        }
    })

    observeEvent(input$resetData,{
        rLogOutput(paste0(rLogOutput(),rLogResetMessage()))
        initVariables(configInitiated())
    })

    observeEvent(input$resultTable,{
        if(!tableChangedByScript()){
            newData <- as.matrix(hot_to_r(input$resultTable))

            newResult <- spotResult()
            newResult$x <- newData[,1:getNDim(input)]
            newResult$y <- newData[,ncol(newData)]

            spotResult(newResult)
        }

        tableChangedByScript(F)
    })

    ## Objective Function Settings changes lead to calculation reset
    observeEvent(listObjectiveFunctionElements(),{
        initVariables(configInitiated())
    })

    observeEvent(input$proposeNewPoint,{
        req(spotResult())

        if(!checkInputCorrectness(input)){
            return()
        }

        if(any(is.na(spotResult()$y)) | nrow(spotResult()$x) > length(spotResult()$y)){
            showModal(modalDialog(title="Missing Data",
                                  "Please fill the result column before generating new design points!"
                                  ,footer=NULL,easyClose=T))
            return()
        }
        if(!input$rLogMode){
            shinyjs::disable("runSpotIter")
            tryCatch(expr = {
                localResult <- spotResult()
                model <- localResult$modelFit
                optimizer <- get(getOptimizer(input))
                optimizerControl <- getControlList(input,"optimizer")
                fun <- evaluateModel(model)
                bounds <- getBounds(input)
                bestPoint <- optimizer(fun = fun, lower = bounds[[1]], upper = bounds[[2]],control = optimizerControl)
                localResult$x <- rbind(localResult$x,bestPoint$xbest)
                spotResult(localResult)}
                , error = function(cond) {
                    showModal(modalDialog(title="Configuration Error",HTML(paste("There seems to be an error in your configuration.<br>
                                                                                 SPOT was not able to run.<br>
                                                                                 Please check for typos/misconfigurations
                                                                                 in the SPOT Config Tab<br><br>Original spot error was:<br>",cond))
                                          ,footer=NULL,easyClose=T))
                    return()
                })
        }else{
            showModal(modalDialog(title="Added Log Entry","Your action was logged into the export menu."
                                  ,footer=NULL,easyClose=T))
        }
        rLogOutput(paste0(rLogOutput(),generateTextNewPoint(input)))
    })

    observeEvent(input$addDimension,{
        maxInputDimension(maxInputDimension() + 1)
        insertUI(
            selector = '#objectiveFunctionInputParameters',
            where = "beforeEnd",
            ui = createDimensionElement(input,
                                        NULL,
                                        maxInputDimension(),
                                        initVariables,
                                        configInitiated))
        initVariables(configInitiated())
    })

    ############## test

    output$clipButton <- renderUI({
        if (requireNamespace("rclipboard", quietly = TRUE)) {
            rclipButton("clipbtn", "Copy to Clipboard", rLogOutput(), icon("clipboard"))
        } else {
            showModal(modalDialog(title="Package Missing","This feature requires the rclipboard package."
                                  ,footer=NULL,easyClose=T))
        }
    })

    output$variableSelectors <- renderUI({
        req(spotResult()$y)
        getPlotSelectorButtonList(input,"AUTO")
    })

    output$slidersResult <- renderUI({
        req(spotResult()$y)
        req(input$selectorXAUTO)

        do.call(tagList, getPlotSliderList(input,"AUTO"))
    })

    output$resultModelPlot <- renderPlotly({
        req(spotResult()$y)

        validate(
            need(getNDim(input) >= 2, "Plots are currently only possible with >= 2 Dimensions")
        )

        # Require that at least one slider exists if input dimensions is larger 2
        if(getNDim(input)> 2){
            val <- getNotSelectedVariables(input,"AUTO")[1]
            req(input[[paste("sliderAUTOx",val,sep="")]])
        }

        model <- spotResult()$modelFit
        if(is.null(model)){
            return()
        }
        if(getNDim(input) == 2){
            #plotModel(model,type ="filled.contour")
            p <- plotModel(model,type ="persp3d")
            p$elementId <- NULL
            p
        }else{
            variables = 1:getNDim(input)
            variables = variables[-getNotSelectedVariables(input, "AUTO")]
            sliderInputs <- getPlotSliderValues(input,"AUTO")
            #plotModel(model,
             #         which = variables, constant = sliderInputs,type ="filled.contour")
            p <- plotModel(model,
                      which = variables, constant = sliderInputs,type ="persp3d")
            p$elementId <- NULL
            p
        }
    })

    output$resultTable <- renderRHandsontable({
        req(spotResult())
        tableChangedByScript(T)
        x <- spotResult()$x
        y <- spotResult()$y

        if(length(dim(x)) > 1){
            len <- nrow(x)
        }else{
            len <- length(x)
        }
        if(len == length(y)){
            data <- cbind(x,y)
        }else{
            data <- cbind(x,c(y,rep(NA,nrow(x)-length(y))))
        }
        colNames <- NULL
        for(i in 1:(ncol(data)-1)){
            colNames <- c(colNames, paste("x",i,sep=""))
        }
        colNames <- c(colNames, "results")
        df <- data.frame(data)
        names(df) <- colNames

        rhandsontable(round(df,3), stretchH = "all")
    })

    output$bestFound <- renderUI({
        req(spotResult()$y)
        req(nrow(spotResult()$x)==length(spotResult()$y))
        getTextoutputBestSolution(input,cbind(spotResult()$x,spotResult()$y))
    })

    output$rLog <- renderText({
        if(!input$shortenLog){
            rLogOutput()
        }else{
            outText <- rLogOutput()
            if(grepl("## Data Reset\n",outText)){
                splits <- strsplit(outText,"## Data Reset\n", fixed=T)[[1]]
                outText <- splits[[length(splits)]]
            }
            outText
        }
    })

    output$fieldOptimizerSelector <- renderUI({
        getUiSelectorXML("optimizer",input)
    })

    output$optimizerUI <- renderUI({
        req(input$optimizerSelector)
        getUiXML("optimizer",input)
    })

    output$fieldModelSelector <- renderUI({
        getUiSelectorXML("model",input)
    })

    output$modelUI <- renderUI({
        req(input$modelSelector)
        getUiXML("model",input)
    })

    output$fieldDesignSelector <- renderUI({
        shinyjs::enable("runCreateDOE")
        shinyjs::enable("runSpotIter")
        getUiSelectorXML("design",input)
    })

    output$designUI <- renderUI({
        req(input$designSelector)
        getUiXML("design",input)
    })

    output$spotConfigUI <- renderUI({
        getUiXML("general",input, selectedInput = "general"
                 , selectedElement = getSelectedElementList("general", "general", input))
    })

    output$objectiveFunctionSelector <- renderUI({
        getObjectiveFunctionSelectorXML(input)
    })

    output$objectiveFunctionInputParameters <- renderUI({
        req(input$objectiveFunction)
        req(input$smoofFunctionSelector)
        isolate(
            maxInputDimension(maxInputDimension() +
                                  generateInputUI(input, initVariables,configInitiated)))
        div()
    })

    output$plotlyModelPlot <- renderPlotly({
        req(spotResult()$y)

        validate(
            need(getNDim(input) >= 2, "Plots are currently only possible with >= 2 Dimensions")
        )

        # Require that at least one slider exists if input dimensions is larger 2
        if(getNDim(input)> 2){
            val <- getNotSelectedVariables(input,"AUTO")[1]
            req(input[[paste("sliderAUTOx",val,sep="")]])
        }

        model <- spotResult()$modelFit
        if(is.null(model)){
            return()
        }
        if(getNDim(input) == 2){
            plotlyModelPlot(model,input)
        }else{
            variables <- 1:getNDim(input)
            notSelectedVars <- getNotSelectedVariables(input, "AUTO")
            variables = variables[-notSelectedVars]
            sliderInputs <- getPlotSliderValues(input,"AUTO")
            constants <- rep(x = 0,times = getNDim(input))
            constants[notSelectedVars] <- sliderInputs
            plotlyModelPlot(model,input,
                      which = variables, constant = sliderInputs)
        }
    })
}
