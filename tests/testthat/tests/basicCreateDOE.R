app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("basicCreateDOE")

app$setInputs(tabs = "spotConfig")
try(app$setInputs(tabs = "runMode"), silent = T)
try(app$setInputs(runCreateDOE = "click"), silent = T)
try(app$setInputs(evaluateData = "click"), silent = T)
app$snapshot(list(output = "resultTable"))
