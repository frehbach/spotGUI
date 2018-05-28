app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("basicCreateDOE")

app$setInputs(tabs = "spotConfig")
app$setInputs(tabs = "runMode")
app$setInputs(runCreateDOE = "click")
app$setInputs(evaluateData = "click")
app$snapshot(list(output = "resultTable"))
