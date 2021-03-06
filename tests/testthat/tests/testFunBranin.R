app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("testFunBranin")

app$setInputs(objectiveFunction = "funBranin")
app$setInputs(tabs = "spotConfig")
try(app$setInputs(tabs = "runMode"), silent = T)
app$setInputs(runCreateDOE = "click")
app$setInputs(evaluateData = "click")
app$snapshot(list(output = "resultTable"))
