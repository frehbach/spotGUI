app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("testFunBranin")

app$setInputs(objectiveFunction = "funBranin")
app$setInputs(tabs = "spotConfig")
app$setInputs(tabs = "runMode")
app$setInputs(runCreateDOE = "click")
app$setInputs(evaluateData = "click")
app$snapshot(list(output = "resultTable"))
