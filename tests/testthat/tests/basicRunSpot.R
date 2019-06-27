app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("basicRunSpot")

app$setInputs(tabs = "spotConfig")
try(app$setInputs(tabs = "runMode"), silent = T)
app$setInputs(runSpotIter = "click")
app$snapshot(list(output = "resultTable"))
