app <- ShinyDriver$new("../", seed = 1)
app$snapshotInit("basicRunSpot")

app$setInputs(tabs = "spotConfig")
app$setInputs(tabs = "runMode")
app$setInputs(runSpotIter = "click")
Sys.sleep(1)
app$snapshot(list(output = "resultTable"))
