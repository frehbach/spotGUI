runGUITests <- function(testNames){
    require("shinytest")
    require("spotGUI")
    require("tools")

    for(testName in testNames){
        cat(paste("\nStarting a test:", testName))
        testApp(".",testName)
    }
}


runGUITests(c("basicRunSpot","basicCreateDOE","basicSpotThenDOEThenSPOT","3dimBranin",
              "testGenoud4DimRF","testBranin_RF_LBFGSB_UR","testWrongInputInBounds",
              "testWrongEnvironmentFunction","testWrongInputCreateDOE","testWrongInputEvaluatePoints",
              "testWrongInputProposeNewPoint","testOnlyRLog","testFunRastrigin"))
