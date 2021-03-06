context("Shiny GUI Interactive Tests")
return()
cat(paste("\nStarting Shiny GUI Interactive Tests"))

runGUITests <- function(testNames){
    require("spotGUI")
    require("tools")

    for(testName in testNames){
        cat(paste("\nStarting a test:", testName))
        shinytest::expect_pass(shinytest::testApp(".",testName))
    }
}


runGUITests(c("basicRunSpot","basicCreateDOE","basicSpotThenDOEThenSPOT",
              "3dimBranin", "testGenoud4DimRF","testBranin_RF_LBFGSB_UR",
              "testWrongInputInBounds","testWrongEnvironmentFunction",
              "testWrongInputCreateDOE","testWrongInputEvaluatePoints",
              "testWrongInputProposeNewPoint","testOnlyRLog","testFunRastrigin",
              "testSmoofBirdFunction"))
