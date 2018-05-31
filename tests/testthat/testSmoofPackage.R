
context("smoofFunctions")

test_that("All Smoof functions are accessible and parameters can be read", {
    funNames <- getAllSmoofFunctions()
    #There should be 74 non-blacklisted single objective functions
    expect_equal(length(funNames) == 74)
    expect_equal(as.vector(getSmoofFunLowerBounds("Rastrigin")) == c(-5.12, -5.12))
    expect_equal(as.vector(getSmoofFunUpperBounds("Rastrigin")) == c(5.12, 5.12))

    #check that bounds can be found for all existing functions
    expect_equal(function(){
        succeeded <- T
        for(e in funNames){
            if(length(as.vector(getSmoofFunLowerBounds(e))) < 2){
                succeeded <- F
            }
        }
        return(succeeded)
    } == T)
})
