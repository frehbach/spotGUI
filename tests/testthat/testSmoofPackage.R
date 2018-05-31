
context("smoofFunctions")

checkAllBounds <- function(){
    succeeded <- T
    for(e in getAllSmoofFunctions()){
        if(length(as.vector(getSmoofFunLowerBounds(e))) < 2){
            succeeded <- F
        }
    }
    return(succeeded)
}

test_that("All Smoof functions are accessible and parameters can be read", {
    #There should be 74 non-blacklisted single objective functions
    expect_equal(length(getAllSmoofFunctions()), 74)
    expect_equal(as.vector(getSmoofFunLowerBounds("Rastrigin")),c(-5.12, -5.12))
    expect_equal(as.vector(getSmoofFunUpperBounds("Rastrigin")),c(5.12, 5.12))

    #check that bounds can be found for all existing functions
    expect_equal(checkAllBounds(), TRUE)
})
