context("Package Data Storage")

test_that("packageDataStorage", {
    strName <- "someTestVariable"
    setEnvData(strName,77)
    expect_equal(getEnvData(strName),77)

    setEnvData(strName,TRUE)
    expect_equal(getEnvData(strName),TRUE)

    setEnvData(strName,list("a",77,TRUE))
    expect_equal(getEnvData(strName),list("a",77,TRUE))
})
