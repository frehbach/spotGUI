
test_that("Non existing package/method Returns NULL", {
    expect_equal(spotGUI:::getMethodHelp("SPOT","someNonExistingFunction"),NULL);
    expect_equal(spotGUI:::getMethodHelp("someNonExistingPackage","someNonExistingFunction"),NULL);

    expect_equal(spotGUI:::getMethodHelpArgument("someNonExistingPackage","someNonExistingFunction"),NULL);
    #A function without Argumentssection:
    expect_equal(spotGUI:::getMethodHelpArgument("SPOT","SPOT"),NULL);

    expect_equal(spotGUI:::getMethodHelpControlList("SPOT"),NULL);
    expect_equal(spotGUI:::getMethodHelpControlList("someNonExistingFunction"),NULL);

    expect_equal(spotGUI:::getAllHelpControlListParams("SPOT"),NULL);
    expect_equal(spotGUI:::getAllHelpControlListParams("someNonExistingFunction"),NULL);
    #some function with arguments but without control list
    expect_equal(spotGUI:::getAllHelpControlListParams("corrcubic"),NULL);
})

test_that("help reading output correct", {
    expect_equal(typeof(spotGUI:::getMethodHelp("SPOT","buildKriging")),"list");
    expect_equal(typeof(spotGUI:::getMethodHelpArgument("SPOT","buildKriging")),"list");
    expect_equal(typeof(spotGUI:::getMethodHelpControlList("buildKriging")),"list");
    expect_equal(typeof(spotGUI:::getAllHelpControlListParams("buildKriging")),"list");

    expect_equal(spotGUI:::getHelpSpotControlParameters()$noise,"Boolean, whether the objective function has noise or not. Default is non-noisy, that is, FALSE.");
})
