### Unit tests for enrichment.R functions

library(OrganoidsToolBox)



#############################################################################
### Tests fisherCategoricalVariable() results
#############################################################################

context("fisherCategoricalVariable() results")


test_that("fisherCategoricalVariable() must return error when drugQuantile is numeric", {

    error_message <- paste0("The \'drugQuantile\' parameter must be a ",
                                    "DrugAUCQuantile object.")

    expect_error(fisherCategoricalVariable(drugQuantile=33, category="test"),
        error_message, fixed=TRUE)
})


test_that("fisherCategoricalVariable() must return error when category is numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    results <- getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
        study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
        quantile=0.2)

    error_message <- paste0("The \'category\' parameter must be a character",
                                    " string.")

    expect_error(fisherCategoricalVariable(drugQuantile=results,
       category=22), error_message, fixed=TRUE)
})
