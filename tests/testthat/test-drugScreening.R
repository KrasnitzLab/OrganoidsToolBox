### Unit tests for drugScreening.R functions

library(OrganoidsToolBox)


#############################################################################
### Tests selectOrgForOneDrug() results
#############################################################################

context("selectOrgForOneDrug() results")

test_that("selectOrgForOneDrug() must return error when drug information missing columns", {

    drugData <- data.frame(organoid_id=c("A", "B", "C"), dosage_type=c("TOP",
                    "TOP", "TOP"), drug_a=c("Metho", "Metho", "Placebo"),
                           relative_auc=c(43, 32, 22), stringsAsFactors=FALSE)

    error_message <- paste0("Mandatory columns are missing from the drug",
        " screening dataset. The mandatory columns are: \'organoid_id\', ",
        "\'timestamp\', \'study\', \'screen_type\', \'dosage_type\', ",
        "\'drug_a\', \'drug_b\', \'drug_c\', \'drug_background\' and ",
        "\'relative_auc\'.")

    expect_error(selectOrgForOneDrug(drugScreening=drugData, drugName="Metho",
        study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
        quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug screening is a numeric", {

    error_message <- paste0("The \'drugScreening\' must be a data.frame.")

    expect_error(selectOrgForOneDrug(drugScreening=33, drugName="Metho",
            study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
            quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug name is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'drugName\' must be a single character ",
                                "string.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName=22,
                study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug name is a vector of strings", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'drugName\' must be a single character ",
                                "string.")

    expect_error(selectOrgForOneDrug(drugScreening=drug,
        drugName=c("Drug1", "Drug2"), study="MEGA-TEST", screenType="TEST-01",
        doseType="Averaged", quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when study is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'study\' must be a vector of character ",
                                    "strings.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
                study=33, screenType="TEST-01", doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when screenType is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'screenType\' must be a vector of character",
                                    " strings.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
                study="MEGA", screenType=33, doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})
