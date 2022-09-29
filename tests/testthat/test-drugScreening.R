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


test_that("selectOrgForOneDrug() must return error when doseType is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'doseType\' must be a single character ",
                                    "string.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
                    study="MEGA", screenType="TEST", doseType=22,
                    quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when doseType is a vector of strings", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'doseType\' must be a single character ",
                                        "string.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType=c("TypeA", "TypeB"),
            quantile=1/3), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when quantile is a string", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                                "numeric between 0 and 0.5.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile="1/3"), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when quantile negative numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                            "numeric between 0 and 0.5.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=-0.02), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when quantile above 0.5", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                            "numeric between 0 and 0.5.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=0.5000001), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    drugName <- "Demo"

    error_message <- paste0("The drug \'", drugName, "\' is not present ",
                      "in the drug screening dataset.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName=drugName,
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when study not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    study <- "Demo"

    error_message <- paste0("The study \'", study, "\' is not present in ",
                                "the drug screening dataset.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
            study=study, screenType="TEST", doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when screen type not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    screenType <- "Demo"

    error_message <- paste0("The screen type \'", screenType, "\' is not ",
                                "present in the drug screening dataset.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
            study="MEGA-TEST", screenType=screenType, doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    screenType <- "Demo"

    error_message <- paste0("The screen type \'", screenType, "\' is not ",
                            "present in the drug screening dataset.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
        study="MEGA-TEST", screenType=screenType, doseType="Averaged",
        quantile=0.2), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    doseType <- "Demo"

    error_message <- paste0("The dossage type \'", doseType, "\' is ",
                                "not present in the drug screening dataset.")

    expect_error(selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
        study="MEGA-TEST", screenType="TEST-01", doseType=doseType,
        quantile=0.2), error_message, fixed=TRUE)
})


test_that("selectOrgForOneDrug() must return expected results 01", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    study <- "MEGA-TEST"

    results <- selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
        study=study, screenType="TEST-01", doseType="Averaged", quantile=0.2)

    expected <- list()
    expected[["upper"]] <- 67.205871430000016
    expected[["lower"]] <- 52.803599419999998

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
        "hT1082", "hT2212"), relative_auc=c(46.61113619000000340975,
        52.61044455999999769347, 68.93725639000000171563, 75.67627993000000685697),
        GROUP=c("SENSITIVE", "SENSITIVE", "RESISTANT", "RESISTANT"),
        stringsAsFactors=FALSE)
    rownames(expected$extreme) <- NULL

    expect_true(inherits(results, "DrugAUCQuantile"))
    expect_equal(results$quantile$lower, expected$lower)
    expect_equal(results$quantile$upper, expected$upper)
    expect_equal(results$extreme, expected$extreme)

})



test_that("selectOrgForOneDrug() must return expected results 01", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    study <- "MEGA-TEST"

    results <- selectOrgForOneDrug(drugScreening=drug, drugName="Methotrexate",
        study=study, screenType=c("TEST-01", "TEST-02"), doseType="Averaged",
        quantile=0.1)

    expected <- list()
    expected[["upper"]] <- 72.87627993000000969914
    expected[["lower"]] <- 50.81065204900000509269

    expect_true(is.DrugAUCQuantile(results))
    expect_equal(results$quantile$lower, expected$lower)
    expect_equal(results$quantile$upper, expected$upper)

})

