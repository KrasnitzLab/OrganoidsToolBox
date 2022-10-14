### Unit tests for drugScreening.R functions

library(OrganoidsToolBox)


#############################################################################
### Tests getClassOneDrug() results
#############################################################################

context("getClassOneDrug() results")

test_that("getClassOneDrug() must return error when drug information missing columns", {

    drugData <- data.frame(organoid_id=c("A", "B", "C"), dosage_type=c("TOP",
                    "TOP", "TOP"), drug_a=c("Metho", "Metho", "Placebo"),
                           relative_auc=c(43, 32, 22), stringsAsFactors=FALSE)

    error_message <- paste0("Mandatory columns are missing from the drug",
        " screening dataset. The mandatory columns are: \'organoid_id\', ",
        "\'timestamp\', \'study\', \'screen_type\', \'dosage_type\', ",
        "\'drug_a\', \'drug_b\', \'drug_c\', \'drug_background\' and ",
        "\'relative_auc\'.")

    expect_error(getClassOneDrug(drugScreening=drugData, drugName="Metho",
        study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
        quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug screening is a numeric", {

    error_message <- paste0("The \'drugScreening\' must be a data.frame.")

    expect_error(getClassOneDrug(drugScreening=33, drugName="Metho",
            study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
            quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug name is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'drugName\' must be a single character ",
                                "string.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName=22,
                study="MEGA-TEST", screenType="TEST-01", doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug name is a vector of strings", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'drugName\' must be a single character ",
                                "string.")

    expect_error(getClassOneDrug(drugScreening=drug,
        drugName=c("Drug1", "Drug2"), study="MEGA-TEST", screenType="TEST-01",
        doseType="Averaged", quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when study is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'study\' must be a vector of character ",
                                    "strings.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
                study=33, screenType="TEST-01", doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when screenType is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'screenType\' must be a vector of character",
                                    " strings.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
                study="MEGA", screenType=33, doseType="Averaged",
                quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when doseType is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'doseType\' must be a single character ",
                                    "string.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
                    study="MEGA", screenType="TEST", doseType=22,
                    quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when doseType is a vector of strings", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'doseType\' must be a single character ",
                                        "string.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType=c("TypeA", "TypeB"),
            quantile=1/3), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when quantile is a string", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                                "numeric between 0 and 0.5.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile="1/3"), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when quantile negative numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                            "numeric between 0 and 0.5.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=-0.02), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when quantile above 0.5", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    error_message <- paste0("The \'quantile\' must be a single positive ",
                            "numeric between 0 and 0.5.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Metho",
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=0.5000001), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    drugName <- "Demo"

    error_message <- paste0("The drug \'", drugName, "\' is not present ",
                      "in the drug screening dataset.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName=drugName,
            study="MEGA", screenType="TEST", doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when study not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    study <- "Demo"

    error_message <- paste0("The study \'", study, "\' is not present in ",
                                "the drug screening dataset.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
            study=study, screenType="TEST", doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when screen type not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    screenType <- "Demo"

    error_message <- paste0("The screen type \'", screenType, "\' is not ",
                                "present in the drug screening dataset.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
            study="MEGA-TEST", screenType=screenType, doseType="Averaged",
            quantile=0.2), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    screenType <- "Demo"

    error_message <- paste0("The screen type \'", screenType, "\' is not ",
                            "present in the drug screening dataset.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
        study="MEGA-TEST", screenType=screenType, doseType="Averaged",
        quantile=0.2), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return error when drug not in the dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    doseType <- "Demo"

    error_message <- paste0("The dossage type \'", doseType, "\' is ",
                                "not present in the drug screening dataset.")

    expect_error(getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
        study="MEGA-TEST", screenType="TEST-01", doseType=doseType,
        quantile=0.2), error_message, fixed=TRUE)
})


test_that("getClassOneDrug() must return expected results 01", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    study <- "MEGA-TEST"

    results <- getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
        study=study, screenType="TEST-01", doseType="Averaged", quantile=0.2)

    expected <- list()
    expected[["upper"]] <- 67.205871430000016
    expected[["lower"]] <- 52.803599419999998

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
            "hT2211", "hT1919", "hT1051", "hT1082", "hT2212"),
        relative_auc=c(46.61113619000000340975, 52.61044455999999769347,
                       53.57621885999999733485, 56.53001800999999915120,
                       60.28033159000000296146,
                       68.93725639000000171563, 75.67627993000000685697),
        group=c("SENSITIVE", "SENSITIVE", rep("AVERAGE", 3),
                    "RESISTANT", "RESISTANT"),
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

    results <- getClassOneDrug(drugScreening=drug, drugName="Methotrexate",
        study=study, screenType=c("TEST-01", "TEST-02"), doseType="Averaged",
        quantile=0.1)

    expected <- list()
    expected[["upper"]] <- 72.87627993000000969914
    expected[["lower"]] <- 50.81065204900000509269

    expect_true(is.DrugAUCQuantile(results))
    expect_equal(results$quantile$lower, expected$lower)
    expect_equal(results$quantile$upper, expected$upper)

})


#############################################################################
### Tests plotDrugAUCViolinPlot() results
#############################################################################

context("plotDrugAUCViolinPlot() results")

test_that("plotDrugAUCViolinPlot() must return error when drugQuantile is character string", {

    error_message <- paste0("The \'drugQuantile\' parameter must ",
        "be a DrugAUCQuantile or DrugAUCQuantileNoReplicate object.")

    expect_error(plotDrugAUCViolinPlot(drugQuantile="test", min=0, max=100,
        trim=FALSE), error_message, fixed=TRUE)
})


test_that("plotDrugAUCViolinPlot() must return error when min is character string", {

    drug <- readRDS(test_path("fixtures", "drugQuantileTest01.RDS"))

    error_message <- "The \'min\' parameter must be a single numeric."

    expect_error(plotDrugAUCViolinPlot(drugQuantile=drug, min="22", max=100,
            trim=FALSE), error_message, fixed=TRUE)
})


test_that("plotDrugAUCViolinPlot() must return error when max is character string", {

    drug <- readRDS(test_path("fixtures", "drugQuantileTest01.RDS"))

    error_message <- paste0("The \'max\' parameter must be a single ",
            "numeric superio to the 'min' parameter.")

    expect_error(plotDrugAUCViolinPlot(drugQuantile=drug, min=22, max="100",
            trim=FALSE), error_message, fixed=TRUE)
})


test_that("plotDrugAUCViolinPlot() must return error when max is lower than min", {

    drug <- readRDS(test_path("fixtures", "drugQuantileTest01.RDS"))

    error_message <- paste0("The \'max\' parameter must be a single ",
        "numeric superio to the 'min' parameter.")

    expect_error(plotDrugAUCViolinPlot(drugQuantile=drug, min=122, max=21,
        trim=FALSE), error_message, fixed=TRUE)
})


test_that("plotDrugAUCViolinPlot() must return error when trim is a numeric", {

    drug <- readRDS(test_path("fixtures", "drugQuantileTest01.RDS"))

    error_message <- "The \'trim\' parameter must be a logical (TRUE or FALSE)."

    expect_error(plotDrugAUCViolinPlot(drugQuantile=drug, min=1, max=21,
        trim=22), error_message, fixed=TRUE)
})


############################################################################
### Tests plotDrugAUCDensityCurve() results
#############################################################################

context("plotDrugAUCViolinPlot() results")

test_that("plotDrugAUCDensityCurve() must return error when drugQuantile is character string", {

    error_message <- paste0("The \'drugQuantile\' parameter must be a ",
        "DrugAUCQuantile or DrugAUCQuantileNoReplicate object.")

    expect_error(plotDrugAUCDensityCurve(drugQuantile="33", byGroup=FALSE),
                        error_message, fixed=TRUE)
})


test_that("plotDrugAUCViolinPlot() must return error when byGroup is character string", {

    drug <- readRDS(test_path("fixtures", "drugQuantileTest01.RDS"))

    error_message <- "The \'byGroup\' parameter must be a logical (TRUE or FALSE)."

    expect_error(plotDrugAUCDensityCurve(drugQuantile=drug, byGroup="3"),
                 error_message, fixed=TRUE)
})


#############################################################################
### Tests selectNoReplicateOrganoids() results
#############################################################################

context("selectNoReplicateOrganoids() results")

test_that("selectNoReplicateOrganoids() must return error when drugscreen is a numeric", {

    drug <- 33

    patientData <- data.frame(organoid_id=c("A", "B", "C"), patient_id=c("1",
        "2", "3"), stringsAsFactors=FALSE)

    error_message <- "The \'drugScreening\' must be a data.frame."

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData), error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return error when patientInfo is a numeric", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    patientData <- 33

    error_message <- "The \'patientInfo\' must be a data.frame."

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData), error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return error when organoid_id column not in drug dataset", {

    drug <- data.frame(organoids_id=c("A", "B", "C"), auc=c(12, 21, 22))

    patientData <- data.frame(organoid_id=c("A", "B", "C"), patient_id=c("1",
                        "2", "3"), stringsAsFactors=FALSE)

    error_message <- "The mandatory column \'organoid_id\' is missing."

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData),  error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return error when organoid_id column not in patient info dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    patientData <- data.frame(organoids_id=c("A", "B", "C"), patient_id=c("1",
                        "2", "3"), stringsAsFactors=FALSE)

    error_message <- paste0("Mandatory columns are missing from the ",
        "patient info dataset. The mandatory columns are: \'organoid_id\' and ",
        "\'patient_id\'.")

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData), error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return error when patient_id column not in patient info dataset", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    patientData <- data.frame(organoid_id=c("A", "B", "C"), patients_id=c("1",
                            "2", "3"), stringsAsFactors=FALSE)

    error_message <- paste0("Mandatory columns are missing from the ",
        "patient info dataset. The mandatory columns are: \'organoid_id\' and ",
        "\'patient_id\'.")

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData), error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return error when organoid without patient info", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))

    patientData <- data.frame(organoid_id=c("A", "B", "C"), patient_id=c("1",
                            "2", "3"), stringsAsFactors=FALSE)

    error_message <- paste0("Not all organoids have an associated patient",
                                " information.")

    expect_error(selectNoReplicateOrganoids(drugScreening=drug,
        patientInfo=patientData), error_message, fixed=TRUE)
})


test_that("selectNoReplicateOrganoids() must return expected results", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile02.RDS"))
    drug <- drug[, c("organoid_id", "study")]

    patientData <- data.frame(organoid_id=c("hT1001", "hT1082", "hT1919",
        "hT3312", "hT1051", "hT2211", "hT2212", "hT2213", "hT2251"),
        patient_id=c("1", "2", "3", "2", "3", "4", "4", "6", "7"),
        stringsAsFactors=FALSE)

    set.seed(1212)

    result <- selectNoReplicateOrganoids(drugScreening=drug,
                               patientInfo=patientData)

    expected <- data.frame(organoid_id=c( "hT1001", "hT1919",  "hT2212",
        "hT2213", "hT2251", "hT3312"), study=c(rep("MEGA-TEST", 4),
        "SUPER-TEST", "MEGA-TEST"),
        patient_id=c("1", "3", "4", "6", "7", "2"), stringsAsFactors=FALSE)
    rownames(expected) <- expected$organoid_id

    expect_equal(result, expected)
})
