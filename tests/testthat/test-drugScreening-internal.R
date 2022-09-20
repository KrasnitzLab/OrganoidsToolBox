### Unit tests for drugScreening-internal.R functions

library(OrganoidsToolBox)


#############################################################################
### Tests plotMetric() results
#############################################################################

context("findOneDrugQuantile() results")

test_that("findOneDrugQuantile() must return error when drug information missing columns", {

    drugData <- data.frame(organoid_id=c("A", "B", "C"), dosage_type=c("TOP",
                    "TOP", "TOP"), drug_a=c("Metho", "Metho", "Placebo"),
                    relative_auc=c(43, 32, 22), stringsAsFactors=FALSE)

    error_message <- paste0("Mandatory columns are missing from the ",
        "drug screening dataset. The mandatory columns are: \'organoid_id\', ",
        "\'timestamp\', \'dosage_type\', \'drug_a\', \'drug_b\', \'drug_c\', ",
        "\'drug_background\' and \'relative_auc\'.")

    expect_error(OrganoidsToolBox:::findOneDrugQuantile(drugData=drugData,
        drugName="Metho", doseType="Averaged", quantile=1/3),
        error_message, fixed=TRUE)
})
