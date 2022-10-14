### Unit tests for enrichment.R functions

library(OrganoidsToolBox)



#############################################################################
### Tests fisherCategoricalVariable() results
#############################################################################

context("fisherCategoricalVariable() results")


test_that("fisherCategoricalVariable() must return error when drugQuantile is numeric", {

    patientData <- data.frame(organoid_id=c("hT1001", "hT1082", "hT1919",
        "hT3312", "hT1051", "hT2211", "hT2212", "hT2213", "hT2251"),
        patient_id=c("1", "2", "3", "2", "3", "4", "4", "6", "7"),
        stringsAsFactors=FALSE)

    error_message <- paste0("The \'drugQuantile\' parameter must be a ",
                                    "DrugAUCQuantile object.")

    expect_error(fisherCategoricalVariable(drugQuantile=33,
        patientInfo=patientData, category="test"), error_message, fixed=TRUE)
})
