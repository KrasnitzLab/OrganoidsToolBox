### Unit tests for drugScreening-internal.R functions

library(OrganoidsToolBox)


#############################################################################
### Tests findOneDrugQuantile() results
#############################################################################

context("findOneDrugQuantile() results")


test_that("findOneDrugQuantile() must return expected results 01", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    result <- OrganoidsToolBox:::findOneDrugQuantile(drugData=drug,
                drugName="Methotrexate", doseType="Averaged", quantile=1/3)

    expected <- list()
    expected[["quantile"]]
    expected[["quantile"]][["lower"]] <- 53.5762188599999973349
    expected[["quantile"]][["upper"]] <- 60.2803315900000029615

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                "hT2211", "hT1051", "hT1082", "hT2212"),
                                GROUP=c(rep("SENSITIVE", 3),
                                rep("RESISTANT", 3)), stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])
    expect_equal(result[["extreme"]]$organoid_id,
                    expected[["extreme"]]$organoid_id)
    expect_equal(result[["extreme"]]$GROUP,
                    expected[["extreme"]]$GROUP)
})


test_that("findOneDrugQuantile() must return expected results 02", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    result <- OrganoidsToolBox:::findOneDrugQuantile(drugData=drug,
                drugName="Methotrexate", doseType="Averaged", quantile=1/4)

    expected <- list()
    expected[["quantile"]]
    expected[["quantile"]][["lower"]] <- 53.0933317100000010669
    expected[["quantile"]][["upper"]] <- 64.6087939900000094440

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                                        "hT1082", "hT2212"),
                                GROUP=c(rep("SENSITIVE", 2),
                                rep("RESISTANT", 2)), stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])
    expect_equal(result[["extreme"]]$organoid_id,
                 expected[["extreme"]]$organoid_id)
    expect_equal(result[["extreme"]]$GROUP,
                 expected[["extreme"]]$GROUP)
})


test_that("findOneDrugQuantile() must return expected error when not enough data", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))
    drug <- drug[1:10,]

    error_message <- paste0("There is not enough data (less than 3 organoids) ",
                    "with the current critera to run quantile analysis.")

    expect_error(OrganoidsToolBox:::findOneDrugQuantile(drugData=drug,
        drugName="Methotrexate", doseType="Averaged", quantile=1/4),
        error_message, fixed=TRUE)
})
