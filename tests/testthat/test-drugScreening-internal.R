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
    expected[["quantile"]][["value"]] <- 1/3
    expected[["quantile"]][["lower"]] <- 53.5762188599999973349
    expected[["quantile"]][["upper"]] <- 60.2803315900000029615

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                    "hT2211", "hT1919", "hT1051", "hT1082",
                                    "hT2212"),
                                group=c(rep("SENSITIVE", 3), "AVERAGE",
                                    rep("RESISTANT", 3)),
                                stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])

    expect_equal(nrow(result[["extreme"]]), nrow(result[["dataset"]]))

    expect_equal(result[["extreme"]]$organoid_id,
                    expected[["extreme"]]$organoid_id)

    expect_equal(result[["extreme"]]$group,
                    expected[["extreme"]]$group)
})


test_that("findOneDrugQuantile() must return expected results 02", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    result <- OrganoidsToolBox:::findOneDrugQuantile(drugData=drug,
                drugName="Methotrexate", doseType="Averaged", quantile=1/4)

    expected <- list()
    expected[["quantile"]]
    expected[["quantile"]][["value"]] <- 1/4
    expected[["quantile"]][["lower"]] <- 53.0933317100000010669
    expected[["quantile"]][["upper"]] <- 64.6087939900000094440

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                "hT2211", "hT1919", "hT1051",
                                "hT1082", "hT2212"),
                            group=c(rep("SENSITIVE", 2), rep("AVERAGE", 3),
                                rep("RESISTANT", 2)),
                            stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])

    expect_equal(nrow(result[["extreme"]]), nrow(result[["dataset"]]))

    expect_equal(result[["extreme"]]$organoid_id,
                 expected[["extreme"]]$organoid_id)

    expect_equal(result[["extreme"]]$group,
                 expected[["extreme"]]$group)
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



#############################################################################
### Tests findQuantileOneDrug() results
#############################################################################

context("findQuantileOneDrug() results")


test_that("findQuantileOneDrug() must return expected results 01", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))

    ## Select the specified dose type
    orgDR.avr <- drug[which(drug$dosage_type == "Averaged"), ]

    ## Select the specified drug
    orgDR.avr <- orgDR.avr[which(tolower(orgDR.avr$drug_a) ==
                                     "methotrexate" &
                                     orgDR.avr$drug_b == "N/A" &
                                     orgDR.avr$drug_c == "N/A" &
                                     orgDR.avr$drug_background == "N/A"), ]

    ## Remove duplicate
    orgDR.avr.u <- orgDR.avr[-1 * which(duplicated(orgDR.avr[,
                        c("organoid_id", "timestamp", "drug_a")])),]

    result <- OrganoidsToolBox:::findQuantileOneDrug(cleanDrugData=orgDR.avr.u,
                                                        quantile=1/3)

    expected <- list()
    expected[["quantile"]]
    expected[["quantile"]][["value"]] <- 1/3
    expected[["quantile"]][["lower"]] <- 53.5762188599999973349
    expected[["quantile"]][["upper"]] <- 60.2803315900000029615

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                    "hT2211", "hT1919", "hT1051", "hT1082",
                                    "hT2212"),
                                        group=c(rep("SENSITIVE", 3), "AVERAGE",
                                                rep("RESISTANT", 3)),
                                        stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])

    expect_equal(nrow(result[["extreme"]]), nrow(result[["dataset"]]))

    expect_equal(result[["extreme"]]$organoid_id,
                 expected[["extreme"]]$organoid_id)

    expect_equal(result[["extreme"]]$group, expected[["extreme"]]$group)
})


test_that("findOneDrugQuantile() must return expected results 02", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))


    ## Select the specified dose type
    orgDR.avr <- drug[which(drug$dosage_type == "Averaged"), ]

    ## Select the specified drug
    orgDR.avr <- orgDR.avr[which(tolower(orgDR.avr$drug_a) ==
                                     "methotrexate" &
                                     orgDR.avr$drug_b == "N/A" &
                                     orgDR.avr$drug_c == "N/A" &
                                     orgDR.avr$drug_background == "N/A"), ]

    ## Remove duplicate
    orgDR.avr.u <- orgDR.avr[-1 * which(duplicated(orgDR.avr[,
                            c("organoid_id", "timestamp", "drug_a")])),]

    result <- OrganoidsToolBox:::findQuantileOneDrug(cleanDrugData=orgDR.avr.u,
                                                        quantile=1/4)

    expected <- list()
    expected[["quantile"]]
    expected[["quantile"]][["value"]] <- 1/4
    expected[["quantile"]][["lower"]] <- 53.0933317100000010669
    expected[["quantile"]][["upper"]] <- 64.6087939900000094440

    expected[["extreme"]] <- data.frame(organoid_id=c("hT3312", "hT1001",
                                        "hT2211", "hT1919", "hT1051",
                                        "hT1082", "hT2212"),
                                group=c(rep("SENSITIVE", 2), rep("AVERAGE", 3),
                                        rep("RESISTANT", 2)),
                                stringsAsFactors=FALSE)

    expect_equal(result[["quantile"]], expected[["quantile"]])

    expect_equal(nrow(result[["extreme"]]), nrow(result[["dataset"]]))

    expect_equal(result[["extreme"]]$organoid_id,
                 expected[["extreme"]]$organoid_id)

    expect_equal(result[["extreme"]]$group, expected[["extreme"]]$group)
})


test_that("findOneDrugQuantile() must return expected error when not enough data", {

    drug <- readRDS(test_path("fixtures", "OneDrugDemoFile.RDS"))
    drug <- drug[1:2,]

    error_message <- paste0("There is not enough data (less than 3 organoids) ",
        "with the current critera to run quantile analysis.")

    expect_error(OrganoidsToolBox:::findQuantileOneDrug(cleanDrugData=drug,
        quantile=1/4), error_message, fixed=TRUE)
})
