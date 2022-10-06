#' @title Filter the drug screen dataset using specific information: drug name,
#' dosage type, name of the study and screen type.
#'
#' @description The function extracts information from a drug screen dataset
#' that is related to the specified parameters. The filters are specific to
#' the drug name ('drug_a' column), the dosage type ('dodsage_type' column),
#' the name of the study ('study' column) and
#' the screen type ('dosage_type' column).
#'
#' @param drugData a \code{data.frame} containing the drug screening
#' information. The mandatory columns are: 'organoid_id', 'timestamp', 'study',
#' 'screen_type', 'dosage_type', 'drug_a', 'drug_b', 'drug_c',
#' 'drug_background',and 'relative_auc'.
#'
#' @param drugName a single \code{character} string representing the
#' selected drug.
#'
#' @param study a single \code{character} string representing the name of
#' the study selected for the analyses. The study must be present in the drug
#' screening dataset. The study can be found in the 'study' column of the
#' drug screening dataset.
#'
#' @param screenType a \code{vector} of \code{character} string representing
#' the type of screening selected for the analyses. The type must be present
#' in the drug screening dataset. The screen type can be found in
#' the 'screen_type' column of the drug screening dataset.
#'
#' @param doseType a single \code{character} string representing the
#' selected dosage type. Default: \code{"Averaged"}.
#'
#' @return a \code{data.frame} containing the selected organoids according to
#' the parameter specification. Those columns are present: 'organoid_id',
#' 'timestamp', dosage_type', 'drug_a', 'drug_b', 'drug_c', 'drug_background',
#' and 'relative_auc'.
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(simpleDrugScreening)
#'
#' ## Filter the drug screening dataset to retain specific rows
#' results <- OrganoidsToolBox:::filterDrugScreenOneDrug(
#'     drugData=simpleDrugScreening, drugName="Methotrexate",
#'     study="MEGA-TEST", screenType="TEST-01", doseType="Averaged")
#'
#'
#' @author Pascal Belleau, Astrid Deschênes
#' @importFrom stats quantile
#' @encoding UTF-8
#' @keywords internal
filterDrugScreenOneDrug <- function(drugData, drugName, study, screenType,
                                doseType) {

    ## Select the specified study
    selectedDrug <- drugData[which(tolower(drugData$study) ==
                                                    tolower(study)), ]

    ## Select the specified screen type
    selectedDrug <- selectedDrug[which(tolower(selectedDrug$screen_type)
                                            %in% tolower(screenType)), ]

    ## Select the specified dose type
    orgDR.avr <- selectedDrug[which(tolower(selectedDrug$dosage_type) ==
                                                        tolower(doseType)), ]

    ## Select the specified drug name
    orgDR.avr <- orgDR.avr[which(tolower(orgDR.avr$drug_a) ==
                                        tolower(drugName) &
                                        orgDR.avr$drug_b == "N/A" &
                                        orgDR.avr$drug_c == "N/A" &
                                        orgDR.avr$drug_background == "N/A"), ]

    ## Remove duplicate lines
    orgDR.avr.u <- orgDR.avr[-1 * which(duplicated(orgDR.avr[,
                                c("organoid_id", "timestamp", "drug_a")])),]

    return(orgDR.avr.u)
}

#' @title Extract the sensitive and resistant organoids
#' for a specific drug from a drug screening dataset
#'
#' @description The function extract the sensitive and resistant organoids
#' for a specific drug. The organoids with an average AUC equal or inferior to
#' the specified quantile are considered sensitive while the organoids with an
#' average AUC superior or equal to (1 - quantile) are considered resistant.
#' The function requires a minimum of 3 values to run the quantile analysis.
#'
#' @param cleanDrugData a \code{data.frame} containing the drug screening
#' information. The mandatory columns are: 'organoid_id', 'timestamp',
#' 'dosage_type', 'drug_a', 'drug_b', 'drug_c', 'drug_background',
#' and 'relative_auc'.
#'
#' @param quantile a single positive \code{numeric} between \code{0} and
#' \code{0.5} representing the cutoff for the organoids selection.
#'
#' @return a \code{list} containing 3 entries:
#' \itemize{
#' \item{quantile}{the \code{upper} and \code{lower} quantiles for the
#' specified drug.}
#' \item{dataset}{a \code{data.frame} containing the data used to
#' calculate the quantiles. }
#' \item{extreme}{a \code{data.frame} containing the sensitive and
#' resistant organoids according to the specified quantiles.
#' The \code{data.frame} also contains the relative AUC. }
#' }
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(simpleDrugScreening)
#'
#' ## Filter the drug screen dataset
#' cleanData <- OrganoidsToolBox:::filterDrugScreenOneDrug(
#'     drugData=simpleDrugScreening, drugName="Methotrexate",
#'     study="MEGA-TEST", screenType="TEST-01", doseType="Averaged")
#'
#' ## Calculate the extreme organoids for the cleaned methotrexate drug
#' ## screening using a quantile of 1/4
#' results <- OrganoidsToolBox:::findQuantileOneDrug(
#'     cleanDrugData=cleanData, quantile=1/4)
#'
#' ## The classification of the organoids is in the 'extreme' entry
#' results$extreme
#'
#' @author Pascal Belleau, Astrid Deschênes
#' @importFrom stats quantile
#' @encoding UTF-8
#' @keywords internal
findQuantileOneDrug <- function(cleanDrugData, quantile) {

    ## Check that the number of rows is sufficient
    if (nrow(cleanDrugData) < 3) {
        stop("There is not enough data (less than 3 organoids) with ",
            "the current critera to run quantile analysis.")
    }

    ## Prepare results
    results <- list()
    results[["quantile"]] <- list()

    ## Calculate upper and lower quantile
    results[["quantile"]][["value"]] <- quantile
    results[["quantile"]][["lower"]] <-
                    unname(quantile(cleanDrugData$relative_auc, quantile))
    results[["quantile"]][["upper"]] <-
                    unname(quantile(cleanDrugData$relative_auc, 1 - quantile))

    results[["dataset"]] <- cleanDrugData[order(cleanDrugData$relative_auc), ]
    rownames(results[["dataset"]]) <- NULL

    ## Select sensitive organoids
    extreme <- cleanDrugData[which(cleanDrugData$relative_auc <=
                                        results[["quantile"]][["lower"]]),]
    extreme$group <- rep("SENSITIVE", nrow(extreme))

    ## Select resistant organoids
    extreme2 <- cleanDrugData[which(cleanDrugData$relative_auc >=
                                        results[["quantile"]][["upper"]]),]
    extreme2$group <- rep("RESISTANT", nrow(extreme2))

    ## Select resistant organoids
    extreme3 <- cleanDrugData[which(!cleanDrugData$organoid_id %in%
                            c(extreme$organoid_id, extreme2$organoid_id)),]
    extreme3$group <- rep("AVERAGE", nrow(extreme3))

    ## Merge all results
    final <- rbind(extreme, extreme2, extreme3)
    final <- final[, c("organoid_id", "relative_auc", "group")]
    results[["extreme"]] <- final[order(final$relative_auc, decreasing=FALSE),]
    rownames(results[["extreme"]]) <- NULL

    return(results)
}


#' @title Extract the sensitive and resistant organoids
#' for a specific drug from a drug screening dataset
#'
#' @description The function extract the sensitive and resistant organoids
#' for a specific drug. The organoids with an average AUC equal or inferior to
#' the specified quantile are considered sensitive while the organoids with an
#' average AUC superior or equal to (1 - quantile) are considered resistant.
#' The function requires a minimum of 3 values to run the quantile analysis.
#'
#' @param drugData a \code{data.frame} containing the drug screening
#' information. The mandatory columns are: 'organoid_id', 'timestamp',
#' 'dosage_type', 'drug_a', 'drug_b', 'drug_c', 'drug_background',
#' and 'relative_auc'.
#'
#' @param drugName a single \code{character} string representing the
#' selected drug.
#'
#' @param doseType a single \code{character} string representing the
#' selected dosage type. Default: \code{"Averaged"}.
#'
#' @param quantile a single positive \code{numeric} between \code{0} and
#' \code{0.5} representing the cutoff for the organoids selection.
#'
#' @return a \code{list} containing 3 entries:
#' \itemize{
#' \item{quantile}{the \code{upper} and \code{lower} quantiles for the
#' specified drug.}
#' \item{dataset}{a \code{data.frame} containing the data used to
#' calculate the quantiles. }
#' \item{extreme}{a \code{data.frame} containing the sensitive and
#' resistant organoids according to the specified quantiles.
#' The \code{data.frame} also contains the relative AUC. }
#' }
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(simpleDrugScreening)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/4
#' results <- OrganoidsToolBox:::findOneDrugQuantile(
#'     drugData=simpleDrugScreening,
#'     drugName="Methotrexate", doseType="Averaged", quantile=1/4)
#'
#' ## The classification of the organoids is in the 'extreme' entry
#' results$extreme
#'
#' @author Pascal Belleau, Astrid Deschênes
#' @importFrom stats quantile
#' @encoding UTF-8
#' @keywords internal
findOneDrugQuantile <- function(drugData, drugName, doseType="Averaged",
                                quantile) {

    ## Select the specified dose type
    orgDR.avr <- drugData[which(tolower(drugData$dosage_type) ==
                                    tolower(doseType)), ]

    ## Select the specified drug
    orgDR.avr <- orgDR.avr[which(tolower(orgDR.avr$drug_a) ==
                                     tolower(drugName) &
                                     orgDR.avr$drug_b == "N/A" &
                                     orgDR.avr$drug_c == "N/A" &
                                     orgDR.avr$drug_background == "N/A"), ]

    ## Remove duplicate
    orgDR.avr.u <- orgDR.avr[-1 * which(duplicated(orgDR.avr[,
                                    c("organoid_id", "timestamp", "drug_a")])),]

    ## Check that the number of rows is sufficient
    if (nrow(orgDR.avr.u) < 3) {
        stop("There is not enough data (less than 3 organoids) with ",
             "the current critera to run quantile analysis.")
    }

    results <- list()
    results[["quantile"]] <- list()

    ## Calculate upper and lower quantile
    results[["quantile"]][["value"]] <- quantile
    results[["quantile"]][["lower"]] <-
        unname(quantile(orgDR.avr.u$relative_auc, quantile))
    results[["quantile"]][["upper"]] <-
        unname(quantile(orgDR.avr.u$relative_auc, 1 - quantile))

    results[["dataset"]] <- orgDR.avr.u[order(orgDR.avr.u$relative_auc), ]
    rownames(results[["dataset"]]) <- NULL

    ## Select sensitive organoids
    extreme <- orgDR.avr.u[which(orgDR.avr.u$relative_auc <=
                                     results[["quantile"]][["lower"]]),]
    extreme$group <- rep("SENSITIVE", nrow(extreme))

    ## Select resistant organoids
    extreme2 <- orgDR.avr.u[which(orgDR.avr.u$relative_auc >=
                                      results[["quantile"]][["upper"]]),]
    extreme2$group <- rep("RESISTANT", nrow(extreme2))


    ## Select resistant organoids
    extreme3 <- orgDR.avr.u[which(!orgDR.avr.u$organoid_id %in%
                        c(extreme$organoid_id, extreme2$organoid_id)),]
    extreme3$group <- rep("AVERAGE", nrow(extreme3))

    final <- rbind(extreme, extreme2, extreme3)
    final <- final[, c("organoid_id", "relative_auc", "group")]
    results[["extreme"]] <- final[order(final$relative_auc, decreasing=FALSE),]
    rownames(results[["extreme"]]) <- NULL

    return(results)
}


#' @title Validate input parameters for the selectOrgForOneDrug() function
#'
#' @description The function validates the input parameters for the
#' selectOrgForOneDrug() function.
#'
#' @param drugScreening a single \code{character} string representing the path
#' and name of the drug screening file that contains the information needed
#' to run the organoid selection. Those columns are mandatory: 'organoid_id',
#' 'timestamp', 'study', 'screen_type', 'dosage_type',
#' 'drug_a', 'drug_b', 'drug_c', 'drug_background' and 'relative_auc'.
#'
#' @param drugName a single \code{character} string representing the name of
#' the drug selected for the analyses.
#'
#' @param study a single \code{character} string representing the name of
#' the study selected for the analyses.
#' @param screenType a \code{vector} of \code{character} string representing
#' the type of
#' screening selected for the analyses.
#'
#' @param doseType a single \code{character} string representing the type of
#' dosage selected for the analyses.
#'
#' @param quantile a single positive \code{numeric} between 0 and 0.5
#' indicating the quantile used to select the organoids. Default: \code{1/3}.
#'
#' @return the value \code{0L} when successful.
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Validation should return OL as all parameters are valids
#' OrganoidsToolBox:::validateSelectOrgForOneDrug(drugScreening=drugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom S4Vectors isSingleNumber
#' @encoding UTF-8
#' @keywords internal
validateSelectOrgForOneDrug <- function(drugScreening, drugName, study,
                                            screenType, doseType, quantile) {

    if (!is.data.frame(drugScreening)) {
        stop("The \'drugScreening\' must be a data.frame.")
    }

    ## Check for mandatory columns in drugScreening
    if (!all(c('organoid_id', 'timestamp', 'study', 'screen_type',
               'dosage_type', 'drug_a', 'drug_b', 'drug_c', 'drug_background',
               'relative_auc') %in% colnames(drugScreening))) {
        stop("Mandatory columns are missing from the drug screening ",
             "dataset. The mandatory columns are: \'organoid_id\', ",
             "\'timestamp\', \'study\', \'screen_type\', \'dosage_type\', ",
             "\'drug_a\', \'drug_b\', \'drug_c\', \'drug_background\' and ",
             "\'relative_auc\'.")
    }

    ## The drugName parameter must be a single character string
    if (!(is.character(drugName) && length(drugName) == 1)) {
        stop("The \'drugName\' must be a single character string.")
    }

    ## The study parameter must be a vector of character strings
    if (!(is.character(study))) {
        stop("The \'study\' must be a vector of character strings.")
    }

    ## The screenType parameter must be a vector of character strings
    if (!(is.character(screenType))) {
        stop("The \'screenType\' must be a vector of character strings.")
    }

    ## The doseType parameter must be a single character string
    if (!(is.character(doseType) && length(doseType) == 1)) {
        stop("The \'doseType\' must be a single character string.")
    }

    ## The quantile must be a single positive numeric between 0 and 0.5
    if (!(isSingleNumber(quantile) && quantile > 0.0 && quantile < 0.5)) {
        stop("The \'quantile\' must be a single positive numeric between",
                " 0 and 0.5.")
    }

    return(0L)
}


#' @title Validate input parameters for the
#' selectOrgWithoutReplicateForOneDrug() function
#'
#' @description The function validates the input parameters for the
#' selectOrgWithoutReplicateForOneDrug() function.
#'
#' @param drugScreening a single \code{character} string representing the path
#' and name of the drug screening file that contains the information needed
#' to run the organoid selection. Those columns are mandatory: 'organoid_id',
#' 'timestamp', 'study', 'screen_type', 'dosage_type',
#' 'drug_a', 'drug_b', 'drug_c', 'drug_background' and 'relative_auc'.
#'
#' @param drugName a single \code{character} string representing the name of
#' the drug selected for the analyses.
#'
#' @param study a single \code{character} string representing the name of
#' the study selected for the analyses.
#'
#' @param screenType a \code{vector} of \code{character} string representing
#' the type of
#' screening selected for the analyses.
#'
#' @param patientInfo a \code{data.frame} containing the meta-data information
#' related to the organoids. The mandatory columns are: 'organoid_id'
#' and 'patient_id'.
#'
#' @param doseType a single \code{character} string representing the type of
#' dosage selected for the analyses.
#'
#' @param quantile a single positive \code{numeric} between 0 and 0.5
#' indicating the quantile used to select the organoids. Default: \code{1/3}.
#'
#' @return the value \code{0L} when successful.
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Load patient information dataset
#' data(patientInfo)
#'
#' ## Validation should return OL as all parameters are valids
#' OrganoidsToolBox:::validateSelectOrgWithoutReplicateForOneDrug(
#'     drugScreening=drugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     patientInfo=patientInfo, doseType="Averaged", quantile=1/3)
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom S4Vectors isSingleNumber
#' @encoding UTF-8
#' @keywords internal
validateSelectOrgWithoutReplicateForOneDrug<- function(drugScreening,
    drugName, study, screenType, patientInfo, doseType, quantile) {

    ## Validate all but patientInfo parameter
    validateSelectOrgForOneDrug(drugScreening=drugScreening, drugName=drugName,
        study=study, screenType=screenType, doseType=doseType,
        quantile=quantile)

    if (!is.data.frame(patientInfo)) {
        stop("The \'patientInfo\' must be a data.frame.")
    }

    ## Check for mandatory columns in patientInfo
    if (!all(c('organoid_id', 'patient_id') %in% colnames(patientInfo))) {
        stop("Mandatory columns are missing from the patient info ",
            "dataset. The mandatory columns are: \'organoid_id\' and ",
            "\'patient_id\'.")
    }

    return(0L)
}
