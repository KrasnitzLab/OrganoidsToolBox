#' @title Extract the sensitive and resistant organoids
#' for a specific drug from a drug screening dataset
#'
#' @description The function extract the sensitive and resistant organoids
#' for a specific drug. The organoids with an average AUC equal or inferior to
#' the specified quantile are considered sensitive while the organoids with an
#' average AUC superior or equal to (1 - quantile) are considered resistant.
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
#' \code{0.5} representing the cutoff for the organdois selection.
#'
#' @return a \code{list} containing 3 entries:
#' \itemize{
#' \item{quantile}{the \code{upper} and \code{lower} quantiles for the
#' specified drug.}
#' \item{dataset}{a \code{data.frame} containing the data used to
#' calculate the quantiles. }
#' \item{extreme}{a \code{data.frame} containing the sensitive and
#' resistant organoids according to the specified quantiles. }
#' }
#'
#' @examples
#'
#' ## Path to the demo pedigree file is located in this package
#' data.dir <- system.file("extdata", package="RAIDS")
#'
#' ## TODO
#'
#' @author Pascal Belleau and Astrid Deschênes
#' @importFrom stats quantile
#' @encoding UTF-8
#' @keywords internal
findOneDrugQuantile <- function(drugData, drugName, doseType="Averaged",
                                            quantile) {

    ## Check for mandatory columns in drugData
    if (! all(c('organoid_id', 'timestamp', 'dosage_type', 'drug_a',
                'drug_b', 'drug_c', 'drug_background', 'relative_auc') %in%
              colnames(drugData))) {
        stop("Mandatory columns are missing from the drug screening ",
                "dataset. The mandatory columns are: \'organoid_id\', ",
                "\'timestamp\', \'dosage_type\', \'drug_a\', \'drug_b\', ",
                "\'drug_c\', \'drug_background\' and \'relative_auc\'.")
    }

    ## Select the specified dose type
    orgDR.avr <- drugData[which(drugData$dosage_type == doseType), ]

    ## Select the specified drug
    orgDR.avr <- orgDR.avr[which(drugData$drug_a == drugName &
                                     drugData$drug_b == "N/A" &
                                     drugData$drug_c == "N/A" &
                                     drugData$drug_background == "N/A"), ]

    ## Remove duplicate
    orgDR.avr.u <- orgDR.avr[-1 * which(duplicated(orgDR.avr[,
                                c("organoid_id", "timestamp", "drug_a")])),]

    results <- list()
    results[["quantile"]] <- list()

    ## Calculate upper and lower quantile
    results[["lower"]] <- unname(quantile(orgDR.avr.u$relative_auc, quantile))
    results[["upper"]] <- unname(quantile(orgDR.avr.u$relative_auc,
                                                                1 - quantile))
    results[["dataset"]] <- orgDR.avr.u

    ## Select sensitive organoids
    extreme <- orgDR.avr.u[which(orgDR.avr.u$relative_auc <=
                                            results[["lower"]]),]
    extreme$GROUP <- rep("SENSITIVE", nrow(extreme))

    ## Select resistant organoids
    extreme2 <- orgDR.avr.u[which(orgDR.avr.u$relative_auc >=
                                            results[["upper"]]),]
    extreme2$GROUP <- rep("RESISTANT", nrow(extreme2))

    results[["extreme"]] <- rbind(extreme, extreme2)

    return(results)
}