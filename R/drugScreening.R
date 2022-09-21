
#' @title Select the organoids with sensitive and resistant behavior for a
#' specific drug screening
#'
#' @description The function selects the organoids within the low and upper
#' quantile, as specified by user, for a specific drug screening.
#'
#' @param drugScreening a single \code{character} string representing the path
#' and name of the drug screening file that contains the information needed
#' to run the organoid selection.
#'
#' @param drugName a single \code{character} string representing the name of
#' the drug selected for the analyses. The drug must be present in the drug
#' screening dataset. The drug name can be found in the 'drug_a' column of the
#' drug screening dataset.
#'
#' @param study a single \code{character} string representing the name of
#' the study selected for the analyses. The study must be present in the drug
#' screening dataset. The study can be found in the 'study' column of the
#' drug screening dataset.
#'
#' @param screenType a \code{vector} of \code{character} string representing
#' the type of
#' screening selected for the analyses. The type must be present in the drug
#' screening dataset.  The screenType can be found in the 'screen_type'
#' column of the
#' drug screening dataset.
#'
#' @param doseType a single \code{character} string representing the type of
#' dosage selected for the analyses. The type must be present in the drug
#' screening dataset.
#'
#' @param quantile a single positive \code{numeric} between 0 and 0.5
#' indicating the quantile used to select the organoids. Default: \code{1/3}.
#'
#' @return a TODO
#'
#' @examples
#'
#' ## TODO
#' drugName <- "Methotrexate"
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom S4Vectors isSingleNumber
#' @encoding UTF-8
#' @export
selectOrgForOneDrug <- function(drugScreening, drugName, study,
            screenType, doseType="Averaged", quantile=1/3) {

    if (!is.data.frame(drugScreening)) {
        stop("The \'drugScreening\' must be a data.frame.")
    }

    ## Check for mandatory columns in drugScreening
    if (!all(c('organoid_id', 'timestamp', 'study', 'screen_type',
                'dosage_type', 'drug_a', 'drug_b', 'drug_c',
                'drug_background', 'relative_auc') %in%
              colnames(drugScreening))) {
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

    results <- findOneDrugQuantile(drugData=drugScreening, drugName=drugName,
                        doseType="Averaged", quantile=quantile)

    return(0L)
}
