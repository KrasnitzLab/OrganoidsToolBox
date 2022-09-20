
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
#' screening file.
#'
#' @param study a single \code{character} string representing the name of
#' the study selected for the analyses. The study must be present in the drug
#' screening file.
#'
#' @param type a single \code{character} string representing the type of
#' study selected for the analyses. The type must be present in the drug
#' screening file.
#'
#' @param ancestry a single \code{character} string representing the ancestry
#' of the selected organoids. If \code{"ALL"}, all organoids are used for the
#' selection. Default: \code{"ALL"}.
#'
#' @param ancestry a TODO. If `ancestry` is \code{"ALL"}, this
#' field is not used.
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
            type, ancestry=c("ALL", "AFR", "EUR", "AMR", "EAS", "SAS"),
            quantile=1/3) {

    ## The drugName parameter must be a single character string
    if (!(is.character(drugName) && length(drugName) == 1)) {
        stop("The \'drugName\' must be a single character string.")
    }

    ## The study parameter must be a vector of character string
    if (!(is.character(study))) {
        stop("The \'study\' must be a vector of character strings.")
    }

    ## The type parameter must be a single character string
    if (!(is.character(type) && length(type) == 1)) {
        stop("The \'type\' must be a single character string.")
    }

    ## The quantile must be a single positive numeric between 0 and 0.5
    if (!(isSingleNumber(quantile) && quantile > 0.0 && quantile < 0.5)) {
        stop("The \'quantile\' must be a single positive numeric between",
                    " 0 and 0.5.")
    }


    return(0L)
}
