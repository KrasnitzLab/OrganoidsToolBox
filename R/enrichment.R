#' @title Run Fisher tests in the sensitive and resistant groups
#' using a categorical value
#'
#' @description The function run Fisher test in the sensitive and
#' resistant groups using a categorical value. TODO
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the sensitive and resistant organoids for a specific drug.
#'
#' @param patientInfo  a \code{data.frame} containing the meta-data information
#' related to the organoids. The mandatory columns are: 'organoid_id'
#' and 'patient_id'.
#'
#' @param category a single \code{character} string representing the column
#' from the 'patientInfo' dataset that should be used as the
#' categorical variable.
#'
#' @return a \code{list} containing 2 components:
#' \itemize{
#' \item{Fisher}{ TODO}
#' \item{table}{a \code{data.frame} TODO }
#' }
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- getClassOneDrug(drugScreening=drugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#' ## The classification of the organoids is in the 'extreme' entry
#' results$extreme
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom stats fisher.test
#' @encoding UTF-8
#' @export
fisherCategoricalVariable <- function(drugQuantile, patientInfo, category) {

    ## Validate that the drugQuantile parameter is a DrugAUCQuantile object
    if (!(is.DrugAUCQuantile(drugQuantile) ||
          is.DrugAUCQuantileNoReplicate(drugQuantile))) {
        stop("The \'drugQuantile\' parameter must be a DrugAUCQuantile ",
             "object.")
    }

    if (!is.data.frame(patientInfo)) {
        stop("The \'patientInfo\' parameter must be a data.frame.")
    }

    if (!is.character(category)) {
        stop("The \'category\' parameter must be a character string.")
    }

    ## The drug must be present in the drug dataset
    if (!(category %in% colnames(patientInfo))) {
        stop("The category \'", category, "\' must be one of the columns in ",
             "the \'patientInfo\' dataset.")
    }

    results <- drugQuantile$extreme

    resultsD <- merge(results, patientInfo[, c("organoid_id", category)],
                      by="organoid_id", all.x = TRUE)

    sensitive <- resultsD[resultsD$group == "SENSITIVE",]
    resistant <- resultsD[resultsD$group == "RESISTANT",]

    categoryAll <- unique(resultsD[[category]])

    all <- list()
    for (i in categoryAll) {
        all[[i]] <- data.frame(V1=c(sum(sensitive[[category]] == i),
                                   sum(resistant[[category]] == i)))
        colnames(all[[i]]) <- i
        rownames(all[[i]]) <- c("SENSITIVE", "RESISTANT")
    }

    allTable <- do.call(cbind, all)

    fisherResult <- fisher.test(allTable)

    result <- list()
    result[["Fisher"]] <- fisherResult
    result[["table"]] <- allTable

    return(result)
}
