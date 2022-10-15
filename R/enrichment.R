#' @title Run Fisher tests in the sensitive and resistant groups
#' using a categorical value
#'
#' @description The function run Fisher test in the sensitive and
#' resistant groups using a categorical value. TODO
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the sensitive and resistant organoids for a specific drug.
#'
#' @param category a single \code{character} string representing the column
#' from the "\code{DrugAUCQuantile}" extreme dataset that should be used as the
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
#' ## TODO
#' results$extreme
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom stats fisher.test
#' @encoding UTF-8
#' @export
fisherCategoricalVariable <- function(drugQuantile, category) {

    ## Validate that the drugQuantile parameter is a DrugAUCQuantile object
    if (!(is.DrugAUCQuantile(drugQuantile))) {
        stop("The \'drugQuantile\' parameter must be a DrugAUCQuantile ",
                "object.")
    }

    if (!is.character(category)) {
        stop("The \'category\' parameter must be a character string.")
    }

    ## The drug must be present in the drug dataset
    if (!(category %in% colnames(drugQuantile$extreme))) {
        stop("The category \'", category, "\' must be one of the columns in ",
                "the \'DrugAUCQuantile\' dataset.")
    }

    results <- drugQuantile$extreme

    sensitive <- results[results$group == "SENSITIVE",]
    resistant <- results[results$group == "RESISTANT",]

    categoryAll <- unique(results[[category]])

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
