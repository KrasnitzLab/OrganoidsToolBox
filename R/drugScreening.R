#' @title Select the organoids with sensitive and resistant behavior for a
#' specific drug screening
#'
#' @description The function selects the organoids within the low and upper
#' quantile, as specified by user, for a specific drug screening.
#'
#' @param drugScreening a \code{data.frame} that contains the drug screening
#' results. The mandatory columns: 'organoid_id',
#' 'timestamp', 'study', 'screen_type', 'dosage_type',
#' 'drug_a', 'drug_b', 'drug_c', 'drug_background' and 'relative_auc'.
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
#' screening dataset. The screen type can be found in the 'screen_type'
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
#' @return an object of class "\code{DrugAUCQuantile}" which contains the
#' sensitive and resistant organoids for a specific drug. This object is a
#' \code{list} with the following 3 components:
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
#' data(drugScreening)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- selectOrgForOneDrug(drugScreening=drugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#' ## The classification of the organoids is in the 'extreme' entry
#' results$extreme
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom S4Vectors isSingleNumber
#' @encoding UTF-8
#' @export
selectOrgForOneDrug <- function(drugScreening, drugName, study,
            screenType, doseType="Averaged", quantile=1/3) {

    ## Validate input types
    validateSelectOrgForOneDrug(drugScreening=drugScreening, drugName=drugName,
        study=study, screenType=screenType, doseType=doseType,
        quantile=quantile)

    ## The drug must be present in the drug dataset
    if (!(tolower(drugName) %in% tolower(unique(drugScreening$drug_a)))) {
        stop("The drug \'", drugName, "\' is not present in the drug ",
             "screening dataset.")
    }

    ## The study must be present in the drug dataset
    if (!(tolower(study) %in% tolower(unique(drugScreening$study)))) {
        stop("The study \'", study, "\' is not present in the drug ",
                "screening dataset.")
    }

    ## The study must be present in the drug dataset
    if (!any(tolower(screenType) %in%
          tolower(unique(drugScreening$screen_type)))) {
        stop("The screen type \'", screenType, "\' is not present in the ",
                    "drug screening dataset.")
    }

    ## The doseType must be present in the drug dataset
    if (!(tolower(doseType) %in%
          tolower(unique(drugScreening$dosage_type)))) {
        stop("The dossage type \'", doseType, "\' is not present in the ",
                "drug screening dataset.")
    }

    ## Select the specified study
    selectedDrugData <- drugScreening[which(tolower(drugScreening$study) ==
                                                tolower(study)), ]

    ## Select the specified screen type
    selectedDrugData <- selectedDrugData[
        which(tolower(selectedDrugData$screen_type) %in% tolower(screenType)), ]

    results <- findOneDrugQuantile(drugData=selectedDrugData,
                        drugName=drugName, doseType="Averaged",
                        quantile=quantile)

    # Return a list marked as an DrugAUCQuantile class
    class(results) <- "DrugAUCQuantile"

    return(results)
}


#' @title TODO
#'
#' @description The function TODO
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the
#' sensitive and resistant organoids for a specific drug.
#'
#' @return TODO
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
plotDrugAUCDensityCurve <- function(drugQuantile) {

    ## Validate that the drugQuantile parameter is a DrugAUCQuantile object
    if (!is.DrugAUCQuantile(drugQuantile)) {
        stop("\'drugQuantile\' must be a DrugAUCQuantile object.")
    }

}


#' @title TODO
#'
#' @description The function TODO
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the
#' sensitive and resistant organoids for a specific drug.
#'
#' @param min a single \code{numeric} representing the minimum value of the
#' y-axis. Default: \code{0}.
#'
#' @param max a single \code{numeric} representing the maximum value of the
#' y-axis. The maximum value must be superior to the mimimum value.
#' Default: \code{100}.
#'
#' @param trim a \code{logical} indicating if the tails of the violins
#' are trimmed to the range of the data. Default: \code{FALSE}.
#'
#' @return a \code{ggplot} object for a violin plot built with the specified
#' data.
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- selectOrgForOneDrug(drugScreening=drugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#' ## Plot results
#' p <- plotDrugAUCViolinPlot(drugQuantile=results, min=20, max=80, trim=TRUE)
#' plot(p)
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom ggplot2 ggplot geom_violin ylim ylab theme_minimal aes .data
#' position_jitter geom_point xlab geom_hline scale_colour_manual theme
#' element_text
#' @importFrom S4Vectors isSingleNumber
#' @encoding UTF-8
#' @export
plotDrugAUCViolinPlot <- function(drugQuantile, min=0, max=100, trim=FALSE) {

    ## Validate that the drugQuantile parameter is a DrugAUCQuantile object
    if (!is.DrugAUCQuantile(drugQuantile)) {
        stop("The \'drugQuantile\' parameter must be a DrugAUCQuantile object.")
    }

    ## Validate that the min is a single numeric
    if (!isSingleNumber(min)) {
        stop("The \'min\' parameter must be a single numeric.")
    }

    ## Validate that the min is a single numeric
    if (!(isSingleNumber(max) && max > min)) {
        stop("The \'max\' parameter must be a single numeric superio to",
                " the 'min' parameter.")
    }

    aucResults <- drugQuantile$extreme
    aucResults$drug <- drugQuantile$dataset$drug_a

    colorsR <- c("RESISTANT"="red2", "AVERAGE"="darkgray",
                    "SENSITIVE"="blue3")

    p <- ggplot(data=aucResults, aes(x=.data$drug, y=.data$relative_auc)) +
        geom_violin(size=1, colour="gray", trim=trim) +
        geom_point(aes(colour = .data$group), size=3,
                        position = position_jitter(seed = 1, width = 0.15)) +
        scale_colour_manual(name="Group", values=colorsR) +
        geom_hline(yintercept=drugQuantile$quantile$lower, linetype="dashed",
                            color = "blue3", size=0.9) +
        geom_hline(yintercept=drugQuantile$quantile$upper, linetype="dashed",
                            color = "red2", size=0.9) +
        ylab("Relative AUC") + ylim(c(min, max)) + xlab("") +
        theme_minimal() +
        theme(axis.title=element_text(size=13, face="bold"),
            axis.text.x=element_text(size=13, face="bold"),
            legend.text=element_text(size=12),
            legend.title = element_text(size=13, face="bold"))

    return(p)
}
