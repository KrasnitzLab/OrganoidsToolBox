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
#' @param study a \code{vector} of \code{character} string representing the
#' name(s) of the study selected for the analyses. The study must be present
#' in the drug screening dataset. The study can be found in the 'study' column
#' of the drug screening dataset.
#'
#' @param screenType a \code{vector} of \code{character} string representing
#' the type of
#' screening selected for the analyses. The type must be present in the drug
#' screening dataset. The screen type can be found in the 'screen_type'
#' column of the drug screening dataset.
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
#' results <- getClassOneDrug(drugScreening=drugScreening,
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
getClassOneDrug <- function(drugScreening, drugName, study,
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
    if (!any(tolower(study) %in% tolower(unique(drugScreening$study)))) {
        stop("The study \'", study, "\' is not present in the drug ",
                "screening dataset.")
    }

    ## The screenType must be present in the drug dataset
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
    selectedDrugData <- filterDrugScreenOneDrug(drugData=drugScreening,
        drugName=drugName, study=study, screenType=screenType,
        doseType=doseType)

    results <- findQuantileOneDrug(cleanDrugData=selectedDrugData,
                                    quantile=quantile)

    # Return a list marked as an DrugAUCQuantile class
    class(results) <- "DrugAUCQuantile"

    return(results)
}


#' @title Select the TODO
#'
#' @description The function TODO
#'
#' @param drugScreening a \code{data.frame} that contains the drug screening
#' results. The column is 'organoid_id' is mandatory.
#'
#' @param patientInfo a \code{data.frame} containing the meta-data information
#' related to the organoids. The mandatory columns are: 'organoid_id'
#' and 'patient_id'.
#'
#' @return a \code{data.frame} with the drug screening results filtered
#' to contain with only one organoid per patient. The extra columns from the
#' patient information dataset is added as extra columns to the final dataset.
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Load patient information dataset for 1 drug
#' data(patientInfo)
#'
#' ## Set seed to get reproducible results
#' set.seed(1212)
#'
#' results <- selectNoReplicateOrganoids(drugScreening=drugScreening,
#'     patientInfo=patientInfo)
#'
#' ## TODO
#' results
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @encoding UTF-8
#' @export
selectNoReplicateOrganoids <- function(drugScreening, patientInfo) {

    ## Validate input types
    validateSelectNoReplicateOrganoids(drugScreening=drugScreening,
                                                    patientInfo=patientInfo)

    ## All organoids should have an associated patient information
    if (!all(unique(drugScreening$organoid_id) %in%
             patientInfo$organoid_id)) {
        stop("Not all organoids have an associated patient information.")
    }

    merged_data <- merge(drugScreening, patientInfo, by="organoid_id",
                            all=FALSE)

    shuffled_data <- merged_data[sample(1:nrow(merged_data), replace=FALSE), ]

    noReplicate <-  shuffled_data[!duplicated(shuffled_data$patient_id), ]

    noReplicate <- noReplicate[order(noReplicate$organoid_id), ]

    rownames(noReplicate) <- noReplicate$organoid_id

    return(noReplicate)
}


#' @title Create a density plot using the relative AUC and the sensitivity
#' classes (SENSITIVE, AVERAGE, RESISTANT) of the organoids for a specific
#' drug.
#'
#' @description The function generates a density plot using the relative AUC
#' and the sensitivity class information present in a \code{DrugAUCQuantile}"
#' object. The function uses [ggplot2::ggplot][ggplot2::ggplot()] function and
#' returns a "\code{ggplot}" object. The density plot can be split by
#' sensitivity classes.
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the sensitive and resistant information for organoids associated
#' to a specific drug screening.
#'
#' @param byGroup a \code{logical} indicating if the density is split
#' by group. Default: \code{FALSE}.
#'
#' @return a \code{ggplot} object that represents a density plot of the
#' AUC according to the drug sensitivity classification(SENSITIVE, AVERAGE,
#' RESISTANT).
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
#' ## Create a density plot where drug sensitivy groups are split
#' densityGraph <- plotDrugAUCDensityCurve(results, byGroup=TRUE)
#' densityGraph
#'
#' @author Astrid Deschênes, Pascal Belleau
#' @importFrom ggplot2 geom_density scale_fill_manual geom_density
#' theme_minimal theme xlab ylab geom_rug element_line
#' @importFrom ggridges geom_density_ridges position_points_jitter
#' @encoding UTF-8
#' @export
plotDrugAUCDensityCurve <- function(drugQuantile, byGroup=FALSE) {

    ## Validate that the drugQuantile parameter is a DrugAUCQuantile object
    if (!(is.DrugAUCQuantile(drugQuantile))) {
        stop("The \'drugQuantile\' parameter must be a DrugAUCQuantile ",
                "object.")
    }

    ## Validate that the byGroup is logical
    if (!(is.logical(byGroup))) {
        stop("The \'byGroup\' parameter must be a logical (TRUE or FALSE).")
    }

    aucResults <- drugQuantile$extreme
    aucResults$drug <- drugQuantile$dataset$drug_a
    aucResults$group <- factor(aucResults$group, levels=c("SENSITIVE",
                                        "AVERAGE", "RESISTANT"))

    colorsR <- c("RESISTANT"="red2", "AVERAGE"="darkgray",
                    "SENSITIVE"="blue3")
    colorsR2 <- c("RESISTANT"="pink", "AVERAGE"="lightgray",
                    "SENSITIVE"="lightblue")

    color <- "darkgray"

    if (byGroup) {
        p <- ggplot(aucResults, aes(x=.data$relative_auc, fill=.data$group,
                                    y=.data$group, colour=.data$group)) +
            geom_density_ridges(jittered_points=TRUE,
                position=position_points_jitter(width=0.05, height=0),
                point_shape='|', point_size=6, point_alpha=1, alpha=0.7) +
            scale_colour_manual(name="Group", values=colorsR) +
            scale_fill_manual(name="Group", values=colorsR2) +
            xlab("Relative AUC") + ylab("Density") +
            theme_minimal() +
            theme(axis.title=element_text(size=13, face="bold"),
                    axis.text=element_text(size=12, face="bold"),
                    legend.text=element_text(size=12),
                    axis.line=element_line(color="black"),
                    axis.ticks=element_line(color="black"),
                    legend.title=element_text(size=13, face="bold"))

    } else {
        p <- ggplot(aucResults, aes(x=.data$relative_auc)) +
            geom_density(fill=color, color=color, alpha=0.7) +
            geom_rug(aes(x=.data$relative_auc, y=0, color=.data$group),
                size=1.1, alpha=0.7,
                position=position_jitter(height=0)) +
            scale_colour_manual(name="Group", values=colorsR) +
            xlab("Relative AUC") + ylab("Density") +
            theme_minimal() +
            theme(axis.title=element_text(size=13, face="bold"),
                axis.text=element_text(size=12, face="bold"),
                legend.text=element_text(size=12),
                axis.line=element_line(color="black"),
                axis.ticks=element_line(color="black"),
                legend.title=element_text(size=13, face="bold"))
    }

    return(p)
}


#' @title Create a violin plot using the relative AUC and the sensitivity
#' classes (SENSITIVE, AVERAGE, RESISTANT) of the organoids for a specific
#' drug.
#'
#' @description The function generates a violin plot using the relative AUC
#' and the sensitivity class information present in a \code{DrugAUCQuantile}"
#' object. The function uses [ggplot2::ggplot][ggplot2::ggplot()] function and
#' returns a "\code{ggplot}" object. The violin tails of the violins can be
#' trimmed to the range of the data.
#'
#' @param drugQuantile an object of class "\code{DrugAUCQuantile}" which
#' contains the
#' sensitive and resistant organoids for a specific drug.
#'
#' @param min a single \code{numeric} representing the minimum value of the
#' y-axis. Default: \code{0}.
#'
#' @param max a single \code{numeric} representing the maximum value of the
#' y-axis. The maximum value must be superior to the minimum value.
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
#' results <- getClassOneDrug(drugScreening=drugScreening,
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
    if (!(is.DrugAUCQuantile(drugQuantile))) {
        stop("The \'drugQuantile\' parameter must be a DrugAUCQuantile",
                " object.")
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

    ## Validate that the trim is logical
    if (!(is.logical(trim))) {
        stop("The \'trim\' parameter must be a logical (TRUE or FALSE).")
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
            legend.title=element_text(size=13, face="bold"))

    return(p)
}
