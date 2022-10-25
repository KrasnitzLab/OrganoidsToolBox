#' OrganoidsToolBox: TODO
#'
#' To learn more about \pkg{OrganoidsToolBox} package see:
#' \url{https://github.com/KrasnitzLab/OrganoidsToolBox/wiki}
#'
#' @docType package
#'
#' @name OrganoidsToolBox-package
#'
#' @aliases OrganoidsToolBox-package OrganoidsToolBox
#'
#' @author
#'
#' Maintainer:
#' Astrid Deschênes <adeschen@@hotmail.com>
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} {for selecting the organoids
#' with sensitive and resistant behavior for a specific drug screening.}
#' }
#' @encoding UTF-8
#' @keywords package
NULL


#' Simple demo drug screening for one drug (Methotrexate). The results have
#' been generated for demonstration purpose. There is only one study in the
#' dataset. There is also only one type of screening in the dataset. Some
#' entries have a value for a second drug. There is not entry for a third
#' drug. There is no background drug.
#'
#' @name simpleDrugScreening
#'
#' @docType data
#'
#' @aliases simpleDrugScreening
#'
#' @format a \code{data.frame} with the following columns:
#' \itemize{
#' \item \code{organoid_id} { a \code{character} string representing the
#' organoid identifier. }
#' \item \code{plate} { a \code{character} string representing the plate
#' identifier. }
#' \item \code{passage_number} { a \code{character} string representing the
#' passage number of the organoid. }
#' \item \code{timestamp} { a \code{character} string representing the date
#' when the screening was done. }
#' \item \code{study} { a \code{character} string representing the study. }
#' \item \code{screen_type} { a \code{character} string representing the
#' screen type. }
#' \item \code{z_prime} { a \code{numeric} representing the assay quality. }
#' \item \code{drug_a} { a \code{character} string representing the name of
#' the first drug used for the drug screen. There is at least one drug used
#' in a drug screening.}
#' \item \code{drug_b} { a \code{character} string representing the name of
#' the second drug used for the drug screen. When no second drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_c} {  a \code{character} string representing the name of
#' the third drug used for the drug screen. When no third drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_background} { a \code{character} string representing the
#' name of
#' the drug used as background for the drug screen. When no drug has been used
#' as background, the value is \code{'N/A'}. }
#' \item \code{dosage_type} { a \code{character} string representing the type
#' of dosage. }
#' \item \code{dosage_concentration_a} { a \code{numeric}
#' representing the dosage of
#' the first drug used for the drug screen. This column should always have a
#' value as there is at least one drug used in a drug screening.}
#' \item \code{dosage_concentration_b} { a \code{numeric}
#' representing the dosage of the second drug used for the drug screen.
#' When no second drug has been used, the value is \code{0}.}
#' \item \code{dosage_concentration_c} { a \code{numeric}
#' representing the dosage of the third drug used for the drug screen.
#' When no third drug has been used, the value is \code{0}. }
#' \item \code{dosage_concentration_background} { a \code{numeric}
#' representing the dosage of the background drug used for the drug screen.
#' When no background drug has been used, the value is \code{0}.  }
#' \item \code{percent_viability} { a \code{numeric}
#' representing the percentage of viability of the organoids under the
#' specific screening conditions. }
#' \item \code{relative_auc} { a \code{numeric}
#' representing the relative AUC (area under the curve) for the specified
#' organoids under specific conditions.  The AUC is calculated using the
#' percentage of viability under different drug concentrations. So, the same
#' relative AUC is repeated on multiple lines.}
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} { for selecting the organoids
#' with sensitive and resistant behavior for a
#' specific drug screening.}
#' }
#'
#' @usage data(simpleDrugScreening)
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(simpleDrugScreening)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- getClassOneDrug(drugScreening=simpleDrugScreening,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#' ## The information of the extreme organoids is found it the 'extreme' entry
#' results$extreme
#'
#'
NULL


#' Simple demo drug screening for one drug (Methotrexate). The results have
#' been generated for demonstration purpose. There is two studies present in
#' the dataset. There is also only two types of screening in the dataset. Some
#' entries have a value for a second drug. There is not entry for a third
#' drug. There is no background drug. At last, there is two types of dosage.
#'
#' @name drugScreening
#'
#' @docType data
#'
#' @aliases drugScreening
#'
#' @format a \code{data.frame} with the following columns:
#' \itemize{
#' \item \code{organoid_id} { a \code{character} string representing the
#' organoid identifier. }
#' \item \code{plate} { a \code{character} string representing the plate
#' identifier. }
#' \item \code{passage_number} { a \code{character} string representing the
#' passage number of the organoid. }
#' \item \code{timestamp} { a \code{character} string representing the date
#' when the screening was done. }
#' \item \code{study} { a \code{character} string representing the study. }
#' \item \code{screen_type} { a \code{character} string representing the
#' screen type. }
#' \item \code{z_prime} { a \code{numeric} representing the assay quality. }
#' \item \code{drug_a} { a \code{character} string representing the name of
#' the first drug used for the drug screen. There is at least one drug used
#' in a drug screening.}
#' \item \code{drug_b} { a \code{character} string representing the name of
#' the second drug used for the drug screen. When no second drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_c} {  a \code{character} string representing the name of
#' the third drug used for the drug screen. When no third drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_background} { a \code{character} string representing the
#' name of
#' the drug used as background for the drug screen. When no drug has been used
#' as background, the value is \code{'N/A'}. }
#' \item \code{dosage_type} { a \code{character} string representing the type
#' of dosage. }
#' \item \code{dosage_concentration_a} { a \code{numeric}
#' representing the dosage of
#' the first drug used for the drug screen. This column should always have a
#' value as there is at least one drug used in a drug screening.}
#' \item \code{dosage_concentration_b} { a \code{numeric}
#' representing the dosage of the second drug used for the drug screen.
#' When no second drug has been used, the value is \code{0}.}
#' \item \code{dosage_concentration_c} { a \code{numeric}
#' representing the dosage of the third drug used for the drug screen.
#' When no third drug has been used, the value is \code{0}. }
#' \item \code{dosage_concentration_background} { a \code{numeric}
#' representing the dosage of the background drug used for the drug screen.
#' When no background drug has been used, the value is \code{0}.  }
#' \item \code{percent_viability} { a \code{numeric}
#' representing the percentage of viability of the organoids under the
#' specific screening conditions. }
#' \item \code{relative_auc} { a \code{numeric}
#' representing the relative AUC (area under the curve) for the specified
#' organoids under specific conditions.  The AUC is calculated using the
#' percentage of viability under different drug concentrations. So, the same
#' relative AUC is repeated on multiple lines.}
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} { for selecting the organoids
#' with sensitive and resistant behavior for a
#' specific drug screening.}
#' }
#'
#' @usage data(drugScreening)
#'
#' @keywords datasets
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
#'     doseType="Averaged", quantile=1/4)
#'
#' ## The information of the extreme organoids is found it the 'extreme' entry
#' results$extreme
#'
#'
NULL




#' Simple demo drug screening for one drug (Methotrexate). The results have
#' been generated for demonstration purpose. There is two studies present in
#' the dataset. There is also only two types of screening in the dataset. Some
#' entries have a value for a second drug. There is not entry for a third
#' drug. There is no background drug. At last, there is two types of dosage.
#'
#' @name drugScreeningMethoSet
#'
#' @docType data
#'
#' @aliases drugScreeningMethoSet
#'
#' @format a \code{data.frame} with the following columns:
#' \itemize{
#' \item \code{organoid_id} { a \code{character} string representing the
#' organoid identifier. }
#' \item \code{timestamp} { a \code{character} string representing the date
#' when the screening was done. }
#' \item \code{study} { a \code{character} string representing the study. }
#' \item \code{screen_type} { a \code{character} string representing the
#' screen type. }
#' \item \code{drug_a} { a \code{character} string representing the name of
#' the first drug used for the drug screen. There is at least one drug used
#' in a drug screening.}
#' \item \code{drug_b} { a \code{character} string representing the name of
#' the second drug used for the drug screen. When no second drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_c} {  a \code{character} string representing the name of
#' the third drug used for the drug screen. When no third drug has been
#' used, the value is \code{'N/A'}.  }
#' \item \code{drug_background} { a \code{character} string representing the
#' name of
#' the drug used as background for the drug screen. When no drug has been used
#' as background, the value is \code{'N/A'}. }
#' \item \code{dosage_type} { a \code{character} string representing the type
#' of dosage. }
#' \item \code{relative_auc} { a \code{numeric}
#' representing the relative AUC (area under the curve) for the specified
#' organoids under specific conditions.  The AUC is calculated using the
#' percentage of viability under different drug concentrations. So, the same
#' relative AUC is repeated on multiple lines.}
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} { for selecting the organoids
#' with sensitive and resistant behavior for a
#' specific drug screening.}
#' }
#'
#' @usage data(drugScreeningMethoSet)
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## Load drug screen dataset and patient information for methotrexate
#' data(drugScreeningMethoSet)
#' data(patientInfoMethoSet)
#'
#' ## Retain unreplicated samples
#' cleanData <- selectNoReplicateOrganoids(drugScreening=drugScreeningMethoSet,
#'     patientInfo=patientInfoMethoSet)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- getClassOneDrug(drugScreening=cleanData,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/3)
#'
#' ## The information of the extreme organoids is found it the 'extreme' entry
#' head(results$extreme)
#'
#' ## Fisher test on ancestry
#' fisherT <- fisherCategoricalVariable(drugQuantile=results,
#'     category="ancestry")
#'
#' fisherT
#'
NULL


#' Simple demo patient information dataset. The information is related to the
#' organoids present in the 'drugScreening' dataset.
#'
#' @name patientInfo
#'
#' @docType data
#'
#' @aliases patientInfo
#'
#' @format a \code{data.frame} with the following columns:
#' \itemize{
#' \item \code{organoid_id} { a \code{character} string representing the
#' organoid identifier. }
#' \item \code{patient_id} { a \code{character} string representing the patient
#' identifier. }
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} { for selecting the samples
#' with sensitive and resistant behavior for a
#' specific drug screening dataset.}
#' \item \code{\link{selectNoReplicateOrganoids}} { for selecting
#' the unrelated samples (only one sample per patient) from a drug screening
#' dataset.}
#' \item \code{\link{fisherCategoricalVariable}} { for running Fisher tests
#' in the sensitive and resistant groups using a categorical value
#' from a drug screening dataset.}
#' }
#'
#' @usage data(patientInfo)
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## Load drug screen dataset for 1 drug
#' data(drugScreening)
#'
#' ## Load patient information dataset for 1 drug
#' data(patientInfo)
#'
#' ## Select unrelated organoids in the drug screening
#' results <- selectNoReplicateOrganoids(drugScreening=drugScreening,
#'     patientInfo=patientInfo)
#'
#' ## The drug screen dataset has been filtered
#' results
#'
#'
NULL


#' Simple demo patient information dataset. The information is related to the
#' organoids present in the 'drugScreening' dataset.
#'
#' @name patientInfoMethoSet
#'
#' @docType data
#'
#' @aliases patientInfoMethoSet
#'
#' @format a \code{data.frame} with the following columns:
#' \itemize{
#' \item \code{organoid_id} { a \code{character} string representing the
#' organoid identifier. }
#' \item \code{patient_id} { a \code{character} string representing the patient
#' identifier. }
#' \item \code{ancestry} { a \code{character} string representing the patient
#' genetic ancestry. }
#' \item \code{BMI} { a \code{character} string representing the patient
#' BMI class. }
#' }
#'
#' @seealso
#' \itemize{
#' \item \code{\link{getClassOneDrug}} { for selecting the samples
#' with sensitive and resistant behavior for a
#' specific drug screening dataset.}
#' \item \code{\link{selectNoReplicateOrganoids}} { for selecting
#' the unrelated samples (only one sample per patient) from a drug screening
#' dataset.}
#' \item \code{\link{fisherCategoricalVariable}} { for running Fisher tests
#' in the sensitive and resistant groups using a categorical value
#' from a drug screening dataset.}
#' }
#'
#' @usage data(patientInfoMethoSet)
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## Load drug screen dataset for methotrexate dataset
#' data(drugScreening)
#'
#' ## Load patient information dataset for methotrexate dataset
#' data(patientInfoMethoSet)
#'
#' ## Retain unreplicated samples
#' cleanData <- selectNoReplicateOrganoids(drugScreening=drugScreeningMethoSet,
#'     patientInfo=patientInfoMethoSet)
#'
#' ## Calculate the extreme organoids for the methotrexate drug screening
#' ## using a quantile of 1/3
#' results <- getClassOneDrug(drugScreening=cleanData,
#'     drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
#'     doseType="Averaged", quantile=1/4)
#'
#' ## The information of the extreme organoids is found it the 'extreme' entry
#' head(results$extreme)
#'
#' ## Fisher test on BMI
#' fisherT <- fisherCategoricalVariable(drugQuantile=results, category="BMI")
#'
#' fisherT
#'
#'
NULL
