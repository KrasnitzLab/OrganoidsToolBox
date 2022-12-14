% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OrganoidsToolBox.R
\docType{data}
\name{patientInfoMethoSet}
\alias{patientInfoMethoSet}
\title{Simple demo patient information dataset. The information is related to the
organoids present in the 'drugScreening' dataset.}
\format{
a \code{data.frame} with the following columns:
\itemize{
\item \code{organoid_id} { a \code{character} string representing the
organoid identifier. }
\item \code{patient_id} { a \code{character} string representing the patient
identifier. }
\item \code{ancestry} { a \code{character} string representing the patient
genetic ancestry. }
\item \code{BMI} { a \code{character} string representing the patient
BMI class. }
}
}
\usage{
data(patientInfoMethoSet)
}
\description{
Simple demo patient information dataset. The information is related to the
organoids present in the 'drugScreening' dataset.
}
\examples{

## Load drug screen dataset for methotrexate dataset
data(drugScreening)

## Load patient information dataset for methotrexate dataset
data(patientInfoMethoSet)

## Retain unreplicated samples
cleanData <- selectNoReplicateOrganoids(drugScreening=drugScreeningMethoSet,
    patientInfo=patientInfoMethoSet)

## Calculate the extreme organoids for the methotrexate drug screening
## using a quantile of 1/3
results <- getClassOneDrug(drugScreening=cleanData,
    drugName="Methotrexate", study="MEGA-TEST", screenType="TEST-01",
    doseType="Averaged", quantile=1/4)

## The information of the extreme organoids is found it the 'extreme' entry
head(results$extreme)

## Fisher test on BMI
fisherT <- fisherCategoricalVariable(drugQuantile=results, category="BMI")

fisherT


}
\seealso{
\itemize{
\item \code{\link{getClassOneDrug}} { for selecting the samples
with sensitive and resistant behavior for a
specific drug screening dataset.}
\item \code{\link{selectNoReplicateOrganoids}} { for selecting
the unrelated samples (only one sample per patient) from a drug screening
dataset.}
\item \code{\link{fisherCategoricalVariable}} { for running Fisher tests
in the sensitive and resistant groups using a categorical value
from a drug screening dataset.}
}
}
\keyword{datasets}
