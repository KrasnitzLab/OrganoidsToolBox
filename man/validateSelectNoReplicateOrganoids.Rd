% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drugScreening-internal.R
\encoding{UTF-8}
\name{validateSelectNoReplicateOrganoids}
\alias{validateSelectNoReplicateOrganoids}
\title{Validate input parameters for the
selectNoReplicateOrganoids() function}
\usage{
validateSelectNoReplicateOrganoids(drugScreening, patientInfo)
}
\arguments{
\item{drugScreening}{a \code{data.frame} containing the drug screening
information. The 'organoid_id' column is mandatory.}

\item{patientInfo}{a \code{data.frame} containing the meta-data information
related to the organoids. The mandatory columns are: 'organoid_id'
and 'patient_id'.}
}
\value{
the value \code{0L} when successful.
}
\description{
The function validates the input parameters for the
selectNoReplicateOrganoids() function.
}
\examples{

## Load drug screen dataset for 1 drug
data(drugScreening)

## Load patient information dataset
data(patientInfo)

## Validation should return OL as all parameters are valids
OrganoidsToolBox:::validateSelectNoReplicateOrganoids(
    drugScreening=drugScreening, patientInfo=patientInfo)

}
\author{
Astrid DeschĂȘnes, Pascal Belleau
}
\keyword{internal}
