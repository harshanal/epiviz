% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab_data.R
\docType{data}
\name{lab_data}
\alias{lab_data}
\title{Synthetic Lab Data for EpiViz functions}
\format{
A data frame with the following columns:
\describe{
\item{date_of_birth}{Date of birth of the patients.}
\item{sex}{Gender of the patients (Factor with levels: "Female", "Male").}
\item{organism_species_name}{Organism species name (Factor with levels: "KLEBSIELLA PNEUMONIAE").}
\item{specimen_date}{Date of specimen collection.}
\item{lab_code}{Laboratory codes (Factor with unique levels).}
\item{local_authority_name}{Name of the local authority.}
\item{local_authority_code}{Code of the local authority.}
\item{region}{Name of UKHSA regions.}
}
}
\usage{
data(lab_data)
}
\description{
A dataset containing synthetic lab data for epidemiological visualisation purposes.
}
\examples{
data(lab_data)
head(lab_data)

}
\keyword{datasets}
