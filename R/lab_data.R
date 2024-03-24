#' @title Synthetic Lab Data for EpiViz functions
#'
#' @description
#' A dataset containing synthetic lab data for epidemiological visualisation purposes.
#'
#' @name lab_data
#'
#' @docType data
#'
#' @usage data(lab_data)
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{date_of_birth}{Date of birth of the patients.}
#'   \item{sex}{Gender of the patients (Factor with levels: "Female", "Male").}
#'   \item{organism_species_Name}{Organism species name (Factor with levels: "KLEBSIELLA PNEUMONIAE").}
#'   \item{specimen_date}{Date of specimen collection.}
#'   \item{lab_code}{Laboratory codes (Factor with unique levels).}
#'   \item{local_authority_name}{Name of the local authority.}
#'   \item{local_authority_code}{Code of the local authority.}
#'   \item{regions}{Name of UKHSA regions.}
#' }
#'
#' @examples
#' data(lab_data)
#' head(lab_data)
#'
#' @keywords datasets
#'
"lab_data"
