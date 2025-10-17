#' PHEC (Dec 2016) Generalised Clipped Boundaries in England
#'
#' This data contains the digital vector boundaries for Public Health England Centres
#' in England as at December 2016. The boundaries are generalised (20m) - clipped to
#' the coastline (Mean High Water mark). Contains both Ordnance Survey and ONS Intellectual
#' Property Rights.
#'
#' @source
#' Source: Office for National Statistics licensed under the Open Government Licence v.3.0
#' Contains OS data © Crown copyright and database right 2022
#' https://open-geography-portalx-ons.hub.arcgis.com/datasets/ons::phec-dec-2016-generalised-clipped-boundaries-in-england/explore
#'
#'
#' @format A dataset with variables:
#' \describe{
#'   \item{OBJECTID}{}
#'   \item{phec16cd}{Public Health England Centre code}
#'   \item{phec16nm}{Public Health England Centre name}
#'   \item{bng_e}{}
#'   \item{bng_n}{}
#'   \item{long}{}
#'   \item{lat}{}
#'   \item{GlobalID}{}
#'   \item{SHAPE_Length}{}
#'   \item{SHAPE_Area}{}
#'   \item{geometry}{}
#' }
#' @keywords internal
"PHEC_boundaries_2016"

#' Local Authority Districts (May 2023) Boundaries for London BGC
#'
#' This data contains the digital vector boundaries for Local Authority Districts,
#' in London, United Kingdom, as at May 2023. The boundaries are generalised (20m) - clipped to
#' the coastline (Mean High Water mark). Contains both Ordnance Survey and ONS Intellectual
#' Property Rights.
#'
#' @source
#' Source: Office for National Statistics licensed under the Open Government Licence v.3.0
#' Contains OS data © Crown copyright and database right 2023
#'
#' Data is a subset of the following, including only Local Authority Districts in
#' Inner London or Outer London.
#' https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2023-boundaries-uk-bgc/explore
#'
#'
#' @format A dataset with variables:
#' \describe{
#'   \item{FID}{}
#'   \item{LAD23CD}{Local Authority District code 2023}
#'   \item{LAD23NM}{Local Authority District name 2023}
#'   \item{LAD23NMW}{Local Authority District name 2023 - Welsh}
#'   \item{BNG_E}{}
#'   \item{BNG_N}{}
#'   \item{LONG}{}
#'   \item{LAT}{}
#'   \item{GlobalID}{}
#'   \item{SHAPE_Length}{}
#'   \item{SHAPE_Area}{}
#'   \item{geometry}{}
#' }
#' @keywords internal
"London_LA_boundaries_2023"

#' Countries (December 2023) Boundaries UK BUC
#'
#' This data contains the digital vector boundaries for  Countries, in the
#' United Kingdom, as at December 2023. The boundaries are ultra generalised (500m)
#' - clipped to the coastline (Mean High Water mark). Contains both Ordnance Survey
#' and ONS Intellectual Property Rights.
#'
#' @source
#' Source: Office for National Statistics licensed under the Open Government Licence v.3.0
#' Contains OS data © Crown copyright and database right 2023
#' https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2023-boundaries-uk-buc/explore
#'
#'
#' @format A dataset with variables:
#' \describe{
#'   \item{FID}{}
#'   \item{CTRY23CD}{Country code 2023}
#'   \item{CTRY23NM}{Country name 2023}
#'   \item{CTRY23NMW}{Country name 2023 - Welsh}
#'   \item{BNG_E}{}
#'   \item{BNG_N}{}
#'   \item{LONG}{}
#'   \item{LAT}{}
#'   \item{GlobalID}{}
#'   \item{SHAPE_Length}{}
#'   \item{SHAPE_Area}{}
#'   \item{geometry}{}
#' }
#' @keywords internal
"UK_boundaries_2023"
