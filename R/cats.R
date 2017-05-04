#' Cat population
#'
#' Hypothetical owned cat population.
#'
#' @format A data frame with 44233 observations and 15 variables:
#' \describe{
#'   \item{track_id}{Census track ID from Santos, Brazil.}
#'   \item{hh_id}{Household ID.}
#'   \item{name}{Cat's name.}
#'   \item{sex}{Cat's sex.}
#'   \item{age}{Cat's age. An age equal to 0 means that the cat has less than 1 year.}
#'   \item{sterilized}{Cat's reproductive status.}
#'   \item{sterilized_last_year}{For sterilized cats, indicates if the cat was sterilized in the previous 12 months.}
#'   \item{free_roaming}{Indicates if the cat has access to the street without supervision.}
#'   \item{acquisition}{Way of acquisition}
#'   \item{acquired_last_year}{Indicates if the cat was acquired in the previous 12 months.}
#'   \item{acquired_sterilized}{Indicates if the cat was sterilized when acquired.}
#'   \item{acquisition_city}{City of acquisition.}
#'   \item{acquisition_state}{State of acquisition.}
#'   \item{turnover_last_year}{Indicates if the cat was acquired in the 12 months following the lost of another cat.}
#'   \item{litter_size_last_year}{Litter size if the queen had the litter in the previous 12 months.}
#' }
"cats"