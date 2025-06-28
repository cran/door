#' Mock Raw Data
#'
#' A mock data that contains examples of a raw dataset of DOOR outcomes, treatment
#' information, and DOOR components
#'
#' @name mock_raw_data
#' @usage data(mock_raw_data)
#' @format A data frame with 55 observations with the following columns
#' \describe{
#'   \item{ARM, Arm text}{Codes and labels of treatment arm}
#'   \item{DOOR, DOORtext}{Codes and labels of DOOR outcome}
#'   \item{clinical failure, clinical failure text}{Codes and labels for one of the DOOR components}
#'   \item{infectious complications, infectious complications text}{Codes and labels for one of the DOOR components}
#'   \item{death, death text}{Codes and labels for one of the DOOR components}
#'   \item{Weight}{IPW weights}
#'   \item{Duration}{Tie breaker}
#' }
#'
"mock_raw_data"
