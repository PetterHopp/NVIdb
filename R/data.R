#' @title Data: Variables per PJS-level.
#'
#' @description A data frame with the variable names (column names) in PJS and their corresponding PJS-level. The column names
#'    are the standardized column names, i.e. after running \code{NVIdb::standardize_columns}.
#'    The raw data can be edited in the \code{./data-raw/PJS_levels.xlsx} and the the code for preparing of the data frame is written in
#'    \code{./data-raw/generate_PJS_levels.R}. The \code{PJS_levels} is used as input for \code{NVIdb::select_PJS_levels}.
#'
#' @details The variables included into a specific level is given the value 1, if not included they are given the value 0. To
#'    ensure that information on a specific level an be traced to the correct sak, all index variables are given value 1.
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{variable}{column name for variables read from PJs and standardized using \code{NVIdb::standardize_columns}}
#'   \item{sak}{columns at sak-level are given value 1}
#'   \item{prove}{columns at prove-level are given value 1}
#'   \item{delprove}{columns at delprove-level are given value 1}
#'   \item{undersokelse}{columns at undersokelse-level are given value 1}
#'   \item{resultat}{columns at resultat-level are given value 1}
#'   \item{konklusjon}{columns at konklusjon-level are given value 1}
#' }
#' @source \code{./data-raw/PJS_levels.xlsx} in package \code{NVIdb}
"PJS_levels"
