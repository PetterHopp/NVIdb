#' @title Data: Variables per PJS-level.
#'
#' @description A data frame with the variable names (column names) in PJS and their corresponding PJS-level. The column names
#'    are the standardized column names, i.e. after running \code{NVIdb::standardize_columns}.
#'    The raw data can be edited in the "./data-raw/PJS_levels.xlsx" and the the code for preparing of the data frame is written in
#'   "./data-raw/generate_PJS_levels.R". The \code{PJS_levels} is used as input for \code{NVIdb::select_PJS_levels}.
#'
#' @details The variables included into a specific level is given the value 1, if not included they are given the value 0. To
#'    ensure that information on a specific level an be traced to the correct sak, all index variables are given value 1.
#'
#' @format A data frame with 9 variables:
#' \describe{
#'   \item{variable}{column name for variables read from PJS and standardized using \code{NVIdb::standardize_columns}}
#'   \item{sak}{columns at sak-level are given value 1}
#'   \item{prove}{columns at prove-level are given value 1}
#'   \item{delprove}{columns at delprove-level are given value 1}
#'   \item{undersokelse}{columns at undersokelse-level are given value 1}
#'   \item{resultat}{columns at resultat-level are given value 1}
#'   \item{konklusjon}{columns at konklusjon-level are given value 1}
#'   \item{subundersokelse}{columns at subundersokelse-level are given value 1}
#'   \item{subresultat}{columns at subresultat-level are given value 1}
#' }
#' @source "./data-raw/PJS_levels.xlsx" in package \code{NVIdb}
"PJS_levels"


#' @title Data: PJS_code_description_colname, standard column names for
#'     description texts for selected code variables in PJS.
#'
#' @description A data frame with the standard variable names (column names) for
#'    the code variables in PJS, their corresponding standard name of the column
#'    with  the descriptive text and a column with the PJS type that will can
#'    be used to translate from the code variable to the descriptive text. The
#'    column names of the code variable are the standardised column names, i.e.
#'    after running \code{NVIdb::standardize_columns}.
#'
#'    The raw data can be edited in the "./data-raw/generate_PJS_code_description_colname.R".
#'    The \code{PJS_code_description_colname} is used by \code{NVIdb::add_PJS_code_description}
#'    when using the options \code{PJS_variable_type = "auto"} and/or
#'    \code{new_column = "auto"}.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{code_colname}{column name for selected code variables in PJS and that have been standardized using \code{NVIdb::standardize_columns}}
#'   \item{type}{the type of PJS variable as used by \code{NVIdb::add_PJS_code_description} to translate PJS-codes to description text}
#'   \item{new_column}{The new standard column names for the corresponding code column name in PJS}
#' }
#' @source "./data-raw/generate_PJS_code_description_colname.R" in package \code{NVIdb}
"PJS_code_description_colname"
