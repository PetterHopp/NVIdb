# build_sql_modules
# Collection of query modules that can be used when building queries for PJS

# build_sql_select_year ----
#' @title Builds sql modules to be included in select statements for PJS
#' @description Builds sql modules to be included in select statements for PJS
#'    when building queries for selecting data. The functions takes the values
#'    for which observations should be selected as input and builds the
#'    sql syntax.
#'
#' @details \code{build_sql_select_year} builds the SQL syntax to select observations
#'     from one or more consecutive years from PJS. The input can be given as
#'     one year, the first and last year or a range of years. If a range is given,
#'     this will be interpreted as first and last years and all years in between
#'     will be included.
#'
#' \code{build_sql_select_code} builds the SQL syntax to select observations
#'     with the given code values from one variable in PJS with hierarchical codes.
#'     When the code value including sub codes  should be selected, add "%" to the
#'     code, see example.
#'
#' Be aware that these functions only builds an sql building block to be
#'     included into a select statement. It will not build a complete select
#'     statement. These functions are mainly intended for internal use and
#'     are called from
#'     \ifelse{html}{\code{\link{build_query_hensikt}}}{\code{build_query_hensikt}},
#'     \ifelse{html}{\code{\link{build_query_one_disease}}}{\code{build_query_one_disease}}
#'     and
#'     \ifelse{html}{\code{\link{build_query_outbreak}}}{\code{build_query_outbreak}}.
#'     If generating own select statements, these can be used to facilitate
#'     the coding. The building blocks can be combined with "AND" and "OR"
#'     and brackets to get the intended select statement.
#'
#' @template build_query_year
#' @param values [\code{character}]\cr
#'     The value of the codes that should be selected. If sub-codes should be
#'     included, add "\%" after the code, see example.
#' @param varname [\code{character(1)}]\cr
#'     The PJS variable name of the variable in PJS from which the
#'     coded values should be selected.
#' @template build_query_db
#'
#' @return SQL-code to be included when building select-statements for PJS.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @rdname build_sql_modules
#' @name build_sql_modules
#'
#' @examples
#' # SQL-select module for selecting year from PJS
#' build_sql_select_year(year = 2020, varname = "aar")
#'
#' build_sql_select_year(year = c(2019, 2021), varname = "aar")
#'
#' build_sql_select_year(year = c(2019:2021), varname = "aar")
#'
#' # SQL-select module for selecting hensiktkode from PJS
#' build_sql_select_code(values = "0100101", varname = "hensiktkode", db = "PJS")
#'
#' build_sql_select_code(values = "0100101%", varname = "hensiktkode", db = "PJS")
#'
#' build_sql_select_code(values = c("0100101", "0100101007", "0100102%", "0100202%"),
#'                       varname = "hensiktkode",
#'                       db = "PJS")
build_sql_select_year <- function(year, varname, db = "PJS") {
  # ARGUMENT CHECKING ----

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1990,
                               upper = as.numeric(format(Sys.Date(), "%Y")),
                               min.len = 1,
                               any.missing = FALSE,
                               add = checks)
  checkmate::assert_character(varname, min.chars = 1, len = 1, any.missing = FALSE, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # CLEAN YEAR INPUT ----
  # Ensure that year vector have unique values and are ordered
  year <- unique(year[order(year)])

  # GENERATE SQL STRING ----
  # set equal if only one year
  if (length(year) == 1) {
    select_year <- paste(varname, "=", year)
  }
  # Use larger than and less than if from year to year
  # The function does not include the possibility of selecting non-consecutive years.
  if (length(year) > 1) {
    if (year[length(year)] == as.numeric(format(Sys.Date(), "%Y"))) {
      select_year <- paste(varname, ">=", year[1])
    } else {
      select_year <- paste(varname, ">=", year[1], "AND",
                           varname, "<=", year[length(year)])
    }
  }
  return(select_year)
}



# build_sql_select_code ----
#' @export
#' @rdname build_sql_modules
#'
build_sql_select_code <- function(values, varname, db = "PJS") {

  # cleaning values argument before argument checking
  if (!is.null(values)) {values <- trimws(values)}

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_character(values, null.ok = TRUE, all.missing = FALSE, min.chars = 1, add = checks)
  checkmate::assert_character(varname, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # # Removes NA to avoid problems with CMD check and generating sql string
  # values <- subset(values, !is.na(values))

  # GENERATE SQL STRING ----
  # Generate empty string if values are NULL
  select_code <- ""

  if (!is.null(values) &&
      (length(values) > 1 || (length(values) == 1 & trimws(values[1]) != ""))) {

    # Include missing if any NA
    if (any(is.na(values))) {
      select_code <- paste(varname, "IS NULL OR ")
      values <- subset(values, !is.na(values))
    }

    # use "=" in sql string for values where sub-codes shall not be included when one code
    if (length(grep("%", values, invert = TRUE)) == 1) {
      select_code <- paste0(select_code, varname, " = '", grep("%", values, value = TRUE, invert = TRUE), "'")
    }

    # use "IN" in sql string for values where sub-codes shall not be included when more than one code
    if (length(grep("%", values, invert = TRUE)) > 1) {
      select_code <- paste0(select_code, varname, " IN ('", paste(grep("%", values, value = TRUE, invert = TRUE), collapse = "', '"), "')")
    }

    # use "like" in sql string for values where sub-codes shall be included
    values <- grep("%", values, value = TRUE, invert = FALSE)
    if (length(values) > 0) {
      for (i in 1:length(values)) {
        if (select_code != "" & grepl("OR $", select_code) < 1) {
          select_code <- paste(select_code, "OR")
        }
        # select_code <- paste(select_code, match.call()[1], varname, "LIKE", values[i])
        select_code <- paste(select_code, varname, "LIKE", paste0("'", values[i], "'"))

      }
    }
  }
  # Removes leading space if only sub-codes are included
  return(trimws(select_code))
}
