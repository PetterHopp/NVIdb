# build_sql_modules
# Collection of query modules that can be used when building queries for PJS


# build_sql_select_year ----
#' @title Builds sql module for selecting year from PJS
#' @description Builds a sql module for selecting one or more years from PJS. This can be included into queries for
#'    selecting data from PJS.
#'
#' @details The function builds the SQL syntax to select one or more consequtive years from PJS.
#'
#' @param db the database for which the query is built. Currently only the value "PJS" is accepted.
#' @param year the year that should be selected as integer. Can be given as one year, the first and last year or a range of years.
#'
#' @return a SQL-code for selecting year from PJS to be included when building select-statements.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @rdname build_sql_modules
#'
#' @examples
#' # SQL-select module for selecting year from PJS
#' build_sql_select_year(year = 2020, varname = aar)
#'
#' build_sql_select_year(year = c(2019, 2021), varname = aar)
#'
#' build_sql_select_year(year = c(2019:2021), varname = aar)
#'

build_sql_select_year <- function(year, varname, db = "PJS") {
  # ARGUMENT CHECKING ----
  
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  checkmate::assert_character(varname, add = checks)
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
#' @title Builds sql module for selecting hierarchical codes from PJS
#' @description Builds a sql module for selecting one or more codes from PJS. This can be included into queries for
#'    selecting data from PJS.
#'
#' @details The function builds the SQL syntax to select one or more codes for one variable from PJS.
#'
#' @param db the database for which the query is built. Currently only the value "PJS" is accepted.
#' @param values the value of the codes that should be selected given as character. If sub-codes should be included, add "%"
#'     after the code, see example.
#' @param varname The variable name of the variable from which tha coded values should be selected
#'
#' @return a SQL-code for selecting the codes from PJS to be included when building select-statements .
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @rdname build_sql_modules
#'
#' @examples
#' # SQL-select module for selecting hensiktkode from PJS
#' build_sql_select_code(values = "0100101", varname = "hensiktkode", db = "PJS") 
#' 
#' build_sql_select_code(values = "0100101%", varname = "hensiktkode", db = "PJS") 
#' 
#' build_sql_select_code(values = c("0100101", "0100101007", "0100102%", "0100202%"), 
#'                       varname = "hensiktkode", 
#'                       db = "PJS") 
#'

build_sql_select_code <- function(values, varname, db = "PJS") {
  
  # cleaning values argument before argument checking
  values <- trimws(values)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  checkmate::assert_character(values, null.ok = TRUE, any.missing = FALSE, min.chars = 1, add = checks)
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
      (length(values) > 1 || length(values) == 1 & trimws(values[1]) != "")) {
    #   select_code <- ""
    # } else {
    
    # use "=" in sql string for values where sub-codes shall not be included when one code
    if (length(grep("%", values, invert = TRUE)) == 1) {
      select_code <- paste0(varname, " = '", grep("%", values, value = TRUE, invert = TRUE), "'")
    }
    
    # use "IN" in sql string for values where sub-codes shall not be included when more than one code
    if (length(grep("%", values, invert = TRUE)) > 1) {
      select_code <- paste0(varname, " IN ('", paste(grep("%", values, value = TRUE, invert = TRUE), collapse = "', '"), "')")
    }
    
    # use "like" in sql string for values where sub-codes shall be included
    values <- grep("%", values, value = TRUE, invert = FALSE)
    if (length(values) > 0) {
      for (i in 1:length(values)) {
        if (select_code != "") {
          select_code <- paste(select_code, "OR")
        } # else {
        # select_code <- ""
        # }
        # select_code <- paste(select_code, match.call()[1], varname, "LIKE", values[i])
        select_code <- paste(select_code, varname, "LIKE", paste0("'", values[i], "'"))
        
      }
    }
  }
  # Removes leading space if only sub-codes are included
  return(trimws(select_code))
}

