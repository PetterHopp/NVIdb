#' @title Manage translation from prodnr8 into different produsent properties
#' @description Function to add a column with gjeldende_prodnr8. In addition there are
#'    functions to read and copy the translation tables.
#' @details \code{add_produsent_properties} can be used to translate the prodnr8 into gjeldende_prodnr8 and/or geo-coordinates.
#'
#'     \code{position} is used to give the place if the new columns in the data.frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for \code{position = "left"} the new variables are placed to the left of the
#'     code_variable. If \code{position = "first"} or \code{position = "last"} the new columns are placed first or last, respectively, in the
#'     data frame. A special case occurs for \code{position = "keep"} which only has meaning when the new column has the same name as an existing
#'     column and overwrite = TRUE. In these cases, the existing column will be overwritten with new data and have the same position.
#'
#'     \code{impute_old_when_missing = TRUE} is used to replace missing values in the \code{new_column} with the value in
#'     \code{code_column}. This is useful when translating prodnr8 to gjeldende_prodnr8. It should not be used when translating
#'     from prodnr8 to something where imputing the old prodnr8 in the new variables don't have any meaning, for example geo-coordinates.
#'
#'     \code{read_prodnr_2_current_prodnr} reads the file "Prodnr2GjeldendeProdnr.csv" into a data frame that can be used by
#'     other routines. Standard setting will the file read in the latest updated file from NVI's internal network. If changing
#'     the \code{from_path}, the function can be used to read the translation file from other directories. This can be useful if having a stand alone
#'     app with no connection the NVI's internal network. In other cases, it should be avoided.
#'
#'     \code{copy_prodnr_2_current_prodnr} copies the file "Prodnr2GjeldendeProdnr.csv" to a chosen directory.
#'
#'     \code{read_prodnr_2_coodinates} reads the file "Prodnr2Koordinater.csv" into a data frame that can be used to merge with data frames with
#'     prodnr8. Standard setting will the file read in the latest updated file from NVI's internal network. If changing the \code{from_path}, the function
#'     can be used to read the translation file from other directories. This can be useful if having a stand alone app with no connection the
#'     NVI's internal network. In other cases, it should be avoided.
#'
#' @param data Data frame with data with a column with a prodnr8
#' @param translation_table Data frame with the table for translating from prodnr8 to gjeldende_prodnr8.
#' @param code_column The column with the coded value. Valid values are one of c("prodnr8"). If the column in
#'     data has another name, it can be input as a named vector, see examples.
#' @param new_column The new columns that should be included into the data frame. The new columns can be up to
#'     c("gjeldende_prodnr8") depending on \code{code_column}. If the new columns in the result data frame
#'     should have other names, \code{new_column} can be input as a named vector, see examples.
#' @template position
#' @template overwrite
#' @param impute_old_when_missing Should the ID-variable be used as value for the \code{new_column} if the
#'     \code{new_column} value is missing? Default is \code{FALSE}. To be used for translating prodnr8 to
#'     gjeldende_prodnr8, see details.
#' @param filename a list with the filenames of the source files with the tables for generating the translation table.
#' @param from_path Path for the source files for the translation table.
#' @param to_path Path for the target translation table when copying the translation table.
#'
#' @return \code{add_produsent_properties} returns a data frame where the column with gjeldende_prodnr8 has been added to the
#'     right of the column with prodnr8.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # CURRENT PRODNR8
#' # Reading from standard directory at NVI's network
#' prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr()
#'
#' # Copy standard file from standard location to the subdirectory Data below the working directory
#' copy_prodnr_2_current_prodnr(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr(from_path = "./Data/")
#'
#' prodnr8 <- c("09140087", "14260856", "17020818", "50060129")
#' olddata <- as.data.frame(prodnr8)
#'
#' # Add new column with current prodnr8
#' newdata <- add_produsent_properties(olddata,
#'                                     translation_table = prodnr_2_gjeldende_prodnr,
#'                                     code_column = "prodnr8",
#'                                     new_column = "gjeldende_prodnr8",
#'                                     position = "left",
#'                                     impute_old_when_missing = TRUE)
#'
#' # COORDINATES
#' # Reading from standard directory at NVI's network
#' prodnr_2_koordinater <- read_prodnr_2_coordinates()
#'
#' newdata <- add_produsent_properties(newdata,
#'                                     translation_table = prodnr_2_koordinater,
#'                                     code_column = "prodnr8",
#'                                     new_column = c("longitude" = "geo_eu89_o",
#'                                                    "latitude" = "geo_eu89_n"))
#' }
#'
add_produsent_properties <- function(data,
                                     translation_table,
                                     code_column,
                                     new_column,
                                     position = "right",
                                     overwrite = FALSE,
                                     impute_old_when_missing = FALSE) {

  # Ensure that code_column and new_column are named vectors by using the internal function set_name_vector()
  # Thereby, the following code can assume these to be named vectors
  code_column <- set_name_vector(code_column)
  new_column <- set_name_vector(new_column)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_add_functions(data = data,
                                 translation_table = translation_table,
                                 code_column = code_column,
                                 new_column = new_column,
                                 overwrite = overwrite,
                                 add = checks)
  # position
  position <- NVIcheckmate::match_arg(x = position,
                                      choices = c("first", "left", "right", "last", "keep"),
                                      several.ok = TRUE,
                                      ignore.case = FALSE,
                                      add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE TRANSLATION TABLE ----
  # Makes the translation table with code_column and new_column. unique() is necessary to avoid duplicate
  # rows when code_column is not "kommunenr"
  code_2_new <- unique(translation_table[, c(unname(code_column), unname(new_column))])

  # ADD NEW COLUMN(S) ----
  # Set up of parameters for the internal function add_new_column(). names() is used to select the column names
  # in the input data and unname() is used to select the column names in the translation table. n_columns_at_once
  # is the number of new columns that should be added.
  data <- add_new_column(data,
                         ID_column = names(code_column),
                         new_colname = names(new_column),
                         translation_tables = list(code_2_new),
                         ID_column_translation_table = unname(code_column),
                         to_column_translation_table = unname(new_column),
                         position = position,
                         overwrite = overwrite,
                         impute_old_when_missing = impute_old_when_missing,
                         n_columns_at_once = length(new_column)
  )


  return(data)
}
