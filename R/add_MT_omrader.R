#' @title Manage translation from komnr to MT-avdeling and MT-region
#' @description Function to add columns with MT_avdelingnr, MT_avdelng (name), MT_regionnr and MT_region (name). In addition there are
#'    functions to read and copy the translation tables.
#' @details \code{add_MT_omrader} can be used to translate the komnr into
#'     MT_avdelingnr, MT_avdeling, MT_regionnr and MT_region. The
#'     function can also be used to translate MT_avdelingnr into
#'     MT_avdeling, MT_regionnr and MT_region or to translate MT_regionnr
#'     into MT_region. When the \code{code_column} in the dataframe is
#'     not equal to one of c("komnr", "MT_avdelingnr", "MT_regionnr") the
#'     \code{code_column} can be input as a named vector. Likewise, if
#'     the new columns should be given other names than c("MT_avdelingnr",
#'     "MT_avdeling", "MT_regionnr", "MT_region"), the \code{new_column}
#'     can be input as a named vector, see examples.
#'
#'     \code{add_MT_omrader} uses a premade translation table
#'     ("komnr_2_MT_avdeling.csv"). These data need to be loaded by
#'     \code{read_MT_omrader} before running \code{add_MT_omrader}, see
#'     example. "komnr_2_MT_avdeling.csv" is made based on information in
#'     PJS adresseregister. The translation table is updated when we know
#'     there is a need.
#'
#'     \code{position} is used to give the place if the new columns in the
#'     data frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for
#'     \code{position = "left"} the new variables are placed to the left of
#'     the code_variable. If \code{position = "first"} or
#'     \code{position = "last"} the new columns are placed first or last,
#'     respectively, in the data.frame. A special case occurs for
#'     \code{position = "keep"} which only has meaning when the new column
#'     has the same name as an existing column and overwrite = TRUE. In these
#'     cases, the existing column will be overwritten with new data and have
#'     the same position.
#'
#'     \code{read_MT_omrader} reads the files "komnr_2_MT_avdeling.csv" and
#'     "MT_omrader.csv" into a data frame, usually named komnr_2_MT_omrader.
#'     This file is used by \code{add_MT_omrader}. If no options to the
#'     function is given, the function will read the latest updated files
#'     from NVI's internal network. If changing the \code{from_path}, the
#'     function can be used to read the translation file from other
#'     directories. This can be useful if having a script that don't have
#'     access to NVI's internal network.
#'
#'     \code{copy_MT_omrader} Copies the csv-files "komnr_2_MT_avdeling.csv"
#'     and "MT_omrader.csv" to another directory. Thereby, these files are
#'     available for \code{read_MT_omrader} if they should be read from
#'     another directory.
#'
#' @param data Data frame with data with a column with a komnr.
#' @param translation_table Data frame with the table for translating from
#'     komnr to MT_areas.
#' @param code_column The column with the coded value. Valid values are one
#'     of c("komnr", "MT_avdelingnr", "MT_regionnr"). If the column in data
#'     has another name, it can be input as a named vector, see examples.
#' @param new_column The new columns that should be included into the data
#'     frame. The new columns can be up to c("MT_avdelingnr", "MT_avdeling",
#'     "MT_regionnr", "MT_region") depending on \code{code_column}.
#'     If the new columns in the result data frame should have other names,
#'     \code{new_column} can be input as a named vector, see examples.
#' @template position
#' @template overwrite
#' @param filename a list with the filenames of the source files with the tables for generating the translation table.
#' @param from_path Path for the source files for the translation table.
#' @param to_path Path to where the source files for the translation table should be copied.
#'
#' @return \code{add_MT_omrader} A data frame where the MT_avdelingnr has been added in the column to the
#'     right of the column with the komnr.
#'
#'     \code{read_MT_omrader} A data frame with the table for translating from komnr to
#'     c("MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region") as read from the source
#'     csv file. If not changing standard input to the function, the standard files at
#'     NVI's internal network is read.
#'
#'     \code{copy_MT_omrader} Copies the csv-files "komnr_2_MT_avdeling.csv" and "MT_omrader.csv"
#'     to another directory. If the target files already exists the source files are only
#'     copied if they are newer than the target files.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' komnr_2_MT_omrader <- read_MT_omrader()
#'
#' # Copy the csv-files used to generate the translation table from the standard location to
#' # the subdirectory Data below the working directory
#' copy_MT_omrader(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' komnr_2_MT_omrader <- read_MT_omrader(from_path = "./Data/")
#'
#' # Add new columns with MT_avdelingnr, MT_avdeling, MT_regionnr, and MT_region based on komnr
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = "komnr",
#'                        new_column = c("MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region"))
#'
#' # Add new columns with MT_avdelingnr and MT_avdeling based on komnr. The colname of the column
#' # with komnr is komnr and the new columns are renamed to MT_avdnr and MT_avd.
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = c("komnr" = "komnr"),
#'                        new_column = c("MT_avdnr" = "MT_avdelingnr", "MT_avd" = "MT_avdeling"))
#'
#' # Add new columns with MT_region based on MT_regionnr. MT_region is renamed to MT_regionnavn
#' # Remember to load "komnr_2_MT_omrader" by "read_MT_omrader()" before running "add_MT_omrader",
#' # see above.
#' newdata <- add_MT_omrader(olddata,
#'                        translation_table = list(komnr_2_MT_omrader),
#'                        code_column = "MT_region",
#'                        new_column = c("MT_regionnavn" = "MT_region"))
#' }
#'
add_MT_omrader <- function(data,
                           translation_table = komnr_2_MT_omrader,
                           code_column = c("komnr"),
                           new_column = c("MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region"),
                           position = "right",
                           overwrite = FALSE) {

  # Ensure that code_column and new_column are named vectors by using the internal function set_name_vector()
  # Thereby, the following code can assume these to be named vectors
  code_column <- set_name_vector(code_column)
  new_column <- set_name_vector(new_column)

  # # ARGUMENT CHECKING ----
  # assert_add_function(data = data,
  #                     translation_table = translation_table,
  #                     code_column = code_column,
  #                     new_column = new_column,
  #                     position = position,
  #                     overwrite = overwrite)
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
  # rows when code_column is not "komnr"
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
                         n_columns_at_once = length(new_column)
  )


  return(data)
}

# To avoid checking of the variable kommune_fylke as default input argument in the function
utils::globalVariables("komnr_2_MT_omrader")
