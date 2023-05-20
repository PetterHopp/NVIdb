#' @title Manage translation from postnr to poststed and komnr
#' @description Function to add columns with poststed and komnr. In addition there are functions to read and copy the translation tables.
#'
#' @details Data sources may provide data with postnr. These functions manage translating postnr to poststed and komnr.
#'
#'     \code{add_poststed} can be used to translate postnr to poststed and komnr.
#'
#'     One has to ensure that the code in the data column represents a postnr. The function will translate any 4 digits that has the same ID as a postnr.
#'
#'     Standard name for the postnr is postnr. If the column with the postnr that should be translated has another name, the
#'     parameter \code{code_column =} can be input as a named vector. Standard names for the new columns are c("poststed", "komnr"). Likewise, if the
#'     new columns should be given other names than these, the parameter \code{new_column =} can be input as a named vector, see examples.
#'
#'     \code{add_poststed} uses a premade translation table (Poststed_UTF8.csv). These data need to be loaded by \code{read_poststed}
#'     before running \code{add_poststed}, see example. "Poststed_UTF8.csv" is made based on information in PJS adresseregister.
#'     The translation table is updated when we know there is a need.
#'
#'     \code{position =} is used to give the place if the new columns in the data.frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for \code{position = "left"} the new variables are placed to the left of the
#'     code_variable. If \code{position = "first"} or \code{position = "last"} the new columns are placed first or last, respectively, in the
#'     data.frame. A special case occurs for \code{position = "keep"} which only has meaning when the new column has the same name as an existing
#'     column and overwrite = TRUE. In these cases, the existing column will be overwritten with new data and have the same position.
#'
#'     \code{read_poststed} read the file "Poststed_UTF8.csv" a data frame that can be used by \code{add_poststed}. Standard setting will read
#'     the file from NVI's internal network. If changing the \code{from_path}, the function can be used to read the translation files from other directories.
#'     This can be useful if having a stand alone app with no connection the NVI's internal network. In other cases, it should be avoided.
#'
#'     \code{copy_poststed} copy the file Poststed_UTF8.csv to a given directory.
#'
#'
#' @param data Data frame with data with a column with postnr
#' @param translation_table Data frame with the translation table for postnr to poststed and komnr
#' @param code_column The name of the column with the postnr
#' @param new_column The name of the new column that should contain the poststed and/or komnr
#' @template position
#' @template overwrite
#' @param filename Filename of the translation table for postnr to poststed and komnr
#' @param from_path Path for the source translation table
#' @param to_path Path for the target translation table when copying the translation table
#'
#' @return \code{add_poststed} A data frame where one or more of the columns c("poststed", "komnr") have been added in the column(s) to the right
#'     of the column with the postnr.
#'
#'     \code{read_poststed} A data frame with the original postnr and the corresponding poststed and komnr. If not changing standard input to the
#'     function, the standard file at NVI's internal network is read.
#'
#'     \code{copy_poststed} copies the source translation table for postnr to poststed and komnr to given directory. If the target file already exists,
#'     the source file is copied only when it is newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' poststed <- read_poststed()
#'
#' # Copy standard file from standard location to the subdirectory Data below the working directory
#' copy_poststed(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' poststed <- read_poststed(from_path = "./Data/")
#'
#' # Add new column with poststed and komnr
#' # The variable postnummer should be translated into poststed and komnr. For poststed
#' # the standard name is kept. For komnr the new variable is named postkomnr.
#' # Remember to load "poststed" by "read_poststed()" before running "add_poststed",
#' # see above.
#' newdata <- add_poststed(olddata,
#'                         translation_table = poststed,
#'                         code_column = c("postnummer" = "postnr"),
#'                         new_column = c("poststed", "postkomnr" = "komnr"))
#' }
#'

add_poststed <- function(data,
                         translation_table = poststed,
                         code_column = c("postnr"),
                         new_column = c("poststed", "komnr"),
                         position = "right",
                         overwrite = FALSE) {

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


  # Makes the translation table with code_column and new_column. unique() is necessary to avoid duplicate
  # rows when code_column is not "komnr"
  code_2_new <- unique(translation_table[, c(unname(code_column), unname(new_column))])

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
utils::globalVariables("poststed")
