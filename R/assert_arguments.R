# assert_arguments
#
# assert collections that developed to fit functions with standardized input arguments.
# - assert_add_function
# - assert_copy_function
#
#
# #' @title Collection of assertions for add_functions
# #' @description Collection of assertions used in standard add_functions.
# #'
# #' @details All add functions except one, have the same arguments and the
# #'     assertion can be standardized.
# #'
# #' @param data Argument to the add-function to be asserted.
# #' @param translation_table Argument to the add-function to be asserted.
# #' @param code_column Argument to the add-function to be asserted.
# #' @param new_column Argument to the add-function to be asserted.
# #' @param position Argument to the add-function to be asserted.
# #' @param overwrite Argument to the add-function to be asserted.
# #'
# #' @return \code{TRUE} if none of the assertions failed. If any of the assertions
# #'     failed, one or more error messages are returned.
# #'
# #' @author Petter Hopp Petter.Hopp@@vetinst.no
# #' @keywords internal
# #'
# assert_add_function <- function(data,
#                                 translation_table,
#                                 code_column,
#                                 new_column,
#                                 position,
#                                 overwrite) {
#
#   # ARGUMENT CHECKING ----
#   # Object to store check-results
#   checks <- checkmate::makeAssertCollection()
#
#   # Perform checks
#   # data
#   checkmate::assert_data_frame(data, add = checks)
#   # translation_table
#   checkmate::assert_data_frame(translation_table, add = checks)
#   # code_column
#   checkmate::assert_vector(code_column, any.missing = FALSE, len = 1, add = checks)
#   NVIcheckmate::assert_names(unname(code_column),
#                              type = "named",
#                              subset.of = colnames(translation_table),
#                              comment = paste0("The value for code_column must be a column in the translation table, ",
#                                               #  deparse(substitute(translation_table)),
#                                               "but '",
#                                               unname(code_column),
#                                               "' is not a column name in the translation table"),
#                              add = checks)
#   NVIcheckmate::assert_names(names(code_column),
#                              type = "named",
#                              subset.of = colnames(data),
#                              comment = paste0("The name of the code_column must be a column in the data",
#                                               # deparse(substitute(data)),
#                                               ", but '",
#                                               base::setdiff(names(code_column), colnames(data)),
#                                               "' is not a column in the data. You must name the code_column ",
#                                               "if the code_column in data is different from in the translation table"),
#                              add = checks)
#   # new_column
#   checkmate::assert_vector(new_column, any.missing = FALSE, min.len = 1, add = checks)
#   NVIcheckmate::assert_names(unname(new_column),
#                              type = "named",
#                              subset.of = colnames(translation_table),
#                              comment = paste0("The value(s) for new_column must be column name(s) in the translation table",
#                                               # deparse(substitute(translation_table)),
#                                               ", but '",
#                                               unname(new_column),
#                                               "' are not column name(s) in the translation table"),
#                              add = checks)
#   if (isFALSE(overwrite)) {
#     NVIcheckmate::assert_names(names(new_column),
#                                type = "named",
#                                disjunct.from = setdiff(colnames(data), code_column),
#                                comment = paste0("The column name(s): '",
#                                                 intersect(colnames(data), names(new_column)),
#                                                 "' already exist in '",
#                                                 deparse(substitute(data)),
#                                                 "`. Either give new column name(s) for the column(s) called '",
#                                                 intersect(colnames(data), names(new_column)),
#                                                 "' or specify overwrite = TRUE to replace values in the existing column(s) with new content"),
#                                add = checks)
#   }
#   NVIcheckmate::assert_names(names(new_column),
#                              type = "named",
#                              disjunct.from = names(code_column),
#                              comment = paste0("You cannot give any of the new column(s) the same name as the code_column '",
#                                               names(code_column),
#                                               "' in the data" # ,
#                                               # deparse(substitute(data)), "`"
#                              ),
#                              add = checks)
#   # position
#   # checkmate::assert_choice(position, choices = c("first", "left", "right", "last", "keep"), add = checks)
#   position <- NVIcheckmate::match_arg(x = position,
#                                       choices = c("first", "left", "right", "last", "keep"),
#                                       several.ok = TRUE,
#                                       ignore.case = FALSE,
#                                       add = checks)
#   # overwrite
#   checkmate::assert_logical(overwrite, any.missing = FALSE, len = 1, add = checks)
#
#
#   # Report check-results
#   checkmate::reportAssertions(checks)
# }

#' @title Collection of assertions for copy_functions
#' @description Collection of assertions used in standard copy_functions.
#'
#' @details All copy functions have the same arguments and the
#'     assertion can be standardized. The assertion of \code{filname} is
#'     constructed to handle both character and list input.
#'
#'     The assertion is based on removing ending "\\" and "/" from
#'     \code{from_path} and \code{to_path} before the assertion is performed.
#'
#' @param filename Argument to the copy-function to be asserted.
#' @param from_path Argument to the copy-function to be asserted. Ending
#'     "\\" and "/" should have been removed before the assertion is performed.
#' @param to_path Argument to the copy-function to be asserted. Ending
#'     "\\" and "/" should have been removed before the assertion is performed.
#'
#' @return \code{TRUE} if none of the assertions failed. If any of the assertions
#'     failed, one or more error messages are returned.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#'
#'
assert_copy_function <- function(filename,
                                 from_path,
                                 to_path) {
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  ## filename
  checkmate::assert(checkmate::check_character(filename,
                                               min.chars = 1,
                                               len = 1),
                    checkmate::check_list(filename, min.len = 1),
                    combine = "or",
                    add = checks)
  
  # checkmate::assert_list(filename, len = 2, add = checks)
  # # from_path
  # checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  #   checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }
  
  ## from_path / filename
  for (i in c(1:length(filename))) {
    checkmate::assert_file_exists(file.path(from_path, filename[[i]]), access = "r", add = checks)
  }
  ## to_path
  checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  
  # Report check-results
  checkmate::reportAssertions(checks)
}

# #' @title Collection of assertions for read_functions
# #' @description Collection of assertions used in standard read_functions.
# #'
# #' @details All read functions have the same arguments and the
# #'     assertion can be standardized. The assertion of \code{filname} is
# #'     constructed to handle both character and list input.
# #'
# #'     The assertion is based on removing ending "\\" and "/" from
# #'     \code{from_path} before the assertion is performed.
# #'
# #' @param filename Argument to the read-function to be asserted.
# #' @param from_path Argument to the read-function to be asserted. Ending
# #'     "\\" and "/" should have been removed before the assertion is performed.
# #'
# #' @return \code{TRUE} if none of the assertions failed. If any of the assertions
# #'     failed, one or more error messages are returned.
# #'
# #' @author Petter Hopp Petter.Hopp@@vetinst.no
# #' @keywords internal
# #'
# #'
# assert_read_function <- function(filename,
#                                  from_path) {
# 
#   # ARGUMENT CHECKING ----
#   # Object to store check-results
#   checks <- checkmate::makeAssertCollection()
# 
#   # Perform checks
#   ## filename
#   NVIcheckmate::assert(checkmate::check_character(filename,
#                                                   min.chars = 1,
#                                                   len = 1),
#                        checkmate::check_list(filename, min.len = 1),
#                        combine = "or",
#                        add = checks)
#   ## from_path / filename
#   for (i in c(1:length(filename))) {
#     checkmate::assert_file_exists(file.path(from_path, filename[[i]]), access = "r", add = checks)
#   }
# 
#   # Report check-results
#   checkmate::reportAssertions(checks)
# }

#' @title Collection of assertions for add_functions
#' @description Collection of assertions used in standard add_functions.
#'
#' @details All add functions except one, have the same arguments and the
#'     assertion can be standardized.
#'
#' The assertCollection must be set up separately and is used as input argument, see example.
#'
#' @param data Argument to the add-function to be asserted.
#' @param translation_table Argument to the add-function to be asserted.
#' @param code_column Argument to the add-function to be asserted.
#' @param new_column Argument to the add-function to be asserted.
#' @param overwrite Argument to the add-function to be asserted.
#' @param add assertCollection.
#'
#' @return An assertCollection that have been updated with the results
#'     of assertions for \code{data}, \code{translation_table},
#'     \code{code_column}, \code{new_column} and \code{overwrite}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#' @export
#'
assert_add_functions <- function(data,
                                 translation_table,
                                 code_column,
                                 new_column,
                                 overwrite,
                                 add) {
  
  # ARGUMENT CHECKING ----
  # Perform checks
  # data
  checkmate::assert_data_frame(data, add = add)
  # translation_table
  checkmate::assert_data_frame(translation_table, add = add)
  # code_column
  checkmate::assert_vector(code_column, any.missing = FALSE, len = 1, add = add)
  NVIcheckmate::assert_names(unname(code_column),
                             type = "named",
                             subset.of = colnames(translation_table),
                             comment = paste0("The value for code_column must be a column in the translation table, ",
                                              #  deparse(substitute(translation_table)),
                                              "but '",
                                              unname(code_column),
                                              "' is not a column name in the translation table"),
                             add = add)
  NVIcheckmate::assert_names(names(code_column),
                             type = "named",
                             subset.of = colnames(data),
                             comment = paste0("The name of the code_column must be a column in the data",
                                              # deparse(substitute(data)),
                                              ", but '",
                                              base::setdiff(names(code_column), colnames(data)),
                                              "' is not a column in the data. You must name the code_column ",
                                              "if the code_column in data is different from in the translation table"),
                             add = add)
  # new_column
  checkmate::assert_vector(new_column, any.missing = FALSE, min.len = 1, add = add)
  NVIcheckmate::assert_names(unname(new_column),
                             type = "named",
                             subset.of = colnames(translation_table),
                             comment = paste0("The value(s) for new_column must be column name(s) in the translation table",
                                              # deparse(substitute(translation_table)),
                                              ", but '",
                                              unname(new_column),
                                              "' are not column name(s) in the translation table"),
                             add = add)
  if (isFALSE(overwrite)) {
    NVIcheckmate::assert_names(names(new_column),
                               type = "named",
                               disjunct.from = setdiff(colnames(data), code_column),
                               comment = paste0("The column name(s): '",
                                                intersect(colnames(data), names(new_column)),
                                                "' already exist in '",
                                                deparse(substitute(data)),
                                                "`. Either give new column name(s) for the column(s) called '",
                                                intersect(colnames(data), names(new_column)),
                                                "' or specify overwrite = TRUE to replace values in the existing column(s) with new content"),
                               add = add)
  }
  NVIcheckmate::assert_names(names(new_column),
                             type = "named",
                             disjunct.from = names(code_column),
                             comment = paste0("You cannot give any of the new column(s) the same name as the code_column '",
                                              names(code_column),
                                              "' in the data" # ,
                                              # deparse(substitute(data)), "`"
                             ),
                             add = add)
  
  # overwrite
  checkmate::assert_logical(overwrite, any.missing = FALSE, len = 1, add = add)
  
  return(add)
}


#' @title Collection of assertions for read_functions
#' @description Collection of assertions used in standard read_functions.
#'
#' @details All read functions include the same arguments and the
#'     assertion can be standardized. The assertion of \code{filname} is
#'     constructed to handle both character and list input.
#'
#'     The assertion is based on removing ending "\\" and "/" from
#'     \code{from_path} before the assertion is performed.

#' The assertCollection must be set up separately and is used as input argument, see example.
#'
#' @param filename Argument to the function to be asserted.
#' @param from_path Argument to the function to be asserted. Ending
#'     "\\" and "/" must have been removed before the assertion is performed.
#' @param add assertCollection.
#'
#' @return An assertCollection that have been updated with the results of assertions for \code{filename} and \code{from_path}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @keywords internal
#' @export
#'
#' @examples
#' # Attach package and make temporary directory
#' library(NVIdb)
#' td <- tempdir()
#' write.csv2(NVIdb::PJS_levels, file = file.path(td, "PJS_levels.csv"))
#'
#' # ARGUMENT CHECKING
#' # Object to store check-results
#' checks <- checkmate::makeAssertCollection()
#' # Perform checks
#' checks <- assert_read_functions(filename = "PJS_levels.csv",
#'                                 from_path = td,
#'                                 add = checks)
#' # Report check-results
#' checkmate::reportAssertions(checks)
assert_read_functions <- function(filename,
                                  from_path,
                                  add) {
  
  checkmate::assert_class(x = add, classes = "AssertCollection")
  
  # Perform checks ----
  
  ## filename
  checkmate::assert(checkmate::check_character(filename,
                                               min.chars = 1, len = 1,
                                               any.missing = FALSE),
                    checkmate::check_list(filename,
                                          min.len = 1,
                                          any.missing = FALSE),
                    combine = "or",
                    add = add)
  ## from_path / filename
  for (i in c(1:length(filename))) {
    checkmate::assert_file_exists(file.path(from_path, filename[[i]]), access = "r", add = add)
  }
  return(add)
}
