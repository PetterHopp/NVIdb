#' @title Manage translation table for produksjonstilskuddskoder (Pkoder)
#' @description Read and copy the translation table for produksjonstilskuddskoder (Pkoder).
#' @details The translation table for Pkoder contains the Pkode, descriptive text, unit of interest (Dyr), whether the code counts unique animals
#'     or not (used when summarising number of animals), and sortering to order the Pkoder. The register covers 2017 and later.
#'
#'     \code{read_Pkode_2_text} reads the file "Produksjonstilskuddskoder_UTF8.csv" into a data frame. The standard settings will read
#'     the file from NVI's internal network. If changing the from_path, the function can be used to read the translation file from other
#'     directories. This can be useful if having a stand alone app with no connection the NVI's internal network. In other cases, it should
#'     be avoided.
#'
#'     \code{copy_Pkode_2_text} copies the file Produksjonstilskuddskoder_UTF8.csv to a given location.
#'
#' @param filename name of the translation table, standard name is "Produksjonstilskuddskoder_UTF8.csv"
#' @param from_path Path for the translation table for produksjonstilskuddskoder
#' @param to_path Path for the target translation table when copying produksjonstilskuddskoder
#'
#' @return Data frame with the translation table for produksjonstilskuddskoder.
#' @return \code{read_Pkode_2_text} A data frame with the translation table for Pkoder to description as read from the csv file.
#'     If not changing standard input to the function, the standard file at NVI's internal network is read.
#'
#'     \code{copy_Pkode_2_text} Copies the source translation table "Produksjonstilskuddskoder_UTF8.csv" to another location. If the target file
#'     already exists, the source file is only copied when it is newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' Pkode_2_text <- read_Pkode_2_text()
#'
#' # Copy standard file from standard location to the sub directory Data below the working directory
#' copy_Pkode_2_text(to_path = "./Data/")
#'
#' # Reading from the sub directory Data below the working directory
#' Pkode_2_text <- read_Pkode_2_text(from_path = "./Data")
#' }
#'
read_Pkode_2_text <- function(filename = "Produksjonstilskuddskoder_UTF8.csv",
                              from_path = paste0(set_dir_NVI("Prodtilskudd"), "StotteData/")) {

  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # assert_read_function(filename = filename, from_path = from_path)

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  assert_read_functions(filename = filename, from_path = from_path)
  # checkmate::assert_character(filename, len = 1, min.chars = 1, add = checks)
  # checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  #   checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }
  # checkmate::assert_file_exists(paste0(from_path, filename), access = "r", add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # reads header and identifies characters by using NVIdb::standardize_columns
  colclasses <- standardize_columns(data = file.path(from_path, filename), property = "colclasses")

  read_csv_file(filename = filename,
                from_path = from_path,
                options = list(colClasses = colclasses, fileEncoding = "UTF-8"))

}
