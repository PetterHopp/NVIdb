#' @export
#' @rdname add_produsent_properties

read_prodnr_2_coordinates <- function(filename = "Prodnr2Koordinater.csv",
                                      from_path = paste0(set_dir_NVI("Prodregister"), "FormaterteData/")) {
  
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  assert_read_function(filename = filename, from_path = from_path)
  
  # # Argument checking
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()
  # # Perform checks
  # checkmate::assert_character(filename, len = 1, min.chars = 1, add = checks)
  # checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  #   checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }
  # checkmate::assert_file_exists(paste0(from_path, filename), access = "r", add = checks)
  # # Report check-results
  # checkmate::reportAssertions(checks)

  df1 <- read_csv_file(filename = filename,
                       from_path = from_path,
                       options = list(colClasses = "character",
                                      fileEncoding = "UTF-8"))

  return(df1)
}

