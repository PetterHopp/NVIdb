#' @export
#' @rdname add_PJS_code_description

read_PJS_codes_2_text <- function(filename = "PJS_codes_2_text.csv",
                                  from_path = paste0(set_dir_NVI("Provedata_Rapportering"), "FormaterteData/")) {

  # Argument checking
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_character(filename, len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  if (endsWith(from_path, "/")) {
    checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  } else {
    checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  }
  checkmate::assert_file_exists(paste0(from_path, filename), access = "r", add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  PJS_codes_2_text <- read_csv_file(filename = filename,
                                    from_path = from_path,
                                    options = list(colClasses = "character", fileEncoding = "UTF-8"))

  # Remove double "" that have replaced single when saving as csv-file
  PJS_codes_2_text$navn <- gsub('\"\"', "\"", PJS_codes_2_text$navn)

  return(PJS_codes_2_text)

}
