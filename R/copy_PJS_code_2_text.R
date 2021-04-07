#' @export
#' @rdname add_PJS_code_description

copy_PJS_codes_2_text <- function(filename = "PJS_codes_2_text.csv",
                                  from_path = paste0(set_dir_NVI("Provedata_Rapportering"), "FormaterteData/"),
                                  to_path = NULL) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # filename
  checkmate::assert_character(filename, len = 1, min.chars = 1, add = checks)
  # from_path / filename
  checkmate::assert_file_exists(paste0(from_path, filename), access = "r", add = checks)
  # to_path
  if (endsWith(to_path, "/")) {
    checkmate::assert_directory_exists(substr(to_path, 1, nchar(to_path) - 1), access = "r", add = checks)
  } else {
    checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  }
  # Report check-results
  checkmate::reportAssertions(checks)

  # COPY FILE ----
  copy_file_if_updated(filename = filename, from_path = from_path, to_path = to_path)

}
