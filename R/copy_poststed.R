#' @export
#' @rdname add_poststed

copy_poststed <- function(filename = "Poststed_UTF8.csv",
                          from_path = paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/"),
                          to_path = NULL) {

  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)

  # ARGUMENT CHECKING ----
  assert_copy_function(filename = filename,
                       from_path = from_path,
                       to_path = to_path)
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()
  #
  # # Perform checks
  # # filename
  # checkmate::assert_character(filename, len = 1, min.chars = 1, add = checks)
  # # from_path / filename
  # checkmate::assert_file_exists(file.path(from_path, filename), access = "r", add = checks)
  # # to_path
  # checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  #
  # # Report check-results
  # checkmate::reportAssertions(checks)

  # COPY FILE ----
  copy_file_if_updated(filename = filename, from_path = from_path, to_path = to_path)

}
