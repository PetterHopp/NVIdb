#' @export
#' @rdname add_poststed

read_poststed <- function(filename = "Poststed_UTF8.csv",
                          from_path = paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/")) {

    # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # READ DATA ----
  read_csv_file(filename = filename,
                from_path = from_path,
                options = list(colClasses = c(postnr = "character", komnr = "character"),
                               fileEncoding = "UTF-8"))

}
