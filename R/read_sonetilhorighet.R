#' @export
#' @rdname add_lokalitet

read_sonetilhorighet <- function(filename = "sonetilhorighet.txt",
                                 from_path = paste0(set_dir_NVI("EksterneDatakilder"), "Lokreg/FormaterteData/Soner/")) {

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
  df1 <- read_csv_file(filename = filename,
                       from_path = from_path,
                       options = list(colClasses = c("LokNr" = "character"),
                                      fileEncoding = "UTF-8"),
                       sep = "\t")

  return(df1)
}
