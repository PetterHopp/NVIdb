#' @export
#' @rdname add_produsent_properties

read_prodnr_2_coordinates <- function(filename = "Prodnr2Koordinater.csv",
                                      from_path = paste0(set_dir_NVI("Prodregister"), "FormaterteData/"),
                                      ...) {

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
  # df1 <- read_csv_file(filename = filename,
  #                      from_path = from_path,
  #                      options = list(colClasses = "character",
  #                                     fileEncoding = "UTF-8"))
  df1 <- data.table::fread(file = file.path(from_path, filename),
                           colClasses = "character",
                           encoding = "UTF-8",
                           showProgress = FALSE,
                           data.table = FALSE,
                           ...)

  return(df1)
}
