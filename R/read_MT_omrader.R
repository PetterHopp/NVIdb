#' @export
#' @rdname add_MT_omrader

read_MT_omrader <- function(filename = list("komnr_2_MT_avdeling.csv", "MT_omrader.csv"),
                            from_path = base::paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/")) {


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
  komnr_2_MT_avdeling <- read_csv_file(filename = filename[[1]],
                                       from_path = from_path,
                                       options = list(colClasses = "character", fileEncoding = "UTF-8"))

  komnr_2_MT_avdeling <- komnr_2_MT_avdeling[, c("kommuneidentifikator", "forekomstidentifikator")]
  colnames(komnr_2_MT_avdeling) <- c("komnr", "MT_avdelingnr")
  komnr_2_MT_avdeling$MT_regionnr <- paste0(substr(komnr_2_MT_avdeling$MT_avdelingnr, 1, 3), "000")

  MT_omrader <- read_csv_file(filename = filename[[2]],
                              from_path = from_path,
                              options = list(colClasses = "character", fileEncoding = "UTF-8"))

  MT_omrader <- MT_omrader[, c("MT_IDnr", "MT_navn")]

  komnr_2_MT_avdeling <- merge(komnr_2_MT_avdeling,
                               MT_omrader,
                               by.x = "MT_avdelingnr",
                               by.y = "MT_IDnr",
                               all.x = TRUE)

  colnames(komnr_2_MT_avdeling)[which(colnames(komnr_2_MT_avdeling) == "MT_navn")] <- "MT_avdeling"


  komnr_2_MT_avdeling <- merge(komnr_2_MT_avdeling,
                               MT_omrader,
                               by.x = "MT_regionnr",
                               by.y = "MT_IDnr",
                               all.x = TRUE)

  colnames(komnr_2_MT_avdeling)[which(colnames(komnr_2_MT_avdeling) == "MT_navn")] <- "MT_region"

  komnr_2_MT_omrader <- komnr_2_MT_avdeling[, c("komnr", "MT_avdelingnr", "MT_avdeling", "MT_regionnr", "MT_region")]

  return(komnr_2_MT_omrader)
}
