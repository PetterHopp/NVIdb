#' @export
#' @rdname read_Prodtilskudd

copy_Prodtilskudd <- function(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                              to_path = NULL,
                              Pkode_year = "last",
                              Pkode_month = "both") {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # from_path
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  if (endsWith(from_path, "/")) {
    checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  } else {
    checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  }
  # to_path
  if (endsWith(to_path, "/")) {
    checkmate::assert_directory_exists(substr(to_path, 1, nchar(to_path) - 1), access = "r", add = checks)
  } else {
    checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  }
  checkmate::assert_subset(Pkode_month, choices = c("both", "last", "01", "03", "05", "07", "10", "12"), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  checkmate::assert(checkmate::check_integerish(as.numeric(Pkode_year[which(!grepl('[:alpha:]', Pkode_year))]),
                                                lower = 1995,
                                                upper = as.numeric(format(Sys.Date(), "%Y")),
                                                any.missing = FALSE,
                                                unique = TRUE),
                    # checkmate::check_character(Pkode_year, min.chars = 4, min.len = 1, any.missing = FALSE),
                    checkmate::check_choice(Pkode_year, choices = c("last")))


  # # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE SELECTED VERSIONS OF EXTRACTS FROM PKODEREGISTERET ----
  filelist <- select_prodtilskudd_files(from_path = from_path,
                                        Pkode_year = Pkode_year,
                                        Pkode_month = Pkode_month)

  # COPY FILES ----
  for (i in c(1:dim(filelist)[1])) {
    copy_file_if_updated(filename = filelist[i, "filename"], from_path = from_path, to_path = to_path)
  }

}
