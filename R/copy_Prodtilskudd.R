#' @export
#' @rdname read_Prodtilskudd

copy_Prodtilskudd <- function(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                              to_path = NULL,
                              Pkode_year = "last",
                              Pkode_month = "both",
                              extracted_date = NULL) {

  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # from_path
  checkmate::assert_string(from_path, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # to_path
  checkmate::assert_string(to_path, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(to_path, access = "r", add = checks)

  if (is.null(extracted_date)) {
    # Pkode_month
    checkmate::assert_subset(Pkode_month, choices = c("both", "last", "01", "03", "05", "07", "10", "12"), add = checks)
    # Pkode_year
    checkmate::assert(checkmate::check_integerish(as.numeric(Pkode_year[grep('[[:alpha:]]', Pkode_year, invert = TRUE)]),
                                                  lower = 1995,
                                                  upper = as.numeric(format(Sys.Date(), "%Y")),
                                                  any.missing = FALSE,
                                                  all.missing = FALSE,
                                                  unique = TRUE),
                      # checkmate::check_character(Pkode_year, min.chars = 4, min.len = 1, any.missing = FALSE),
                      checkmate::check_choice(Pkode_year, choices = c("last")),
                      add = checks)
  }
  # If extracted_date != NULL, then input "both" and "last" are not accepted
  if (!is.null(extracted_date)) {
    # Pkode_month
    NVIcheckmate::assert_subset_character(Pkode_month,
                                          choices = c("01", "03", "05", "07", "10", "12"),
                                          comment = "The inputs 'both' and 'last' are not accepted when 'extracted_date' is given",
                                          add = checks)
    # Pkode_year
    NVIcheckmate::assert_integerish(as.numeric(Pkode_year[grep('[[:alpha:]]', Pkode_year, invert = TRUE)]),
                                    lower = 1995,
                                    upper = as.numeric(format(Sys.Date(), "%Y")),
                                    any.missing = FALSE,
                                    all.missing = FALSE,
                                    unique = TRUE,
                                    comment = "The input 'last' is not accepted when 'extracted_date' is given",
                                    add = checks)
  }

  # Report check-results
  checkmate::reportAssertions(checks)



  # # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE SELECTED VERSIONS OF EXTRACTS FROM PKODEREGISTERET ----
  filelist <- select_prodtilskudd_files(from_path = from_path,
                                        Pkode_year = Pkode_year,
                                        Pkode_month = Pkode_month,
                                        extracted_date = extracted_date)

  # COPY FILES ----
  for (i in c(1:dim(filelist)[1])) {
    copy_file_if_updated(filename = filelist[i, "filename"], from_path = from_path, to_path = to_path)
  }

}
