#' @export
#' @rdname add_kommune_fylke


copy_kommune_fylke <- function(filename = list("Kommune_UTF8.csv",
                                               "komnr_2_gjeldende_komnr_UTF8.csv",
                                               "Fylke_UTF8.csv"),
                               from_path = paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/"),
                               to_path = NULL) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # filename
  checkmate::assert_list(filename, len = 3, add = checks)
  # # from_path
  # checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  #   checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }
  # from_path / filename
  for (i in c(1:length(filename))) {
    checkmate::assert_file_exists(paste0(from_path, filename[[i]]), access = "r", add = checks)
  }
  # to_path
  if (endsWith(to_path, "/")) {
    checkmate::assert_directory_exists(substr(to_path, 1, nchar(to_path) - 1), access = "r", add = checks)
  } else {
    checkmate::assert_directory_exists(to_path, access = "r", add = checks)
  }
  # Report check-results
  checkmate::reportAssertions(checks)

  # COPY FILES ----
  for (i in c(1:length(filename))) {
    copy_file_if_updated(filename = filename[[i]], from_path = from_path, to_path = to_path)
  }

}
