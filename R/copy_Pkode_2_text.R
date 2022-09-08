#' @export
#' @rdname read_Pkode_2_text

copy_Pkode_2_text <- function(filename = "Produksjonstilskuddskoder2_UTF8.csv",
                              from_path = paste0(set_dir_NVI("Prodtilskudd"), "StotteData/"),
                              to_path = NULL) {

  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  to_path <- sub("/+$|\\\\+$", "", to_path)

  # ARGUMENT CHECKING ----
  assert_copy_function(filename = filename,
                       from_path = from_path,
                       to_path = to_path)

  # COPY FILE ----
  copy_file_if_updated(filename = filename, from_path = from_path, to_path = to_path)

}
