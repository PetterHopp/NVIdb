#' @export
#' @rdname add_produsent_properties

copy_prodnr_2_current_prodnr <- function(filename = "Prodnr2GjeldendeProdnr.csv",
                                         from_path = paste0(set_dir_NVI("Prodregister"), "FormaterteData/"),
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
