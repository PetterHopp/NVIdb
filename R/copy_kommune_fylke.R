#' @export
#' @rdname add_kommune_fylke


copy_kommune_fylke <- function(filename = list("Kommune_UTF8.csv",
                                               "komnr_2_gjeldende_komnr_UTF8.csv",
                                               "Fylke_UTF8.csv"),
                               from_path = paste0(set_dir_NVI("GrunndataLand"), "FormaterteData/"),
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
  for (i in c(1:length(filename))) {
    copy_file_if_updated(filename = filename[[i]], from_path = from_path, to_path = to_path)
  }

}
