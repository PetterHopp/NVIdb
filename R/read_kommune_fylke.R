#' @export
#' @rdname add_kommune_fylke

# DESIGN ----
# Reads tables with kommune, fylke and original to current komnr
# generates at translation table with all information for each original kommunenr
# Use NVIdb:::add_new_column to build the table, thereby order is preserved and column position can be decided

read_kommune_fylke <- function(filename = list("Kommune_UTF8.csv",
                                               "komnr_2_gjeldende_komnr_UTF8.csv",
                                               "Fylke_UTF8.csv"),
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
  ## Read files with kommune and fylke data
  # Read kommune (nr and name)
  kommune <- read_csv_file(filename = filename[[1]],
                           from_path = from_path,
                           options = list(colClasses = "character", fileEncoding = "UTF-8"))

  # Read komnr to current komnr (old and new number)
  kommunenr_2_current_kommunenr <- read_csv_file(filename = filename[[2]],
                                                 from_path = from_path,
                                                 options = list(colClasses = "character", fileEncoding = "UTF-8"))

  # Read fylke (nr and name)
  fylke <- read_csv_file(filename = filename[[3]],
                         from_path = from_path,
                         options = list(colClasses = "character", fileEncoding = "UTF-8"))

  ### Generate one table with translation from kommunenr to original and current kommune and fylke ----
  # Add original fylkenr
  kommune_fylke <- kommune
  kommune_fylke$fylkenr <- substr(kommune_fylke$komnr, 1, 2)

  # Add original fylke
  kommune_fylke <- add_new_column(kommune_fylke,
                                  ID_column = "fylkenr",
                                  new_colname = "fylke",
                                  translation_tables = list(fylke),
                                  ID_column_translation_table = "fylkenr",
                                  to_column_translation_table = "fylke")

  # Add current kommunenr
  kommune_fylke <- add_new_column(kommune_fylke,
                                  ID_column = "komnr",
                                  new_colname = "gjeldende_komnr",
                                  translation_tables = list(kommunenr_2_current_kommunenr),
                                  ID_column_translation_table = "komnr",
                                  to_column_translation_table = "gjeldende_komnr",
                                  position = "last")

  # Add name of current kommune
  kommune_fylke <- add_new_column(kommune_fylke,
                                  ID_column = "gjeldende_komnr",
                                  new_colname = "gjeldende_kommune",
                                  translation_tables = list(kommune),
                                  ID_column_translation_table = "komnr",
                                  to_column_translation_table = "kommune")

  # Add current fylkenr
  kommune_fylke$gjeldende_fylkenr <- substr(kommune_fylke$gjeldende_komnr, 1, 2)

  # Add current fylke
  kommune_fylke <- add_new_column(kommune_fylke,
                                  ID_column = "gjeldende_fylkenr",
                                  new_colname = "gjeldende_fylke",
                                  translation_tables = list(fylke),
                                  ID_column_translation_table = "fylkenr",
                                  to_column_translation_table = "fylke")

  # Remove unnecessary columns
  kommune_fylke$kategoritype <- NULL

  return(kommune_fylke)
}
