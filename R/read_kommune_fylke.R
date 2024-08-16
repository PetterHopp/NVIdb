#' @export
#' @rdname add_kommune_fylke

# DESIGN ----
# Reads tables with kommune, fylke and original to current komnr
# generates at translation table with all information for each original kommunenr
# Use NVIdb:::add_new_column to build the table, thereby order is preserved and column position can be decided

read_kommune_fylke <- function(filename = list("komnr_2_gjeldende_komnr2_UTF8.csv",
                                               "Fylke_UTF8.csv"),
                               from_path = file.path(set_dir_NVI("GrunndataLand", slash = FALSE), "FormaterteData")) {
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
  # Read komnr to current komnr (old and new number)
  kommune_fylke <- read_csv_file(filename = filename[[1]],
                                 from_path = from_path,
                                 options = list(colClasses = "character", fileEncoding = "UTF-8"))

  # Read fylke (nr and name)
  fylke <- read_csv_file(filename = filename[[2]],
                         from_path = from_path,
                         options = list(colClasses = "character", fileEncoding = "UTF-8"))

  ### Generate one table with translation from kommunenr to original and current kommune and fylke ----
  # Add original fylkenr
  kommune_fylke$fylkenr <- substr(kommune_fylke$komnr, 1, 2)

  # Add original fylke
  kommune_fylke <- merge(x = kommune_fylke,
                         y = fylke[, c("fylkenr", "fylke")],
                         by = "fylkenr",
                         all.x = TRUE)
  kommune_fylke$fylkenr_in_period <- substr(kommune_fylke$komnr_in_period, 1, 2)

  colnames(fylke)[which(colnames(fylke) == "fylke")] <- "fylke_in_period"
  # Add current kommunenr
  kommune_fylke <- merge(x = kommune_fylke,
                         y = fylke[, c("fylkenr", "fylke_in_period")],
                         by.x = "fylkenr_in_period",
                         by.y = "fylkenr",
                         all.x = TRUE)

  # Select and order necessary columns
  kommune_fylke <- kommune_fylke[, c("komnr", "kommune", "fylkenr", "fylke",
                                     "from_year", "to_year",
                                     "komnr_in_period", "kommune_in_period",
                                     "fylkenr_in_period", "fylke_in_period")]

  kommune_fylke <- kommune_fylke[order(kommune_fylke$from_year, kommune_fylke$komnr), ]

  return(kommune_fylke)
}
