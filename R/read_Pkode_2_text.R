#' @title Manage translation table for produksjonstilskuddskoder (Pkoder)
#' @description Read and copy the translation table for produksjonstilskuddskoder
#'     (Pkoder).
#' @details The translation table for Pkoder contains the Pkode, descriptive
#'     text, unit of interest (Dyr), whether the code counts unique animals or
#'     not (used when summarising number of animals), and sortering to order the
#'     Pkoder. The register covers 2017 and later.
#'
#'     \code{read_Pkode_2_text} reads the file "Produksjonstilskuddskoder2_UTF8.csv"
#'     into a data frame. The default is to read the file from NVI's internal
#'     network. If changing the from_path, the function can be used to read the
#'     translation file from other directories. This can be useful if having a
#'     stand alone app with no connection the NVI's internal network. In other
#'     cases, it should be avoided.
#'
#'     \code{copy_Pkode_2_text} copies the file "Produksjonstilskuddskoder2_UTF8.csv"
#'     to a given location.
#'
#' @param filename [\code{character(1)}]\cr
#' Name of the translation table. Defaults to "Produksjonstilskuddskoder2_UTF8.csv".
#' @param from_path [\code{character(1)}]\cr
#' Path for the translation table for produksjonstilskuddskoder. Defaults to
#'     standard directory at the NVI network.
#' @param to_path [\code{character(1)}]\cr
#' Path for the target translation table when copying produksjonstilskuddskoder.
#' @param keep_old_names [\code{logical(1)}]\cr
#' Keep old column names as were used as standard in NVIdb <= v0.7.1. Defaults
#'     to \code{FALSE}.
#'
#' @return \code{read_Pkode_2_text} A data frame with the translation table for
#'     Pkoder to description as read from the csv file. If not changing standard
#'     input to the function, the standard file at NVI's internal network is read.
#'
#'     \code{copy_Pkode_2_text} Copies the source translation table
#'     "Produksjonstilskuddskoder2_UTF8.csv" to another location. If the target
#'     file already exists, the source file is only copied when it is newer than
#'     the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' Pkode_2_text <- read_Pkode_2_text()
#'
#' # Copy standard file from standard location to the sub directory Data below the working directory
#' copy_Pkode_2_text(to_path = "./Data/")
#'
#' # Reading from the sub directory Data below the working directory
#' Pkode_2_text <- read_Pkode_2_text(from_path = "./Data")
#' }
#'
read_Pkode_2_text <- function(filename = "Produksjonstilskuddskoder2_UTF8.csv",
                              from_path = paste0(set_dir_NVI("Prodtilskudd"), "StotteData/"),
                              keep_old_names = FALSE) {

  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  checkmate::assert_flag(x = keep_old_names, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # READ DATA ----
  # reads header and identifies characters by using NVIdb::standardize_columns
  colclasses <- standardize_columns(data = file.path(from_path, filename), property = "colclasses")

  Pkoder <- read_csv_file(filename = filename,
                          from_path = from_path,
                          options = list(colClasses = colclasses, fileEncoding = "UTF-8"))

  if (isTRUE(keep_old_names)) {
    standard_names <- c("soknadaar", "telledato", "Pkodeart", "Pkode",
                        "beskrivelse", "enhet", "unike_dyr", "sortering")
    if (isTRUE(checkmate::check_subset(x = standard_names,
                                       choices = colnames(Pkoder)))) {
      Pkoder <- Pkoder[, c(standard_names, base::setdiff(colnames(Pkoder), standard_names))]
      colnames(Pkoder) <- c("S\u00F8knads\u00E5r", "Telledato", "Art", "Kode",
                            "Beskrivelse", "Enhet", "Seleksjon", "Sortering",
                            base::setdiff(colnames(Pkoder), standard_names))
      Pkoder$Telledato <- format(as.Date(Pkoder$Telledato), "%d.%m")
      Pkoder[which(Pkoder$Art == "Svin"), "Art"] <- "Gris"
      Pkoder$Enhet <- snakecase::to_sentence_case(Pkoder$Enhet)
    }
  }
  return(Pkoder)
}
