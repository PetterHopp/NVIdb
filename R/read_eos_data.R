#' @title Read EOS data from RaData
#' @description Reads EOS data from RaData. Includes historical data if these exists.
#'     It is possible to limit the data to one or more years.
#' @details read_eos_data uses \code{data.table::fread} to read the data with the 
#'     settings \code{showProgress = FALSE} and \code{data.table = FALSE}. Other 
#'     arguments can be passed to \code{data.table::fread} if necessary.
#' 
#' @param from_path Path for raw data from eos_data
#' @param eos_table_name The name of the table with eos raw data
#' @param year The years to be included in the result. Defaults to 
#'     \code{NULL}, i.e. no selection.
#' @param colClasses The class of the columns, as in utils::read.table, Defaults to 
#'     \code{"character"}.
#' @param encoding The encoding. Defaults to \code{"UTF-8"}.
#' @param \dots	Other arguments to be passed to \code{data.table::fread}.
#'
#' @return A data frame with data from EOS.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#### Function for reading EOS data from RaData. Also reads historic data if file exists
# eos_table_name er det samme som TABLE_NAME i EOS-databasen 
# (Se all_views_eos.csv i RaData-mappen)
read_eos_data <- function(eos_table_name, 
                          from_path = paste0(set_dir_NVI("OKprogrammer"), "OKstatistikkApp/RaData"),
                          year = NULL,
                          colClasses = "character", 
                          encoding = "UTF-8",
                          ...) {
  
  
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  ## eos_table_name
  checkmate::assert_string(eos_table_name, min.chars = 1, add = checks)
  ## from_path / filename
  checkmate::assert_file_exists(file.path(from_path, paste0(eos_table_name, ".csv")), access = "r", add = checks)
  ## year
  checkmate::assert_integerish(as.numeric(year),
                               lower = 1995,
                               upper = as.numeric(format(Sys.Date(), "%Y")),
                               any.missing = FALSE,
                               all.missing = FALSE,
                               unique = TRUE,
                               add = checks)
  ## colClasses
  checkmate::assert_character(colClasses, min.chars = 1, min.len = 1, 
                              any.missing = FALSE,
                              all.missing = FALSE, 
                              null.ok = TRUE,
                              add = checks)
  
  ## encoding
  checkmate::assert_subset(encoding, choices = c("UTF-8", "latin1"), add = checks)
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  
  # Import of data from csv-files retrieved from EOS
  # EOS inneholder data fra siste og foregående år og antas å være i kontinuerlig endring
  eos_data <-  data.table::fread(file = file.path(from_path, paste0(eos_table_name, ".csv")), 
                                 colClasses = colClasses, 
                                 encoding = encoding,
                                 showProgress = FALSE,
                                 data.table = FALSE,
                                 ...)
  
  column_names <- colnames(eos_data)
  colnames(eos_data) <- tolower(column_names)
  
  # Import av historiske data fra EOS
  # EOS-historikkfiler oppdateres 1 x årlig. Data hentes ut etter oppdatering og lagres i csv-filer
  # Hentes derfor fra csv-filen for bruk i OK-statistikken
  if (file.exists(file.path(from_path, paste0(eos_table_name, "_historikk.csv")))) {
    eos_data_historikk <-  data.table::fread(file = file.path(from_path, paste0(eos_table_name, "_historikk.csv")), 
                                             colClasses = colClasses, 
                                             encoding = encoding,
                                             showProgress = FALSE,
                                             data.table = FALSE,
                                             ...)
    
    # Fjerner år fa historikktabellen som også ligger i driftstabellen. Setter deretter sammen tabellene
    first_year_in_eos_data <- min(substr(eos_data$saksnr, 1, 4))
    colnames(eos_data_historikk) <- tolower(column_names)
    eos_data_historikk <- subset(eos_data_historikk, substr(eos_data_historikk$saksnr, 1, 4) < first_year_in_eos_data)
    eos_data <- rbind(eos_data, eos_data_historikk)
  }  
  
  if (!is.null(year)) {
    eos_data <- subset(eos_data, substr(eos_data$saksnr, 1, 4) %in% year)
  }
  
  colnames(eos_data) <- column_names
  return(eos_data)
}

#