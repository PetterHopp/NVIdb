#' @title Standardizing PJS-data
#' @description Standardizing PJS-data. This standardizing should always be performed.
#'     Other functions used for further preparation of PJSdata, like
#'     \ifelse{html}{\code{\link{choose_PJS_levels}}}{\code{choose_PJS_levels}}
#'     , and
#'     \ifelse{html}{\code{\link{exclude_from_PJSdata}}}{\code{exclude_from_PJSdata}}
#'     will not work as intended unless the column names are standardized.
#'
#' @details The function performs the following standardizing of data extracted from PJS:
#' \itemize{
#'   \item The unnecessary columns konkl_provenr and vet_distriktnr are removed.
#'   \item The column names are standardized using
#'     \ifelse{html}{\code{\link{standardize_columns}}}{\code{standardize_columns}}.
#'   \item Numeric variables are transformed to numbers.
#'   \item Date variables are transformed to date format.
#'   \item Character variables are trimmed for leading and trailing spaces.
#'   \item The variables saksnr and, if possible, fagnr are generated.
#'   \item Test data, i.e. saker with ansvarlig_seksjon in c("14", "99") are deleted.
#'   }
#'
#' @param PJSdata [\code{data.frame}]\cr
#' Data retrieved from PJS.
#' @param dbsource [\code{character(1)}]\cr
#' The table that is the source of data. This will be used for fetching
#'     standard column names by
#'     \ifelse{html}{\code{\link{standardize_columns}}}{\code{standardize_columns}}
#'     and should be the name of the data source as registered in the
#'     "column_standards" table. Defaults to "v2_sak_m_res".
#'
#' @return \code{data.frame} with standardized PJS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @author Johan Åkerstedt Johan.Akerstedt@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Standardizing sak_m_res
#' sak_m_res <- standardize_PJSdata(PJSdata = sak_m_res)
#' }
#'
standardize_PJSdata <- function(PJSdata, dbsource = "v2_sak_m_res") {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # pjsDATA
  checkmate::assert_data_frame(PJSdata, add = checks)
  # dbsource
  checkmate::assert_character(dbsource, len = 1, min.chars = 1, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  # PERFORM STANDARDIZATION ----
  # Remove unnecessary columns
  PJSdata$konkl_provenr <- NULL
  PJSdata$vet_distriktnr <- NULL

  # Standardize column names
  PJSdata <- NVIdb::standardize_columns(data = PJSdata, dbsource = dbsource, property = "colnames")

  # Change to numeric for ID-numbers and counts
  # Done before trimming character variables to reduce variables that needs to be trimmed
  cols_2_modify <- intersect(colnames(PJSdata), c("aar", "innsendelsenr", "provenr", "delprovenr", "undnr",
                                                  "resnr", "subundnr", "subresnr", "konklnr",
                                                  "ant_prover", "ant_i_samleprove", "ant_delprover", "ant_i_samledelprove"))
  PJSdata[, cols_2_modify] <- lapply(PJSdata[, cols_2_modify], as.numeric)

  # Change to date for date-variables
  # Done before trimming character variables to reduce variables that needs to be trimmed
  cols_2_modify <- intersect(colnames(PJSdata), c("mottatt", "uttatt", "avsluttet", "sak_forst_avsluttet",
                                                  "uttatt_parprove", "mottatt_parprove",
                                                  "und_godkjent", "und_avsluttet",
                                                  "subund_godkjent", "subund_avsluttet", "subund_startet"))
  PJSdata[, cols_2_modify] <- lapply(PJSdata[, cols_2_modify], as.Date, format = "%d.%m.%y")

  # Trim character variables
  cols_2_modify <- names(PJSdata)[vapply(PJSdata, is.character, logical(1))]
  PJSdata[, cols_2_modify] <- lapply(PJSdata[, cols_2_modify], trimws)

  # Generate saksnr and fagnr
  PJSdata$saksnr <- paste(PJSdata$aar, PJSdata$ansvarlig_seksjon, PJSdata$innsendelsenr, sep = "-")
  if ("fagkode" %in% cols_2_modify & "fagnr" %in% cols_2_modify) {
    PJSdata$fagnr <- paste(PJSdata$aar, PJSdata$ansvarlig_seksjon, paste0(PJSdata$fagkode, PJSdata$fagnr), sep = "-")
  }

  # Delete test data, i.e. saker with ansvarlig_seksjon in c("14", "99")
  PJSdata <- subset(PJSdata, !PJSdata$ansvarlig_seksjon %in% c("14", "99"))

return(PJSdata)
}
