#' @title Standardising EOS-data 
#' @description Standardising EOS-data. This standardising should always be performed.
#'     Otherwise summary numbers can be wrong.
#'
#' @details The function performs the following standardising of data extracted from EOS:
#' \itemize{
#'   \item The column names are standardised using \code{\link{standardize_columns}}.
#'   \item Numeric variables are transformed to numbers.
#'   \item Date variables are transformed to dates.
#'   \item Double registrations of a "Sak" due to the municipality being divided 
#'         between two Food Safety Authority office, are merged into one and for 
#'         these, the information on Food Safety Authority office is removed.
#'   \item Breed is transformed to species. 
#'   \item Number of examined samples are corrected so it don't exceed the number 
#'         of received samples.
#'   }
#' Transformation from breed to species is only performed when species is included
#'     in the data. You need to import the translation table for PJS-codes to perform
#'     the translation, use \code{PJS_codes_2_text <- read_PJS_codes_2_text()}.
#'     
#' Correction of number of tested samples is only done when both number of received
#'     and number of tested are included in the data.
#' 
#' @param data [\code{data.frame}]\cr
#'     The data retrieved from EOS.
#' @param dbsource [\code{character(1)}]\cr
#'     If specified, this will be used for fetching standard column names by  
#'    \code{\link{standardize_columns}}. Defaults to the name of data. 
#' @param standards [\code{data.frame}]\cr
#'     The translation table to standard column names. Defaults to \code{NULL}. 
#' @param breed_to_species [\code{logical(1)}]\cr
#'     If \code{TRUE}, breed is translated back to species. Defaults to \code{TRUE)}. 
#' @param adjust_n_examined [\code{logical(1)}]\cr
#'     If \code{TRUE}, the number of examined samples is adjusted so it is at maximum 
#'     the number of received samples. Defaults to \code{TRUE}. 
#' @param \dots Other arguments to be passed to \code{\link{standardize_columns}}.
#' 
#' @return \code{data.frame} with standardized EOS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Standardizing proveresultat_bse
#' PJS_codes_2_text <- read_PJS_codes_2_text()
#' proveresultat_bse <- standardize_eos_data(data = proveresultat_bse)
#' }
#'
standardize_eos_data <- function(data,
                                 dbsource = deparse(substitute(data)), 
                                 standards = NULL, 
                                 breed_to_species = TRUE, 
                                 adjust_n_examined = TRUE,
                                 ...) {
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_data_frame(data)
  checkmate::assert_character(dbsource, len = 1, min.chars = 1, add = checks)
  checkmate::assert_data_frame(standards, null.ok = TRUE, add = checks)
  checkmate::assert_flag(breed_to_species, add = checks) 
  checkmate::assert_flag(adjust_n_examined, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # standardize_columns
  data <- standardize_columns(data = data,
                              dbsource = dbsource,
                              standards = standards,
                              property = "colnames",
                              ...) 
  
  # Change to numeric for ID-numbers and counts
  # Performed before trimming character variables to reduce variables that needs to be trimmed
  cols_2_modify <- intersect(colnames(data), c("lopenr", "aar", "innsendelsenr", "avvik_i_registrering", 
                                               "ant_prover", grep("ant_und", colnames(data), value = TRUE)))
  data[, cols_2_modify] <- lapply(data[, cols_2_modify], as.numeric)
  
  # Change to date for date-variables
  # Performed before trimming character variables to reduce variables that needs to be trimmed
  cols_2_modify <- intersect(colnames(data), c("mottatt", "uttatt", "avsluttet", "sist_endret"))
  for (dato in cols_2_modify) {
  data[, dato] <- substr(data[, dato], 1, 10)
  }
  data[, cols_2_modify] <- lapply(data[, cols_2_modify], as.Date, format = "%Y-%m-%d")
  
  # # Trim character variables
  # cols_2_modify <- names(data)[vapply(data, is.character, logical(1))]
  # data[, cols_2_modify] <- lapply(data[, cols_2_modify], trimws)
  
  # remove double rows due to one Sak being assigned to two MT offices 
  # It varies which variables keep the information on MT office
  groupvar <- intersect(c("rekvirent", "rekvirentnr", "mt_avdelingnr", "mt_avdeling"), 
                            colnames(data))
  data <- data %>% 
    dplyr::add_count(dplyr::across("saksnr"), name = "ant_per_sak") %>% 
    dplyr::add_count(dplyr::across(c("saksnr", groupvar)), name = "ant_per_MT") 
  
  rownums <- which(data$ant_per_sak == (2 * data$ant_per_MT) )
  column_names <- intersect(c("lopenr", "rekvirent", "rekvirentnr", "mt_avdelingnr", "mt_avdeling"), 
                            colnames(data))
  data[rownums, column_names] <- rep(NA_integer_, length(column_names))
  data[, c("ant_per_sak", "ant_per_MT")] <- c(NULL, NULL)
  data <- unique(data)

  # backtranslate breed to species
  if (isTRUE(breed_to_species) & "art" %in% colnames(data)) {
    PJS_codes_2_text <- read_PJS_codes_2_text()
    data <- add_PJS_code_description(data = data, 
                             PJS_variable_type = "artrase",
                             code_colname = "art",
                             new_column = "artkode",
                             backward = TRUE) 
    
    data <- add_PJS_code_description(data = data, 
                             PJS_variable_type = "art",
                             code_colname = "artkode",
                             new_column = "art",
                             position = "keep",
                             overwrite = TRUE)
    data$artkode <- NULL
  } 
  
  # adjust number of examined
  if (isTRUE(adjust_n_examined) & "ant_prover" %in% colnames(data)) {
    ant_und <- grep("ant_und", colnames(data), value = TRUE)
    for (i in ant_und) {
      data[, i] <- pmin(data[, "ant_prover"], data[, i])
    }
  }
  return(data) 
}

