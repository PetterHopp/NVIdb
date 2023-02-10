#' @title Standardising EOS-data 
#' @description Standardising EOS-data. This standardising should always be performed.
#'     Otherwise the summary numbers can be wrong.
#'
#' @details The function performs the following standardising of data extracted from EOS:
#' \itemize{
#'   \item The column names are standardised using \code{\link{standardize_columns}}.
#'   \item Numeric variables are transformed to numbers.
#'   \item Date variables are transformed to date format.
#'   \item Character variables are trimmed for leading and trailing spaces.
#'   \item Double registrations of a Sak due to the municipality being divided 
#'         between two Food Safety Authority offices are merged into one and for 
#'         these the information on Food Safety Authority office is removed.
#'   \item Breed is transformed to species. 
#'   }
#'
#' @param data Data frame with data retrieved from EOS.
#' @param dbsource If specified, this will be used for fetching standard column
#'     names by \code{\link{standardize_columns}}. Defaults to the name of data. 
#' @param standards  data.frame The translation table to standard column names. 
#'     Defaults to \code{NULL}. 
#' @param breed_to_species [logical(1)] If \code{TRUE}, breed is translated back 
#'     to species. Defaults to \code{TRUE)}. 
#' @param adjust_n_examined [logical(1)] If \code{TRUE}, the number of examined 
#'     samples is adjusted so it is maximum number of received samples. Defaults 
#'     to \code{TRUE)}. 
#' @param \dots    Other arguments to be passed to \code{standardize_columns}.
#' 
#' @return data frame with standardized EOS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @author Johan Åkerstedt Johan.Akerstedt@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Standardizing sak_m_res
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
  cols_2_modify <- intersect(colnames(data), c("aar", "ant_prover", grep("ant_und", colnames(data), value = TRUE)))
  data[, cols_2_modify] <- lapply(data[, cols_2_modify], as.numeric)
  
  # Change to date for date-variables
  # Performed before trimming character variables to reduce variables that needs to be trimmed
  cols_2_modify <- intersect(colnames(data), c("mottatt", "uttatt", "avsluttet", "sist_endret"))
  data[, cols_2_modify] <- lapply(data[, cols_2_modify], as.Date, format = "%d.%m.%y")
  
  # Trim character variables
  cols_2_modify <- names(data)[vapply(data, is.character, logical(1))]
  data[, cols_2_modify] <- lapply(data[, cols_2_modify], trimws)
  
  # remove double rows due to one Sak being assigned to two MT offices 
  data <- data %>% 
    dplyr::add_count(saksnr, name = "ant_per_sak") %>% 
    dplyr::add_count(saksnr, rekvirent_nr, name = "ant_per_MT") 
  
  rownums <- data[which(data$ant_per_sak == (2 * data$ant_per_MT) )]
  data[rownums, c("id", "rekvirent_type", "rekvirent_nr", "rekvirent")] <- c(NA_integer_, NA_character_, NA_character_, NA_character_)
  data[, c("ant_per_sak", "ant_per_MT")] <- c(NULL, NULL)
  data <- unique(data)
  # %>% 
  #   dplyr::mutate(id = dplyr::case_when(ant_per_sak == (2 * ant_per_MT) ~ NA_integer_, 
  #                                       TRUE ~ as.integer(id))) %>%
  #   dplyr::mutate(rekvirent_type = dplyr::case_when(ant_per_sak == (2 * ant_per_MT) ~ NA_character_, 
  #                                                   TRUE ~ rekvirent_type)) %>%
  #   dplyr::mutate(rekvirent_nr = dplyr::case_when(ant_per_sak == (2 * ant_per_MT) ~ NA_character_, 
  #                                                 TRUE ~ rekvirent_nr)) %>%
  #   dplyr::mutate(rekvirent = dplyr::case_when(ant_per_sak == (2 * ant_per_MT) ~ NA_character_, 
  #                                              TRUE ~ rekvirent)) %>%
  #   dplyr::select(-c(ant_per_sak, ant_per_MT)) %>%
  #   dplyr::distinct() 
  
  # backtranslate breed to species
  if (isTRUE(breed_to_species)) {
    add_PJS_code_description(data = data, 
                             PJS_variable_type = "artrase",
                             code_colname = "art",
                             new_column = "artkode",
                             backward = TRUE) 
    
    add_PJS_code_description(data = data, 
                             PJS_variable_type = "art",
                             code_colname = "artkode",
                             new_column = "art",
                             overwrite = TRUE)
    data$artkode <- NULL
  } 
  
  # adjust number of examined
  if (isTRUE(adjust_n_examined)) {
    ant_und <- grep("ant_und", colnames(data), value = TRUE) 
    for (i in ant_und) {
      data[, i] <- pmin(data[, c("antall_prover", i)]) 
    } 
  } 
  return(data) 
}

