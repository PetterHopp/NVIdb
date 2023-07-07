#' @title Sets disease selection parameters
#' @description Sets the disease selection parameters and store them in a list
#'     object. The list follows a standardised named format and the elements can
#'     be used as input to 
#'     \ifelse{html}{\code{\link{build_query_hensikt}}}{\code{build_query_hensikt}},
#'     \ifelse{html}{\code{\link{build_query_one_disease}}}{\code{build_query_one_disease}}
#'     or
#'     \ifelse{html}{\code{\link{build_query_outbreak}}}{\code{build_query_outbreak}}.
#'
#' @details Saker in PJS that concern one infection / disease can be characterised
#'     by the "analytt" (at "konklusjon" and/or "resultat" level), specific "hensikter",
#'     a relevant "utbrudds_ID" and/or specific "metoder." These can be used to select
#'     saker in PJS and/or to structure and simplify the output from PJS.
#'
#'     One or more specific "hensiktkoder" may be input to the selection statement.
#'     With specific "hensiktkode" is meant a "hensiktkode" that will imply that the sample
#'     will be examined for specific infectious agent(s) or disease. One or more
#'     specific "metodekoder" may be input to the selection statement. With specific
#'     "metodekode" is meant a "metodekode" that implies an examination that will give one
#'     of the input 2 as a result. If sub-codes of "analyttkode" or "hensiktkode"
#'     should be included, end the code with \%.
#'
#'     The selection parameters can be input values for dedicated arguments. For input parameters
#'     \code{hensikt2select}, \code{utbrudd2select}, \code{metode2select}, 
#'     \code{analytt2select}, \code{art2select}, and \code{include_missing_art}, 
#'     the input may be given in a source file. This may be handy if the
#'     selection will be performed many times. It also gives the possibility of
#'     using a for loop that selects PJS-data and performs similar analyses for one
#'     disease at a time.
#'
#' @param hensikt2select [\code{character}]\cr
#' Specific "hensiktkoder" for the "analytt" in question. If sub-codes should
#'     be included, end the code with \%. Can be \code{NULL}.
#' @param hensikt2delete [\code{character}]\cr
#' "hensiktkoder" for which saker should be excluded
#'     If sub-codes should be included, end the code with \%. Can be \code{NULL}.
#' @param utbrudd2select [\code{character(1)}]\cr
#' "utbruddsID". Can be \code{NULL}.
#' @param metode2select [\code{character}]\cr
#' Specific "metodekoder for the "analytt" in question." Can be \code{NULL}.
#' @param analytt2select [\code{character}]\cr
#' "analyttkoder". If sub-codes should be included, end the code with \%.
#'     Can be \code{NULL}.
#' @param art2select [\code{character}]\cr
#' "artkoder". If sub-codes should be included, end the code with \%.  \code{NA} can be
#'     combined with another "artkode". Can be \code{NULL}.
#' @param include_missing_art [\code{character(1)}]\cr
#' Should missing art be included. Must be one of c("never", "always", "for_selected_hensikt").
#'     If NULL, it is set to "always" when \code{art2select} includes NA, else it is set to "never". 
#' @param file [\code{character(1)}]\cr
#' path and file name for an R script that can be sourced and that
#'     sets the parameters \code{hensikt2select}, \code{utbrudd2select}, \code{metode2select}, and
#'     \code{analytt2select}. Can be \code{NULL}.
#'
#' @return A named list with selection parameters that can be used to generate
#'     SQL selection-statements and facilitate structuring output from PJS.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Selection parameters for Pancreatic disease (PD)
#' selection_parameters <- set_disease_parameters(
#'   analytt2select = c("01220104%", "1502010235"),
#'   hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
#'   metode2select = c("070070", "070231", "010057", "060265")
#'   )
set_disease_parameters <- function(hensikt2select = NULL,
                                   hensikt2delete = NULL,
                                   utbrudd2select = NULL,
                                   metode2select = NULL,
                                   analytt2select = NULL,
                                   art2select = NULL,
                                   include_missing_art = NULL,
                                   file = NULL) {
  
  # SET SELECTION PARAMETERS ----
  # Import values from parameter file if exists
  if (!is.null(file)) {
    checkmate::assert_file(x = file)
    if (!is.null(file)) {
      script <- as.character(parse(file = file, encoding = "UTF-8"))
      
      script <- script[grepl(pattern = paste0("[^hensikt2select|^hensikt2delete|^analytt2select|^metode2select|",
                                              "^art2select|^utbrudd2select|^missing_art]",
                                              "[[:blank:]]*",
                                              "[=|<\\-]"),
                             script)]
      
      for (i in 1:length(script)) {
        eval(parse(text = script[i]))
      }
    }
  }
  
  # PREPARE INPUT BEFORE ARGUMENT CHECKING ----
  # when include_missing_art = NULL, set to "always" if NA included in art2select, else set to "never"
  if (is.null(include_missing_art)) {
    if (!is.null(art2select) && any(is.na(art2select))) {
      include_missing_art <- "always"
    } else {
      include_missing_art <- "never"
    }
  }
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  NVIcheckmate::assert_non_null(list(analytt2select, hensikt2select, utbrudd2select, file), add = checks)
  checkmate::assert_character(hensikt2select, min.chars = 2, max.chars = 15, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(hensikt2delete, min.chars = 2, max.chars = 15, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(utbrudd2select, max.chars = 5, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(metode2select, n.chars = 6, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(analytt2select, min.chars = 2, max.chars = 20, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(art2select, min.chars = 2, max.chars = 20, all.missing = FALSE, null.ok = TRUE, add = checks)
  # if (!is.null(art2select) && any(is.na(art2select))) {
  checkmate::assert_choice(include_missing_art, 
                           choices = c("never", "always", "for_selected_hensikt"), 
                           add = checks)
  # }
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # CREATE LIST WITH PARAMETER VALUES ----
  return(list("hensikt2select" = hensikt2select,
              "hensikt2delete" = hensikt2delete,
              "utbrudd2select" = utbrudd2select,
              "metode2select" = metode2select,
              "analytt2select" = analytt2select,
              "art2select" = art2select,
              "include_missing_art" = include_missing_art))
}

