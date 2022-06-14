#' @title Sets disease selection parameters
#' @description Sets the disease selection parameters in a list object. The list
#'     follows a standardised and named format and can be used as input to
#'     build_query_one_disease or build_query_hensikt.
#'
#' @details Saker in PJS that concern one infection / disease can be characterised
#'     by the analytt (at konklusjon and/or resultat level), specific hensikter,
#'     utbrudds_ID and/or specific metoder. These can be used to select saker in
#'     PJS and/or structurise and simplify the output from PJS.
#'
#'     One or more specific hensikter may be input to the selection statement.
#'     With specific hensikt is meant a hensikt that will imply
#'     that the sample will be examined for the infectious agent or disease.
#'     One or more specific metoder may be input to the selection statement.
#'     With specific metode is meant a metode that implies an
#'     examination that will give one of the input analytter as a result.
#'
#'     The selection parameters can be input the function as such. Alternatively,
#'     a source file for the input parameters hensikt2select, utbrudd2select,
#'     metode2select and analytt2select may be given. This may be handy if the
#'     selection will be performed many times. It also gives the possibility of
#'     using a for loop for selecting and analysing data for several similar
#'     diseases at one time.
#'
#'     The ouput is a named list with the selection parameters. This list can be
#'     used as input build_query_one_disease or build_query_hensikt.
#'
#' @param year One year or a vector with years giving the first and last years
#'     that should be selected as integer.
#' @param hensikt2select Vector with specific hensikter. If sub-hensikter should
#'     be included, end the code with \%. Can be \code{NULL}.
#' @param utbrudd2select String with an utbrudd ID. Can be \code{NULL}.
#' @param metode2select Vector with specific metoder. Can be \code{NULL}.
#' @param analytt2select Vector with  one or more analyttkode given as a character.
#'     If sub-analytter should be included, end the code with \%. Can be \code{NULL}.
#' @param file An R script that can be sourced and that sets the parameters
#'     hensikt2select, utbrudd2select, metode2select, analytt2select. Can be
#'     \code{NULL}.
#'
#' @return A list with select-statement fom v2_sak_m_res and v_sakskonklusjon to
#'     be included in a \code{RODBC::sqlQuery}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Selection parameters for Pancreatic disease (PD)
#' selection_parameters <- set_disease_selection_parameters(
#'   year = 2020,
#'   analytt2select = c("01220104%", "1502010235"),
#'   hensikt2select = c("0100108018", "0100109003", "0100111003", "0800109"),
#'   metode2select = c("070070", "070231", "010057", "060265")
#'   )
set_disease_selection_parameters <- function(year = NULL,
                                             hensikt2select = NULL,
                                             utbrudd2select = NULL,
                                             metode2select = NULL,
                                             analytt2select = NULL,
                                             file = NULL) {

  # ARGUMENT CHECKING ----

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  checkmate::assert_character(hensikt2select, min.chars = 2, null.ok = TRUE, add = checks)
  checkmate::assert_character(utbrudd2select, null.ok = TRUE, add = checks)
  checkmate::assert_character(metode2select, min.chars = 6, null.ok = TRUE, add = checks)
  NVIcheckmate::assert_non_null(list(analytt2select, hensikt2select, utbrudd2select, file), add = checks)
  checkmate::assert_character(analytt2select, min.chars = 2, add = checks)
  if (!is.null(file)) {
    checkmate::assert_file(x = file, add = checks)
  }

  # Report check-results
  checkmate::reportAssertions(checks)


  if (!is.null(file)) {
    source(file = file)
  }

  return(list("year" = year,
              "hensikt2select" =  hensikt2select,
              "utbrudd2select" = utbrudd2select,
              "metode2select" = metode2select,
              "analytt2select" = analytt2select))
}
