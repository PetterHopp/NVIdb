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
#'     The purpose is a short description of purpose of the selection described by the
#'     selection parameters for example "ok_svin_virus" that describes the selection
#'     parameters for the OK programme for virus in swine. The purpose is also used as
#'     part of the file name for selection_parameters, i.e. "purpose_selection_parameters"
#'     and in the annual tables for ok_programmer: Kontrolltabeller for yyyy.
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
#'     \code{hensikt2select}, \code{hensikt2delete}, \code{utbrudd2select}, \code{metode2select},
#'     \code{analytt2select}, \code{analytt2delete}, \code{art2select}, \code{include_missing_art},
#'     \code{select_statement}, and \code{FUN},
#'     the input may be given in a source file. This may be handy if the
#'     selection will be performed many times. It also gives the possibility of
#'     using a for loop that selects PJS-data and performs similar analyses for one
#'     disease at a time.
#'
#'     The selection parameter \code{analytt2delete} is intended for the situation where
#'     \code{analytt2select} includes analytter higher in the hierarchy and there are
#'     specific analytter lower in the hierarchy that should not be included. A typical
#'     example is the selection of all samples with the analytt Mycobacterium spp and
#'     below, but one is only interested in M. tuberculosis complex but not in M. avium.
#'
#'     The possibility of input other arguments are kept to make it possible to use the
#'     deprecated arguments \code{missing_art} and \code{file}. If these are used, a
#'     warning is issued and the input is transferred to \code{include_missing_art} and
#'     \code{selection_parameters}, respectively.
#'
#' @param purpose [\code{character}]\cr
#' A short description of the purpose of the selection, see details. Defaults to NULL.
#' @param hensikt2select [\code{character}]\cr
#' Specific "hensiktkoder" for the "analytt" in question. If sub-codes should
#'     be included, end the code with \%. Defaults to \code{NULL}.
#' @param hensikt2delete [\code{character}]\cr
#' "hensiktkoder" for which saker should be excluded.
#'     If sub-codes should be included, end the code with \%. Defaults to \code{NULL}.
#' @param utbrudd2select [\code{character(1)}]\cr
#' "utbruddsID". Defaults to \code{NULL}.
#' @param metode2select [\code{character}]\cr
#' Specific "metodekoder" for the "analytt" in question. Defaults to \code{NULL}.
#' @param analytt2select [\code{character}]\cr
#' "analyttkoder" for the agent and/or disease. If sub-codes should be included,
#'     end the code with \%. Defaults to \code{NULL}.
#' @param analytt2delete [\code{character}]\cr
#' Specific "analyttkoder" that should be deleted, see details. If sub-codes should
#'     be included, end the code with \%. Defaults to \code{NULL}.
#' @param art2select [\code{character}]\cr
#' "artkoder". If sub-codes should be included, end the code with \%.  \code{NA} can be
#'     combined with another "artkode". Defaults to \code{NULL}.
#' @param include_missing_art [\code{character(1)}]\cr
#' Should missing art be included. Must be one of c("never", "always", "for_selected_hensikt").
#'     If \code{NULL}, it is set to "always" when \code{art2select} includes \code{NA}, else it is set to "never".
#'     Defaults to \code{NULL}.
#' @param selection_parameters [\code{character(1)}]\cr
#' Either the path and file name for an R script that can be sourced and that
#'     sets the selection parameters or a named list with the selection parameters
#'     (i.e. equal to the output of this function). Defaults to \code{NULL}.
#' @param FUN [\code{function}]\cr
#' Function to build the selection statement, see
#'     \ifelse{html}{\code{\link{retrieve_PJSdata}}}{\code{retrieve_PJSdata}}).
#'     Defaults to \code{NULL}.
#' @param select_statement [\code{character(1)}]\cr
#' A written select statement, see
#'     \ifelse{html}{\code{\link{retrieve_PJSdata}}}{\code{retrieve_PJSdata}}.
#'     Defaults to \code{NULL}.
#' @param \dots Other arguments to be passed to \code{set_disease_parameters}.
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
set_disease_parameters <- function(purpose = NULL,
                                   hensikt2select = NULL,
                                   hensikt2delete = NULL,
                                   utbrudd2select = NULL,
                                   metode2select = NULL,
                                   analytt2select = NULL,
                                   analytt2delete = NULL,
                                   art2select = NULL,
                                   include_missing_art = NULL,
                                   FUN = NULL,
                                   select_statement = NULL,
                                   selection_parameters = NULL,
                                   ...) {

  # SET SELECTION PARAMETERS ----
  # Vector with possible selection parameter names
  # missing_art is deprecated
  var2select_template <- c("purpose",
                           "hensikt2select", "hensikt2delete", "utbrudd2select",
                           "metode2select", "analytt2select", "analytt2delete", "art2select",
                           "include_missing_art", "missing_art",
                           "FUN", "select_statement")

  # PREPARE ARGUMENTS BEFORE CHECKING ----
  if ("file" %in% ...names() & is.null(selection_parameters)) {
    selection_parameters <- unlist(list(...)$file)
    warning(paste("The argument 'file' is deprecated.",
                  "Use 'selection_parameters' instead",
                  "The input to 'file' has been transferred to 'selection_parameters' if this is NULL."))
  }

  if ("missing_art" %in% ...names() & is.null(include_missing_art)) {
    include_missing_art <- unlist(list(...)$missing_art)
    if (include_missing_art == "non_selected_hensikt") {include_missing_art <- "for_selected_hensikt"}
    warning(paste("The argument 'missing_art' is deprecated.",
                  "Use 'include_missing_art' instead",
                  "The input to 'missing_art' has been transferred to 'include_missing_art' if this is NULL."))
  }


  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Import values from parameter file if exists
  if (!is.null(selection_parameters)) {
    NVIcheckmate::assert(checkmate::check_file_exists(x = selection_parameters, access = "r"),
                         checkmate::check_list(x = selection_parameters),
                         combine = "or",
                         comment = "The argument selection_parameter must either be a file with selection parameters or a list with selection parameters",
                         add = checks)
    if (isTRUE(checkmate::check_file_exists(x = selection_parameters, access = "r"))) {
      script <- as.character(parse(file = selection_parameters, encoding = "UTF-8"))

      script <- script[grepl(pattern = paste0("[^",
                                              paste(var2select_template, collapse = "|^"),
                                              "]",
                                              "[[:blank:]]*",
                                              "[=|<\\-]"),
                             script)]

      for (i in 1:length(script)) {
        eval(parse(text = script[i]))
      }
    }
    if (isTRUE(checkmate::check_list(x = selection_parameters))) {
      checkmate::assert_subset(x = names(selection_parameters),
                               choices = var2select_template,
                               empty.ok = FALSE)
      var2select <- intersect(names(selection_parameters[!sapply(selection_parameters, is.null)]),
                              var2select_template)
      for (i in var2select) {
        # assign(i, unname(unlist(selection_parameters[i])))
        assign(i, unname(selection_parameters[i][[1]]))
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
  # # Object to store check-results
  # checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_string(purpose, null.ok = TRUE, add = checks)
  NVIcheckmate::assert_non_null(list(analytt2select, hensikt2select, utbrudd2select, unlist(selection_parameters)), add = checks)
  checkmate::assert_character(hensikt2select, min.chars = 2, max.chars = 15, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(hensikt2delete, min.chars = 2, max.chars = 15, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(utbrudd2select, max.chars = 5, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(metode2select, min.chars = 2, max.chars = 6, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(analytt2select, min.chars = 2, max.chars = 20, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(analytt2delete, min.chars = 2, max.chars = 20, any.missing = FALSE, null.ok = TRUE, add = checks)
  checkmate::assert_character(art2select, min.chars = 2, max.chars = 20, all.missing = FALSE, null.ok = TRUE, add = checks)
  # if (!is.null(art2select) && any(is.na(art2select))) {
  checkmate::assert_choice(include_missing_art,
                           choices = c("never", "always", "for_selected_hensikt"),
                           add = checks)
  # }
  checkmate::assert_function(FUN, null.ok = TRUE, add = checks)
  checkmate::assert(checkmate::check_list(x = select_statement, null.ok = TRUE),
                    checkmate::check_string(x = select_statement),
                    combine = "or",
                    add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # CREATE LIST WITH PARAMETER VALUES ----
  return(list("purpose" = purpose,
              "hensikt2select" = hensikt2select,
              "hensikt2delete" = hensikt2delete,
              "utbrudd2select" = utbrudd2select,
              "metode2select" = metode2select,
              "analytt2select" = analytt2select,
              "analytt2delete" = analytt2delete,
              "art2select" = art2select,
              "include_missing_art" = include_missing_art,
              "FUN" = FUN,
              "select_statement" = select_statement))
}
