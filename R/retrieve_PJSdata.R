#' @title Retrieves data from PJS
#' @description Retrieves and standardises PJS data. The intention of
#'     \code{retrieve_PJSdata} is to shorten code and to ensure that a standard
#'     procedure is followed for retrieving PJS-data. \code{retrieve_PJSdata}
#'     constructs the select statement by a build_query-function (see details)
#'     and selection parameters. An open ODBC-channel is created using
#'     \ifelse{html}{\code{\link{login_by_credentials_PJS}}}{\code{login_by_credentials_PJS}},
#'     and the data is retrieved using the select statement. Thereafter, the
#'     data is standardised using
#'     \ifelse{html}{\code{\link{standardize_PJSdata}}}{\code{standardize_PJSdata}}
#'     and
#'     \ifelse{html}{\code{\link{exclude_from_PJSdata}}}{\code{exclude_from_PJSdata}}.
#'
#' @details The function is dependent that credentials for PJS have been saved using
#'     \ifelse{html}{\code{\link{set_credentials_PJS}}}{\code{set_credentials_PJS}}.
#'     else, an open ODBC channel to PJS cannot be established.
#'
#' The select statement for PJS can be built giving the selection parameters and
#'     input to one of the build_query-functions, i.e.
#'     \ifelse{html}{\code{\link{build_query_hensikt}}}{\code{build_query_hensikt}},
#'     \ifelse{html}{\code{\link{build_query_one_disease}}}{\code{build_query_one_disease}}
#'     and
#'     \ifelse{html}{\code{\link{build_query_outbreak}}}{\code{build_query_outbreak}}.
#'     The selection parameters can be set by using
#'     \ifelse{html}{\code{\link{set_disease_parameters}}}{\code{set_disease_parameters}}.
#'     or by giving a list of similar format for input to
#'     \code{selection_parameters}, see the build_query-functions for necessary
#'     input.
#'
#' \code{retrieve_PJSdata} gives the possibility of giving the select_statement
#'     as a string instead of using the build_query-functions. This should only
#'     by done for select statements that
#'     previously have been tested and are known to have correct syntax.
#'     \code{retrieve_PJSdata} has no possibility of checking the syntax before
#'     it is submitted to PJS and untested select statements can take a lot of
#'     time or stop the function without proper error messages.

#'
#' @template build_query_year
#' @param selection_parameters [\code{character(1)}]\cr
#' Either the path and file name for an R script that can be sourced and that
#'     sets the selection parameters or a named list with the selection parameters
#'     (i.e. similar to the output of
#'     \ifelse{html}{\code{\link{set_disease_parameters}}}{\code{set_disease_parameters}}).
#'     Defaults to \code{NULL}.
#' @param FUN [\code{function}]\cr
#' Function to build the selection statement, see details. Defaults to \code{NULL}.
#' @param select_statement [\code{character(1)}]\cr
#' A written select statement, see details. Defaults to \code{NULL}.
#' @param \dots Other arguments to be passed to underlying functions.
#'
#' @return A named list with PJS data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#'
#' #
retrieve_PJSdata <- function(year,
                             selection_parameters,
                             FUN,
                             select_statement = NULL,
                             ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")),
                               min.len = 1,
                               add = checks)
  NVIcheckmate::assert(checkmate::check_file_exists(x = selection_parameters, access = "r"),
                       checkmate::check_list(x = selection_parameters, null.ok = TRUE),
                       combine = "or",
                       comment = "The argument selection_parameter must either be a file with selection parameters or a list with selection parameters",
                       add = checks)
  checkmate::assert_function(FUN, null.ok = TRUE, add = checks)
  checkmate::assert_choice(deparse(substitute(FUN)),
                           choices = c("build_query_one_disease", "build_query_hensikt", "build_query_outbreak"),
                           null.ok = TRUE,
                           add = checks)
  checkmate::assert(checkmate::check_list(x = select_statement, null.ok = TRUE),
                    checkmate::check_string(x = select_statement),
                    combine = "or",
                    add = checks)
  NVIcheckmate::assert_non_null(list(selection_parameters, select_statement, add = checks))
  NVIcheckmate::assert_non_null(list(FUN, select_statement), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # GENERATE SELECT STATEMENT ----
  if (!is.null(selection_parameters) & !is.null(FUN)) {
    selection_parameters <- set_disease_parameters(selection_parameters = selection_parameters)

    # Character vector with arguments for FUN
    FUN_args <- names(formals(args(FUN)))

    # Create FUN_input for modifications,
    #  keep the original selection_parameters.
    FUN_input <- selection_parameters
    # Rename list objects to input for FUN
    names(FUN_input) <- gsub("2select", "", names(FUN_input))
    # Include year and period in FUN_input
    FUN_input <- append(FUN_input,
                        values = list("year" = year, "period" = year))
    FUN_input <- append(FUN_input,
                        values = c("db" = "PJS"))

    # Keep only relevant arguments for FUN in FUN_input
    FUN_input <- FUN_input[FUN_args]
    select_statement <- do.call(FUN, FUN_input)
  }

  # OPEN ODBC CHANNEL ----
  journal_rapp <- login_PJS()
  PJSdata <- vector("list", length = length(select_statement))

  # dbsource_names <- rep(NA, length(select_statement))
  dbsource <- names(select_statement)
  names(dbsource) <- names(select_statement)
  if (!is.null(FUN)) {
    dbsource <- gsub(pattern = "selection_v2_sak_m_res", replacement = "v2_sak_m_res", x = dbsource)
    dbsource <- gsub(pattern = "selection_sakskonklusjon", replacement = "v_sakskonklusjon", x = dbsource)
  }

  for (i in c(1:length(select_statement))) {

    # READ DATA FROM PJS ----
    # dbsource <- substr(select_statement[i],
    #                    gregexpr(pattern = "v_", text = select_statement[i])[[1]][1],
    #                    gregexpr(pattern = "v_", text = select_statement[i])[[1]][2] - 1)
    # dbsource <- stringi::stri_extract_first_words(dbsource)
    # dbsource_names[i] <- dbsource

    PJSdata[[i]] <- RODBC::sqlQuery(journal_rapp,
                                    select_statement[i],
                                    as.is = TRUE,
                                    stringsAsFactors = FALSE)

    # STANDARDIZE DATA ----
    # PJSdata
    PJSdata[[i]] <- standardize_PJSdata(PJSdata = PJSdata[[i]], dbsource = dbsource[i])

    # Exclude ring trials, quality assurance and samples from abroad
    # PJSdata
    PJSdata[[i]] <- exclude_from_PJSdata(PJSdata = PJSdata[[i]], ...)

  }

  # CLOSE ODBC CHANNEL ----
  RODBC::odbcClose(journal_rapp)

  PJSdata <- stats::setNames(PJSdata, names(dbsource))

  # RETURN RESULT ----
  return(PJSdata)
}
