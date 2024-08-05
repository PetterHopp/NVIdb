#' @title Retrieves data from PJS
#' @description Retrieves and standardises PJS data. \code{retrieve_PJSdata} is
#'     a wrapper for several \code{NVIdb}-functions and the intention of
#'     \code{retrieve_PJSdata} is to shorten code and to ensure that a standard
#'     procedure is followed when retrieving PJS data, see details. It can only
#'     be used for retrieving case data from PJS where the columns "aar",
#'     "ansvarlig_seksjon" and "innsendelsenr" are included in the columns. It
#'     cannot be used for retrieving data from other tables available in
#'     "journal_rapp".
#'
#' @details \code{retrieve_PJSdata} is a wrapper for the following \code{NVIdb}-functions:
#' \itemize{
#'   \item Constructs the select statement by a build_query-function (see details)
#'     and selection parameters.
#'   \item Creates an open ODBC-channel using
#'     \ifelse{html}{\code{\link{login_PJS}}}{\code{login_PJS}}.
#'   \item Retrieves the data using the select statement constructed above.
#'   \item Standardises the data using
#'     \ifelse{html}{\code{\link{standardize_PJSdata}}}{\code{standardize_PJSdata}}.
#'   \item Excludes unwanted cases using
#'     \ifelse{html}{\code{\link{exclude_from_PJSdata}}}{\code{exclude_from_PJSdata}}.
#'   }
#'
#' For the function to run automatically without having to enter PJS user
#'     credentials, it is dependent that PJS user credentials have been saved using
#'     \ifelse{html}{\code{\link{set_credentials_PJS}}}{\code{set_credentials_PJS}}.
#'     Otherwise, the credentials must be input manually to establish an open
#'     ODBC channel.
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
#'     as a string instead of using the build_query-functions. If so, the
#'     select_statement should be included in the selection parameters. This
#'     should only by done for select statements that previously have been tested
#'     and are known to have correct syntax. \code{retrieve_PJSdata} has no
#'     possibility of checking the sql syntax before it is submitted to PJS and
#'     untested select statements can take a lot of time or stop the function
#'     without proper error messages. In the case that both a select_statement
#'     and a function with the necessary selection_parameters are given,
#'     the select_statement constructed by the function will be used.
#'
#' The output is a named list where each entry is a data frame with PJS data. If
#'     the select statement is named, the returned data frame will have that name.
#'     If the select statement is unnamed, it will try to identify the first
#'     table in the select statement and use this as name. If not possible, the
#'     name will be of the format "PJSdata#" where # is the number of the select
#'     statement.

#'
#' @param year [\code{numeric}]\cr
#' One year or a vector giving the first and last years that should be selected.
#'     Defaults to \code{NULL}.
#' @param selection_parameters [\code{character(1)}]\cr
#' Either the path and file name for an R script that can be sourced and that
#'     sets the selection parameters or a named list with the selection parameters
#'     (i.e. of the same format as the output of
#'     \ifelse{html}{\code{\link{set_disease_parameters}}}{\code{set_disease_parameters}}).
#'     Defaults to \code{NULL}.
#' @param FUN \code{deprecated}\cr
#' \code{FUN} should instead be included as input to \code{selection_parameters}.
#'     Defaults to \code{NULL}.
#' @param select_statement \code{deprecated}\cr
#' \code{select_statement} should instead be included as input to
#'     \code{selection_parameters}. Defaults to \code{NULL}.
#' @param \dots Other arguments to be passed to the underlying functions:
#'     \ifelse{html}{\code{\link{login_PJS}}}{\code{login_PJS}}
#'     and
#'     \ifelse{html}{\code{\link{exclude_from_PJSdata}}}{\code{exclude_from_PJSdata}}.
#'
#' @return A named list with PJS data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#'
#' #
retrieve_PJSdata <- function(year = NULL,
                             selection_parameters = NULL,
                             FUN = NULL,
                             select_statement = NULL,
                             ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year,
                               lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")),
                               min.len = 1,
                               null.ok = TRUE,
                               add = checks)
  NVIcheckmate::assert(checkmate::check_file_exists(x = selection_parameters, access = "r"),
                       checkmate::check_list(x = selection_parameters),
                       combine = "or",
                       comment = "The argument selection_parameter must either be a file with selection parameters or a list with selection parameters",
                       add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # READ ARGUMENTS
  selection_parameters <- set_disease_parameters(selection_parameters = selection_parameters)

  # CHECK FOR DEPRECATED ARGUMENTS ----
  if (!is.null(FUN)) {
    w_msg <- paste("The argument 'FUN' is deprecated.",
                   "'FUN' should be included as input to 'selection_parameters' instead.")
    if (is.null(selection_parameters$FUN)) {
      selection_parameters$FUN <- FUN
      warning(paste(w_msg,
                    "The input to 'FUN' has been transferred to 'selection_parameters$FUN'."))
    } else {warning(paste(w_msg,
                          "The input to 'FUN' has been overwritten by 'selection_parameters$FUN'."))
    }
  }

  if (!is.null(select_statement)) {
    w_msg <- paste("The argument 'select_statement' is deprecated.",
                   "'select_statement' should be included in input to 'selection_parameters' instead.")
    if (is.null(selection_parameters$select_statement)) {
      selection_parameters$select_statement <- select_statement
      warning(paste(w_msg,
                    "The input to 'select_statement' has been transferred to 'selection_parameters$select_statement'."))
    } else {warning(paste(w_msg,
                          "The input to 'select_statement' has been overwritten by 'selection_parameters$select_statement'."))
    }
  }

  # ARGUMENT CHECKING OF selection_parameters ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  checkmate::assert_function(unlist(selection_parameters$FUN), null.ok = TRUE, add = checks)
  checkmate::assert(checkmate::check_list(x = selection_parameters$select_statement, null.ok = TRUE),
                    checkmate::check_string(x = selection_parameters$select_statement),
                    combine = "or",
                    add = checks)
  NVIcheckmate::assert_non_null(list(selection_parameters$hensikt2select,
                                     selection_parameters$analytt2select,
                                     selection_parameters$utbrudd2select,
                                     selection_parameters$select_statement, add = checks))
  NVIcheckmate::assert_non_null(list(unlist(selection_parameters$FUN), selection_parameters$select_statement), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # The select statement is overwritten by a constructed select_statement below
  # if FUN and the necessary selection_parameters for FUN are given
  select_statement <- selection_parameters$select_statement

  # GENERATE SELECT STATEMENT ----
  if (NVIcheckmate::check_non_null(list(selection_parameters$hensikt2select,
                                        selection_parameters$analytt2select,
                                        selection_parameters$utbrudd2select)) &
      !is.null(selection_parameters$FUN)) {
    # Character vector with arguments for FUN
    FUN_args <- names(formals(args(selection_parameters$FUN)))

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
    select_statement <- do.call(selection_parameters$FUN, FUN_input)
  }

  # GIVE NAME TO EACH SELECTION STATEMENT
  # check if the select statements are named. If not, give them names
  # if there are no names for the list entries
  if (is.null(names(select_statement))) {
    select_statement_names <- rep("", c(1:length(select_statement)))
  } else {
    # Naming elements that miss names if some are named
    select_statement_names <- names(select_statement)
  }
  # for (i in missing_names) {
  missing_names <- which(select_statement_names == "")
  if (length(missing_names) > 0) {
    for (i in missing_names) {
      select_statement_names[i] <- substr(select_statement[i],
                                          gregexpr(pattern = "v[[:digit:]]*_", text = select_statement[i])[[1]][1],
                                          min(gregexpr(pattern = "v[[:digit:]]*_", text = select_statement[i])[[1]][2] - 1,
                                              nchar(select_statement[i]), na.rm = TRUE))
      # select_statement_names[i] <- stringi::stri_extract_first_words(select_statement_names[i])
      select_statement_names[i] <- sub("(\\s|,|\\.)[[:print:]]*", "", select_statement_names[i])
      if (select_statement_names[i] == "") {select_statement_names[i] <- paste0("PJSdata", as.character(i))}
    }
  }

  # IDENTIFY PROBABLE dbsource FROM select_statement_names
  dbsource <- select_statement_names
  dbsource <- gsub(pattern = "selection_v2_sak_m_res", replacement = "v2_sak_m_res", x = dbsource)
  dbsource <- gsub(pattern = "selection_sakskonklusjon", replacement = "v_sakskonklusjon", x = dbsource)
  dbsource <- gsub(pattern = "PJSdata[[:digit:]]*", replacement = "v2_sak_m_res", x = dbsource)

  # OPEN ODBC CHANNEL ----
  journal_rapp <- login_PJS(dbinterface = "odbc", ...)
  PJSdata <- vector("list", length = length(select_statement))

  # PERFORM SELECTION AND STANDARDISATION FOR EACH SELECT STATEMENT ----
  for (i in c(1:length(select_statement))) {

    # READ DATA FROM PJS ----
    PJSdata[[i]] <- DBI::dbGetQuery(con = journal_rapp,
                                    statement = select_statement[[i]])
    # STANDARDIZE DATA ----
    PJSdata[[i]] <- standardize_PJSdata(PJSdata = PJSdata[[i]], dbsource = dbsource[i])

    # Exclude ring trials, quality assurance and samples from abroad
    PJSdata[[i]] <- exclude_from_PJSdata(PJSdata = PJSdata[[i]], ...)
  }

  # CLOSE ODBC CHANNEL ----
  DBI::dbDisconnect(journal_rapp)

  # RETURN RESULT ----
  # Give name to each entry in the list of PJSdata
  PJSdata <- stats::setNames(PJSdata, select_statement_names)
  return(PJSdata)
}
