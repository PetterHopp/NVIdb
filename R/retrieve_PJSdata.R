#' @title Sets disease selection parameters
#' @description Sets the disease selection parameters and store them in a list
#'     object. The list follows a standardised named format and the elements can
#'     be used as input to 
#'     \ifelse{html}{\code{\link{build_query_hensikt}}}{\code{build_query_hensikt}},
#'     \ifelse{html}{\code{\link{build_query_one_disease}}}{\code{build_query_one_disease}}
#'     or
#'     \ifelse{html}{\code{\link{build_query_outbreak}}}{\code{build_query_outbreak}}.
#'
#' @details Retrieves saker in PJS 
#' 
#' Be aware that these functions only builds an sql building block to be
#'     included into a select statement. It will not build a complete select
#'     statement. These functions are mainly intended for internal use and
#'     are called from 
#'     \ifelse{html}{\code{\link{build_query_hensikt}}}{\code{build_query_hensikt}},
#'     \ifelse{html}{\code{\link{build_query_one_disease}}}{\code{build_query_one_disease}}
#'     and
#'     \ifelse{html}{\code{\link{build_query_outbreak}}}{\code{build_query_outbreak}}.
#'     If generating own select statements, these can be used to facilitate
#'     the coding. The building blocks can be combined with "AND" and "OR" 
#'     and brackets to get the intended select statement.
#'
#' @template build_query_year
#' @param selection_parameters [\code{character(1)}]\cr
#' Either the path and file name for an R script that can be sourced and that
#'     sets the selection parameters or a named list with the selection parameters
#'     (i.e. equal to the output of this function). Defaults to \code{NULL}.
#' @param FUN [\code{function}]\cr
#' Function to build the selection statement. 
#' @param select_statement [\code{character(1)}]\cr
#' "utbruddsID". Defaults to \code{NULL}.
#' @param \dots Other arguments to be passed to `set_disease_parameters`.
#'
#' @return A named list with PJS data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # Selection parameters for Pancreatic disease (PD)
# 
retrieve_PJSdata <- function(year, 
                             selection_parameters, 
                             FUN, 
                             select_statement, 
                             ...) {
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  NVIcheckmate::assert(checkmate::check_file_exists(x = selection_parameters, access = "r"),
                       checkmate::check_list(x = selection_parameters),
                       combine = "or",
                       comment = "The argument selection_parameter must either be a file with selection parameters or a list with selection parameters",
                       add = checks)
  
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  selection_parameters <- set_disease_parameters(selection_parameters = selection_parameters) 
  
    # List with arguments and default values
    FUN_args <- formals(args(NVIdb::build_query_one_disease))
    # Character vector with arguments only
    FUN_args <- names(FUN_args)
    # Remove first argument (either year or period)
    # Identify only relevant arguments from selection parameters
    # Input relevant arguments in correct order; do.call??
    select_statement <- FUN(year = year,
                            analytt = selection_parameters$analytt2select, 
                            hensikt = selection_parameters$hensikt2select,
                            utbrudd = selection_parameters$utbrudd2select,
                            metode = selection_parameters$metode2select)
    
    
    # READ DATA FROM PJS ----
    journal_rapp <- login_PJS()
    
    PJSrawdata <- sqlQuery(journal_rapp,
                           select_statement["selection_v2_sak_m_res"],
                           as.is = TRUE,
                           stringsAsFactors = FALSE)
    
    
    
    PJSsakskonklusjon <- sqlQuery(journal_rapp,
                                  select_statement["selection_sakskonklusjon"],
                                  as.is = TRUE,
                                  stringsAsFactors = FALSE)
    
    odbcClose(journal_rapp)
    
    
    
    # STANDARDIZE DATA ----
    
    # PJSdata
    PJSdata <- standardize_PJSdata(PJSdata = PJSrawdata)
    # sakskonklusjon
    sakskonklusjon <- standardize_PJSdata(PJSdata = PJSsakskonklusjon) 
    
    
    # Exclude ring trials, quality assurance and samples from abroad
    # PJSdata
    PJSdata <- exclude_from_PJSdata(PJSdata = PJSdata, abroad = "exclude", quality = "exclude")
    # sakskonklusjon
    sakskonklusjon <- exclude_from_PJSdata(PJSdata = sakskonklusjon, abroad = "exclude", quality = "exclude") 
    
    return(list(PJSdata, sakskonklusjon)) 
  } 
  
  