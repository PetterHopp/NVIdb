#' @title Builds query to select data for a disease outbreak from PJS
#' @description Builds a query to select all data for a disease outbreak from
#'     PJS. The input are utbruddsid, hensiktskoder, analyttkoder for the
#'     infectious agent and/or disease, and metodekoder specific for the
#'     infection and/or disease. The the query is written in T-SQL as used by MS-SQL.
#'
#' @details The function builds the SQL syntax to select all PJS-saker
#'     regarding a disease outbreak from PJS. This is based on a outbreak being
#'     defined by an utbruddsid, hensiktkoder and/or analyttkoder for the
#'     infectious agent and/or disease. At least one of these must be given as
#'     input to the function.
#'
#'     The utbruddsid is the internal id of the utbrudd in the utbrudds-register
#'     in PJS. One or more utbruddsid may be given as input.
#'
#'     One or more hensiktkoder may be input to the selection statement. These
#'     may define the outbreak by themselves or may be input in addition to the
#'     utbruddsid and/or analyttkode.
#'
#'     One or more analyttkoder may be input to the selection statement. These
#'     may define the outbreak by themselves or may be input in addition to the
#'     utbruddsid and/or hensiktkode.
#'
#'     In addition one or more specific metoder may be input to the selection
#'     statement. With specific metode is meant a metode that implies an
#'     examination that will give one of the input analytter as a result. These
#'     cannot be sufficient to define the outbreak, but is included if the
#'     outbreak is defined as all samples examined for a specific analytt.
#'
#' @param period [\code{numeric}]\cr
#'     Time period given as year. One year or a vector giving the first
#'     and last years that should be selected.
#' @param utbrudd  [\code{character}]\cr
#'     Utbruddsid(er) that should be selected. Defaults to \code{NULL}.
#' @param hensikt [\code{character}]\cr
#'     Specific hensiktkoder. If sub-hensikter should be included,
#'     end the code with \%. Defaults to \code{NULL}.
#' @param analytt [\code{character}]\cr
#'     Analyttkoder that should be selected. If sub-analytter should be included,
#'     end the code with \%. Defaults to \code{NULL}.
#' @param metode [\code{character}]\cr
#'     Specific metodekoder. Defaults to \code{NULL}.
#' @template build_query_db
#'
#' @return A list with select-statement for v2_sak_m_res and v_sakskonklusjon to
#'     be included in a \code{RODBC::sqlQuery}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # SQL-select query for an outbreak
#' build_query_outbreak(period = 2022,
#'                      utbrudd = "27",
#'                      hensikt = c("0100101014", "0100102005", "0100103005",
#'                                  "0100104029", "0200130%"),
#'                      analytt = "01130301%",
#'                      metode = NULL)
build_query_outbreak <- function(period,
                                 utbrudd = NULL,
                                 hensikt = NULL,
                                 analytt = NULL,
                                 metode = NULL,
                                 db = "PJS") {

  # Argument checking

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(period, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  checkmate::assert_character(utbrudd, min.chars = 1, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(hensikt, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(analytt, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  NVIcheckmate::assert_non_null(x = list(utbrudd, hensikt, analytt), add = checks)
  checkmate::assert_character(metode, min.chars = 6, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  select_year <- NVIdb::build_sql_select_year(year = period, varname = "aar")

  select_hensikt <- NVIdb::build_sql_select_code(values = hensikt, varname = "hensiktkode")
  if (nchar(select_hensikt) > 0) {select_codes <- select_hensikt}

  # Select utbruddsid
  select_utbrudd <- NVIdb::build_sql_select_code(values = utbrudd, varname = "utbrudd_id")
  if (nchar(select_utbrudd) > 0) {
    if (nchar(select_codes) > 0) {select_codes <- paste(select_codes, "OR")}
    select_codes <- paste(select_codes, select_utbrudd)
  }

  # Select metodekode
  select_metode <- NVIdb::build_sql_select_code(values = metode, varname = "metodekode")
  if (nchar(select_metode) > 0) {
    if (nchar(select_codes) > 0) {select_codes <- paste(select_codes, "OR")}
    select_codes <- paste(select_codes, select_metode)
  }

  # Select konkl_analyttkode
  select_konkl_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "konkl_analyttkode")
  if (nchar(select_konkl_analytt) > 0) {
    if (nchar(select_codes) > 0) {select_codes <- paste(select_codes, "OR")}
    select_codes <- paste(select_codes, select_konkl_analytt)
  }

  # Select res_analyttkode
  select_res_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "analyttkode_funn")
  if (nchar(select_res_analytt) > 0) {
    if (nchar(select_codes) > 0) {select_codes <- paste(select_codes, "OR")}
    select_codes <- paste(select_codes, select_res_analytt)
  }


  # Build query
  selection_v2_sak_m_res <- paste("SELECT * FROM v2_sak_m_res",
                                  "WHERE", select_year, "AND",
                                  paste0("(",
                                         select_codes,
                                         ")"))

  # # Remove unnecessary spaces from string
  selection_v2_sak_m_res <- gsub(' +', ' ', selection_v2_sak_m_res)
  # selection_v2_sak_m_res <- gsub(" )", ")", selection_v2_sak_m_res, fixed = TRUE)
  # selection_v2_sak_m_res <- gsub("( ", "(", selection_v2_sak_m_res, fixed = TRUE)


  select_year <- NVIdb::build_sql_select_year(year = period, varname = "sak.aar")

  if (!is.null(analytt)) {
    select_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "analyttkode")
    select_analytt <- paste0("AND (",
                             select_analytt,
                             ")")
  } else {select_analytt <- ""}



  # Build query
  selection_sakskonklusjon <- paste("SELECT v_sakskonklusjon.*,",
                                    "sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet, sak.hensiktkode,",
                                    "sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                                    "FROM v_innsendelse AS sak",
                                    "INNER JOIN v_sakskonklusjon",
                                    "ON (v_sakskonklusjon.aar = sak.aar AND",
                                    "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                                    "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                                    "WHERE", select_year,
                                    select_analytt)

  # Remove unnecessary spaces from string
  selection_sakskonklusjon <- gsub(" +", " ", selection_sakskonklusjon)
  selection_sakskonklusjon <- gsub(" $", "", selection_sakskonklusjon)
  # selection_sakskonklusjon <- gsub("( ", "(", selection_sakskonklusjon, fixed = TRUE)

  select_statement <- list("selection_v2_sak_m_res" = selection_v2_sak_m_res,
                           "selection_sakskonklusjon" = selection_sakskonklusjon)

  return(select_statement)
}
