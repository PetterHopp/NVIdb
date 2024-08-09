#' @title Builds query for selecting data for one disease from PJS
#' @description Builds the query for selecting all data for one
#'     infection/disease within one year from PJS. The input is the
#'     analytter for the infectious agent and/or disease, the
#'     hensikter and metoder specific for the infection and/or
#'     disease. The the query is written in T-SQL as used by MS-SQL.
#'
#' @details The function builds the SQL syntax to select all PJS-journals
#'     concerning one infection and/o disease from PJS. This is based
#'     on selecting all journals with the disease and/or infectious
#'     agent analytt in resultat, konklusjon or sakskonklusjon. By this,
#'     all journals were the examination have been performed and a
#'     result has been entered should be selected.
#'
#'     One or more specific hensikter may be input to the selection
#'     statement. With specific hensikt is meant a hensikt that will
#'     imply that the sample will be examined for the infectious agent
#'     or disease. Thereby, the selection will include samples that
#'     haven't been set up for examination yet, samples that were
#'     unfit for examination and samples for which wrong conclusions
#'      have been entered.
#'
#'     One or more specific metoder may be input to the selection
#'     statement. With specific metode is meant a metode that implies
#'     an examination that will give one of the input analytter as a
#'     result. Thereby, the query will include samples that have been
#'     set up for examination, but haven't been examined yet, samples
#'     that were unfit for examination and samples for which wrong
#'     results have been entered.
#'
#'     To select both the disease analytt and the infectious agent
#'     analytt ensures that all journals that have been examined with
#'     a result is included in the output. The inclusion of specific
#'     hensikter and metoder, if exists, ensures that all journals
#'     received with the purpose of examining for the infectious agent
#'     and/or disease will be included even if the examination has
#'     not been performed. This is important for a full control of
#'     all relevant data for an infectious agent and/or disease.
#'
#' @template build_query_year
#' @param analytt [\code{character}]\cr
#'     Analyttkoder that should be selected. If sub-analytter should be included,
#'     end the code with \%.
#' @param hensikt [\code{character}]\cr
#'     Specific hensiktkoder. If sub-hensikter should be included,
#'     end the code with \%. Defaults to \code{NULL}.
#' @param metode [\code{character}]\cr
#'     Specific metodekoder. Defaults to \code{NULL}.
#' @template build_query_db
#'
#' @return A list with select-statement fom v2_sak_m_res and v_sakskonklusjon to be included in a \code{RODBC::sqlQuery}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' # SQL-select query for Pancreatic disease (PD)
#' build_query_one_disease(year = 2020,
#'                         analytt = c("01220104%", "1502010235"),
#'                         hensikt = c("0100108018", "0100109003", "0100111003", "0800109"),
#'                         metode = c("070070", "070231", "010057", "060265"))
build_query_one_disease <- function(year, analytt, hensikt = NULL, metode = NULL, db = "PJS") {

  # Argument checking

  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
  checkmate::assert_character(analytt, min.chars = 2, any.missing = FALSE, add = checks)
  checkmate::assert_character(hensikt, min.chars = 2, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_character(metode, min.chars = 6, null.ok = TRUE, any.missing = FALSE, add = checks)
  checkmate::assert_choice(db, choices = c("PJS"), add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  select_year <- NVIdb::build_sql_select_year(year = year, varname = "aar")

  select_hensikt <- NVIdb::build_sql_select_code(values = hensikt, varname = "hensiktkode")
  if (nchar(select_hensikt) > 0) {select_hensikt <- paste(select_hensikt, "OR")}

  # Select metodekode
  select_metode <- NVIdb::build_sql_select_code(values = metode, varname = "metodekode")
  if (nchar(select_metode) > 0) {select_metode <- paste(select_metode, "OR")}

  select_konkl_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "konkl_analyttkode")

  select_res_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "analyttkode_funn")

  # Build query
  selection_v2_sak_m_res <- paste("SELECT * FROM v2_sak_m_res",
                                  "WHERE", select_year, "AND",
                                  "(",
                                  select_hensikt,
                                  select_metode,
                                  select_konkl_analytt, "OR",
                                  select_res_analytt,
                                  ")")

  # # Remove double spaces from string
  selection_v2_sak_m_res <- gsub(' +', ' ', selection_v2_sak_m_res)


  select_year <- NVIdb::build_sql_select_year(year = year, varname = "sak.aar")

  select_analytt <- NVIdb::build_sql_select_code(values = analytt, varname = "analyttkode")

  # Build query
  selection_sakskonklusjon <- paste("SELECT v_sakskonklusjon.*,",
                                    "sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet, sak.hensiktkode,",
                                    "sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                                    "FROM v_innsendelse AS sak",
                                    "INNER JOIN v_sakskonklusjon",
                                    "ON (v_sakskonklusjon.aar = sak.aar AND",
                                    "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                                    "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                                    "WHERE", select_year, "AND (",
                                    select_analytt,
                                    ")")

  # # Remove double spaces from string
  selection_sakskonklusjon <- gsub(" +", " ", selection_sakskonklusjon)

  select_statement <- list("selection_v2_sak_m_res" = selection_v2_sak_m_res,
                           "selection_sakskonklusjon" = selection_sakskonklusjon)

  return(select_statement)
}
