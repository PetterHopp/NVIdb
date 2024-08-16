#' @title Builds query for selecting data for hensikt from PJS
#' @description Builds the query for selecting all data for one or
#'     more hensikt within one year from PJS. The query is written
#'     in T-SQL as used by MS-SQL.
#'
#' @details The function builds the SQL syntax to select all
#'     PJS-journals concerning the hensiktkoder from PJS.
#'
#' @template build_query_year
#' @param hensikt [\code{character}]\cr
#'     Vector with one or more specific hensiktkoder. If sub-hensikter
#'     should be included, end the code with \%.
#' @template build_query_db
#'
#' @return A list with select-statements for "v2_sak_m_res" and
#'     "v_sakskonklusjon", respectively. The statements should be
#'     included in a \code{RODBC::sqlQuery}.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name build_query_hensikt-deprecated
#' @keywords internal
#'
#' @examples
#' # SQL-select query for Pancreatic disease (PD)
#' build_query_hensikt(year = 2020,
#'                     hensikt = c("0200102"))
#'
NULL

#' @title build_query_hensikt is Deprecated
#' @description \code{build_query_hensikt} was deprecated in NVIdb v0.13.0 released
#'     2024-##-##. All PJS related functions have been moved to \code{NVIpjsr}.
#'     Use \code{NVIpjsr::build_query_hensikt} instead. When attaching packages,
#'     remember to attach \code{NVIdb} before \code{NVIpjsr}.
#' @details The old help pages can be found at \code{help("build_query_hensikt-deprecated")}.
#'     Information on deprecated functions can be found at \code{help("NVIdb-deprecated")}.
#'
#' @template build_query_year
#' @param hensikt [\code{character}]\cr
#'     Vector with one or more specific hensiktkoder. If sub-hensikter
#'     should be included, end the code with \%.
#' @template build_query_db
#'
#' @export
#' @keywords internal
#'

build_query_hensikt <- function(year, hensikt, db = "PJS") {

  # DEPRECATED ----
  .Deprecated(new = "build_query_hensikt",
              package = "NVIdb",
              msg = paste("'build_query_hensikt' is replaced by
                          'NVIpjsr::build_query_hensikt'"))

  if (isTRUE(NVIcheckmate::check_package(x = "NVIpjsr", type = "installed"))) {
    select_statement <- NVIpjsr::build_query_hensikt(year = year,
                                              hensikt = hensikt,
                                              db = db)

    return(select_statement)
  } else {
    # Argument checking
    # Object to store check-results
    checks <- checkmate::makeAssertCollection()
    # Perform checks
    checkmate::assert_integerish(year, lower = 1990, upper = as.numeric(format(Sys.Date(), "%Y")), min.len = 1, add = checks)
    checkmate::assert_character(hensikt, min.chars = 2, any.missing = FALSE, add = checks)
    checkmate::assert_choice(db, choices = c("PJS"), add = checks)
    # Report check-results
    checkmate::reportAssertions(checks)

    select_year <- NVIdb::build_sql_select_year(year = year, varname = "aar")

    select_hensikt <- NVIdb::build_sql_select_code(values = hensikt, varname = "hensiktkode")

    selection_v2_sak_m_res <- paste("SELECT * FROM v2_sak_m_res",
                                    "WHERE", select_year, "AND",
                                    "(",
                                    select_hensikt,
                                    ")")

    select_year <- NVIdb::build_sql_select_year(year = year, varname = "sak.aar")

    select_hensikt <- NVIdb::build_sql_select_code(values = hensikt, varname = "sak.hensiktkode")

    selection_sakskonklusjon <- paste("SELECT v_sakskonklusjon.*,",
                                      "sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet, sak.hensiktkode,",
                                      "sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                                      "FROM v_innsendelse AS sak",
                                      "INNER JOIN v_sakskonklusjon",
                                      "ON (v_sakskonklusjon.aar = sak.aar AND",
                                      "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                                      "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                                      "WHERE", select_year, "AND (",
                                      select_hensikt,
                                      ")")

    select_statement <- list("selection_v2_sak_m_res" = selection_v2_sak_m_res,
                             "selection_sakskonklusjon" = selection_sakskonklusjon)

    return(select_statement)
  }
}
