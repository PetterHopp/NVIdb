#' @title Choose columns from specified PJS-levels
#' @description Fast way to specify the variables from specific PJS-levels.
#'
#' @details When reading PJS-data through certain views, data from more levels that needed may have been read. Some views will also
#'     generate so-called Cartesian product increasing the number of rows considerably. By choosing columns from only specified
#'     levels the number of unique rows may be reduced considerably.
#'
#'     The function will include columns with colnames that follows the conventional column names as given after using \code{standardize_columns}.
#'     In addition, column names that are the same as the standardized names but without the suffix "kode", will be included into the
#'     specified levels.
#'
#'     As standard, only unique (distinct) rows are output. This can be changed by specifying \code{unique = FALSE}.#'
#'
#' @param data Data frame with data from PJS
#' @param levels PJS-levels from which data should be chosen. Valid values are c("sak", "prove", "delprove", "undersokelse", "resultat",
#'     "konklusjon", "subundersokelse", "subresultat").
#' @param keep_col Column names of columns that should be included in addition to the columns defined by levels.
#' @param remove_col Column names of columns that should be removed even if being at the defined levels.
#' @param unique_rows If \code{TRUE} (default), only unique rows are included in the data frame.
#'
#' @return A data frame with columns from the chosen levels in PJS.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name choose_PJS_levels-deprecated
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Attach packages
#' library(RODBC)
#' library(NVIdb)
#'
#' # Read from PJS
#' journal_rapp <- login_by_credentials_PJS()
#' PJSdata <- sqlQuery(journal_rapp,
#'                   "select *
#'                    from V2_SAK_M_RES
#'                    where aar = 2020 and ansvarlig_seksjon = '01' and innsendelsesnummer = 1",
#'                   as.is = TRUE,
#'                   stringsAsFactors = FALSE)
#' odbcClose(journal_rapp)
#'
#' # Generate two data frames,
#' #  generates data frame with sak, prove, konklusjon
#' s_p_k <- choose_PJS_levels(PJSdata,
#'                            levels = c("sak", "prove", "konklusjon"),
#'                            remove_col = c("vet_distriktnr", "karantene",
#'                                           "kartreferanse", "epi_id", "landnr",
#'                                           "uttatt_parprove", "mottatt_parprove",
#'                                           "eksportland", "importdato",
#'                                           "tidl_eier", "avkom_imp_dyr",
#'                                           "okologisk_drift", "skrottnr",
#'                                           "kjonn", "fodselsdato", "konklnr"),
#'                            unique_rows = TRUE)
#'
#' #  generates data frame with sak, prove, undersokelse and resultat
#' s_p_u_r <- choose_PJS_levels(PJSdata,
#'                              levels = c("sak", "prove", "undersokelse", "resultat"),
#'                              remove_col = c("vet_distriktnr", "karantene",
#'                                             "kartreferanse", "epi_id", "landnr",
#'                                             "uttatt_parprove", "mottatt_parprove",
#'                                             "eksportland", "importdato",
#'                                             "tidl_eier", "avkom_imp_dyr",
#'                                             "okologisk_drift", "skrottnr",
#'                                             "kjonn", "fodselsdato"),
#'                              unique_rows = TRUE)
#' }
#'
NULL

#' @title choose_PJS_levels is Deprecated
#' @description \code{choose_PJS_levels} was deprecated in NVIdb v0.13.0 released
#'     2024-##-##. All PJS related functions have been moved to \code{NVIpjsr}.
#'     Use \code{NVIpjsr::choose_PJS_levels} instead. When attaching packages,
#'     remember to attach \code{NVIdb} before \code{NVIpjsr}.
#' @details The old help pages can be found at \code{help("choose_PJS_levels-deprecated")}.
#'     Information on deprecated function can be found at \code{help("NVIdb-deprecated")}.
#'
#' @param data Data frame with data from PJS
#' @param levels PJS-levels from which data should be chosen. Valid values are c("sak", "prove", "delprove", "undersokelse", "resultat",
#'     "konklusjon", "subundersokelse", "subresultat").
#' @param keep_col Column names of columns that should be included in addition to the columns defined by levels.
#' @param remove_col Column names of columns that should be removed even if being at the defined levels.
#' @param unique_rows If \code{TRUE} (default), only unique rows are included in the data frame.
#'
#' @export
#' @keywords internal
#'

choose_PJS_levels <- function(data,
                              levels,
                              keep_col = NULL,
                              remove_col = NULL,
                              unique_rows = TRUE) {

  # DEPRECATED ----
  .Deprecated(new = "choose_PJS_levels",
              package = "NVIdb",
              msg = paste("'choose_PJS_levels' is replaced by
                          'NVIpjsr::choose_PJS_levels'"))

  if (isTRUE(NVIcheckmate::check_package(x = "NVIpjsr", type = "installed"))) {
    select_statement <- NVIpjsr::choose_PJS_levels(data = data,
                                                   levels = levels,
                                                   keep_col = keep_col,
                                                   remove_col = remove_col,
                                                   unique_rows = unique_rows)

    return(select_statement)
  } else {
    # Argument checking
    # Object to store check-results
    checks <- checkmate::makeAssertCollection()
    # Perform checks
    checkmate::assert_data_frame(data, add = checks)
    checkmate::assert_subset(levels, choices = colnames(NVIdb::PJS_levels[, c(2:dim(NVIdb::PJS_levels)[2])]), empty.ok = FALSE, add = checks)
    checkmate::assert_character(keep_col, null.ok = TRUE, add = checks)
    checkmate::assert_character(remove_col, null.ok = TRUE, add = checks)
    checkmate::assert_logical(unique_rows, add = checks)
    # Report check-results
    checkmate::reportAssertions(checks)

    column_names <- NVIdb::PJS_levels[, c("variable", levels)]
    if (length(levels) > 1) {
      column_names$select <- rowSums(NVIdb::PJS_levels[, levels], na.rm = TRUE)
    } else {
      column_names$select <- NVIdb::PJS_levels[, levels]
    }

    column_names <- subset(column_names, column_names$select > 0)

    # column_names <- union(as.vector(column_names$variable), union(as.vector(sub("kode", "", column_names$variable))))
    column_names <- unique(c(as.vector(column_names$variable), sub("kode", "", as.vector(column_names$variable))))

    if (!is.null(keep_col)) {
      column_names <- unique(c(column_names, keep_col))
    }

    column_names <- setdiff(column_names, remove_col)

    data <- data[, intersect(colnames(data), column_names)]

    if (unique_rows == TRUE) {data <- unique(data)}

    return(data)
  }
}
