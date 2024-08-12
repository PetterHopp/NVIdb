#' @title exclude rows from PJS-data
#' @description Performs common subsetting of PJS-data by excluding rows
#'
#' @details Performs common cleaning of PJSdata by removing samples that usually
#'     should not be included when analyzing PJSdata. The cleaning is dependent
#'     on having the following columns eier_lokalitettype, eierlokalitetnr and
#'     hensiktkode.
#'
#'     \code{abroad = "exclude"} will exclude samples that have eier_lokalitet
#'     of type "land" and eier_lokalitetnr being different from NO. Samples
#'     registered on other types than LAND are not excluded.
#'
#'     \code{quality = "exclude"} will exclude all samples registered s quality
#'     assurance and ring trials, i.e. hensiktkode starting with "09".
#'
#' @param PJSdata Data frame with data extracted from PJS.
#' @param abroad If equal "exclude", samples from abroad are excluded. Allowed
#'     values are c("exclude", "include").
#' @param quality If equal "exclude", samples registered as quality assurance
#'     and ring trials are excluded. Allowed values are c("exclude", "include").
#'
#' @return data frame without excluded PJS-data.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name exclude_from_PJSdata-deprecated
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # cleaning sak_m_res
#' sak_m_res <- exclude_from_PJSdata(PJSdata = sak_m_res,
#'                                    abroad = "exclude",
#'                                    quality = "exclude")
#' }
#'
NULL

#' @title exclude_from_PJSdata is Deprecated
#' @description \code{exclude_from_PJSdata} was deprecated in NVIdb v0.13.0 released
#'     2024-##-##. All PJS related functions have been moved to \code{NVIpjsr}.
#'     Use \code{NVIpjsr::exclude_from_PJSdata} instead. When attaching packages,
#'     remember to attach \code{NVIdb} before \code{NVIpjsr}.
#' @details The old help pages can be found at \code{help("exclude_from_PJSdata-deprecated")}.
#'     Information on deprecated function can be found at \code{help("NVIdb-deprecated")}.
#'
#' @param PJSdata Data frame with data extracted from PJS.
#' @param abroad If equal "exclude", samples from abroad are excluded. Allowed
#'     values are c("exclude", "include").
#' @param quality If equal "exclude", samples registered as quality assurance
#'     and ring trials are excluded. Allowed values are c("exclude", "include").
#'
#' @export
#' @keywords internal
#'

exclude_from_PJSdata <- function(PJSdata, abroad = "exclude", quality = "exclude") {

  # DEPRECATED ----
  .Deprecated(new = "exclude_from_PJSdata",
              package = "NVIdb",
              msg = paste("'exclude_from_PJSdata' is replaced by
                          'NVIpjsr::exclude_from_PJSdata'"))

  if (isTRUE(NVIcheckmate::check_package(x = "NVIpjsr", type = "installed"))) {
    select_statement <- NVIpjsr::exclude_from_PJSdata(PJSdata = PJSdata,
                                                      abroad = abroad,
                                                      quality = quality)

    return(select_statement)
  } else {
    # ARGUMENT CHECKING ----
    # Object to store check-results
    checks <- checkmate::makeAssertCollection()

    # Perform checks
    # pjsDATA
    checkmate::assert_data_frame(PJSdata, add = checks)
    # abroad
    checkmate::assert_choice(abroad, choices = c("exclude", "include"), add = checks)
    # quality
    checkmate::assert_choice(quality, choices = c("exclude", "include"), add = checks)

    # Report check-results
    checkmate::reportAssertions(checks)


    # PERFORM CLEANING ----
    # Remove samples from abroad
    if (abroad == "exclude") {
      # Remove eier_lokalitet with landnr different from Norway in address

      PJSdata <- subset(PJSdata,
                        PJSdata$eier_lokalitettype != "LAND" |
                          is.na(PJSdata$eier_lokalitettype) |
                          (PJSdata$eier_lokalitettype == "LAND" & PJSdata$eier_lokalitetnr == "NO"))

    }


    if (quality == "exclude") {
      # Delete qualty assurance
      PJSdata <- subset(PJSdata,
                        substr(PJSdata$hensiktkode, 1, 2) != "09" |
                          is.na(PJSdata$hensiktkode))

    }

    return(PJSdata)
  }
}
