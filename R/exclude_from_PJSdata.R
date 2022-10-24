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
#' @export
#' @examples
#' \dontrun{
#' # cleaning sak_m_res
#'  sak_m_res <- exclude_from_PJSdata(PJSdata = sak_m_res,
#'                                    abroad = "exclude",
#'                                    quality = "exclude")
#' }
#'
exclude_from_PJSdata <- function(PJSdata, abroad = "exclude", quality = "exclude") {

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
