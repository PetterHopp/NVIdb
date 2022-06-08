#' @title Deprecated Functions in Package NVIdb
#' @description These functions are provided for compatibility with older
#'     versions of NVIdb only, and may be defunct as soon as the next release.
#'     When possible, alternative functions are mentioned. Help pages for
#'     deprecated functions are available at \code{help("<function>-deprecated")}.
#' @details \code{add_produsent} was deprecated 2022-05-02 as other properties
#'     than 'gjeldende_prodnr8' could not be included without breaking backward
#'     compatibility. Use \code{add_produsent_properties} instead and ensure
#'     to set the parameter \code{impute_old_when_missing = TRUE} when translating
#'     from "prodnr8" to "gjeldende_prodnr8" and set the parameter
#'     \code{impute_old_when_missing = FALSE} when translating from "prodnr8" to
#'     other properties.
#'
#' @param \dots (arguments)
#' @return (results)
#' @name NVIdb-deprecated
#' @keywords internal
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' add_produsent(...) ### -- use \code{\link{add_produsent_properties}}   instead
#' }
NULL
