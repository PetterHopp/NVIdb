#' @title Deprecated Functions in Package NVIdb
#' @description These functions are provided for compatibility with older
#'     versions of NVIdb only, and may be defunct as soon as the next release.
#'     When possible, alternative functions are mentioned. Help pages for
#'     deprecated functions are available at \code{help("<function>-deprecated")}.
#' @details \code{add_produsent} was deprecated from v0.8.0 released 2022-08-25
#'     as other properties
#'     than 'gjeldende_prodnr8' could not be included without breaking backward
#'     compatibility. Use \code{\link{add_produsent_properties}} instead and ensure
#'     to set the parameter \code{impute_old_when_missing = TRUE} when translating
#'     from "prodnr8" to "gjeldende_prodnr8" and set the parameter
#'     \code{impute_old_when_missing = FALSE} when translating from "prodnr8" to
#'     other properties.
#'
#' \code{set_PAT}, \code{get_PAT}, and \code{remove_PAT} was deprecated from
#'     v0.11.0 released 2023-09-22. The functions were never taken into use.
#'     Functions from the much better package \code{gitcreds} should be used instead.
#'
#' \code{login_EOS}, \code{login_by_input_EOS}, and \code{login_by_credentials_EOS}
#'     was deprecated from v0.##.# released 2024-##-##. The functions were rarely
#'     used and are unnecessary wrappers around the login-functions. The
#'     login-functions with the argument \code{dbservice} = "EOS" should be used
#'     instead, see \code{\link{login}}.
#'
#' \code{set_credentials_EOS} was deprecated from v0.##.# released 2024-##-##.
#'     The function was rarely used and is an unnecessary wrapper around
#'      \code{set_credentials}. \code{\link{set_credentials}} with the argument
#'      \code{dbservice} = "EOS" should be used instead.
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
#' add_produsent(...) ### -- use add_produsent_properties() instead.
#' set_PAT(...) ### -- use gitcreds::gitcreds_set() instead.
#' get_PAT(...) ### -- use gitcreds::gitcreds_get() instead.
#' remove_PAT(...) ### -- use gitcreds::gitcreds_delete() instead.
#' login_EOS(...) ### -- use login("EOS") instead.
#' login_by_input_EOS(...) ### -- use login_by_input("EOS") instead.
#' login_by_credentials_EOS(...) ### -- use login_by_credentials("EOS") instead.
#' set_credentials_EOS(...) ### -- use set_credentials("EOS") instead.
#' }
NULL
