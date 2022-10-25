#' @title Manage personal access token (PAT) for internet services
#' @description Save or remove the current user's PAT for internet services in the  the user profile.
#' @details For internet services like GitHub, personal access tokens can replace username and password when accessing the service. To simplify
#'     the access to the internet services when using R, the function \code{set_PAT} makes it possible to save the personal access token (PAT) in
#'     the user profile at the current machine. When the PAT has been saved in the user profile, the functions \code{get_PAT} will automatically
#'     get the PAT for use in code accessing the internet service.
#'
#'     The user profile is not copied between computers. Consequently, if a user runs scripts with \code{get_PAT} on different computers,
#'     the PAT has to be saved at each computer separately.
#'
#'     \code{set_PAT(service)} is used to set the PAT for a internet service. The PAT are input using windows and saved in the users profile at
#'     the current computer. When the PAT for the service has been changed, \code{set_PAT(service)} can be used to update the PAT.
#'
#'     \code{get_PAT(service)} is used to get the PAT for a internet service that previously has been saved in the users profile at the current
#'     computer.
#'
#'     \code{remove_PAT(service)} is used to delete the PAT for a internet service from the user's profile.
#'
#' @param service Name of the internet service, for example "GitHub". For internet services where one don't use the premade wrappers, the name can
#'     be chosen freely, but must be the same as used in \code{get_PAT}
#' @return \code{set_PAT} The PAT for a internet service are saved in the user profile at the current computer.
#'
#'     \code{get_PAT} The PAT for a internet service are fetched from the user profile at the current computer to be used in R-scripts.
#'
#'     \code{remove_PAT} The PAT for a internet service are deleted from the user profile at the current computer.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @examples
#' \dontrun{
#' set_PAT("GitHub")
#'
#' get_PAT("GitHub")
#'
#' remove_PAT("GitHub")
#' }
#'
set_PAT <- function(service) {


  # ARGUMENT CHECKING service ----
  checkmate::assert_character(x = service, min.chars = 1, len = 1, any.missing = FALSE)

  # Removes previously set PAT for the database service
  remove_PAT(service)

  # Open window for input of PAT to the given service and saves service and PAT in the user's profile
  # Use the service name as input to username
  keyring::key_set_with_value(service = service,
                              username = service,
                              password = getPass::getPass(paste("Enter your PAT for", service)),
                              keyring = NULL)
}
