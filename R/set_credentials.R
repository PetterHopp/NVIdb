#' @title Manage username and password (credentials) for database services at NVI
#' @description Save or remove the current user's username and password for
#'     the data base services at the Norwegian Veterinary Institute in the
#'     the user profile.
#' @details The Norwegian Veterinary Institute has access to various database
#'     services. To simplify the access to the database services when using R,
#'     the function \code{set_credentials} makes it possible to save the
#'     username and password (credentials) in the user profile at the current
#'     machine. When the username and password have been saved in the user
#'     profile, the functions \code{login} or \code{login_by_credentials} will
#'     automatically log in to the database services without any need of new
#'     input of username and password.
#'
#'     The user profile is not copied between computers. Consequently, if
#'     a user runs scripts with \code{login} on different computers, the
#'     credentials have to be saved at each computer separately.
#'
#'     \code{set_credentials(dbservice)} is used to set the username and
#'     password for a database service. The username and password are input
#'     using windows and saved in the users profile at the current computer.
#'     When the password for the database service have been changed,
#'     \code{set_credentials(dbservice)} can be used to update the password.
#'
#'     \code{set_credentials_PJS} is a wrapper for \code{set_credentials(dbservice)}
#'     used to set the username and password for journal_rapp/PJS. Journal_rapp
#'     has views to information in PJS and some other internal databases at NVI.
#'     The username and password are the same as for PJS. When the password for
#'     PJS have been changed, \code{set_credentials_PJS} can be used to update
#'     the password.
#'
#'     The wrapper function \code{set_credentials_EOS} is deprecated.
#'     Use \code{set_credentials(dbservice = "EOS")} instead.
#'
#'     \code{remove_credentials(dbservice)} is used to delete the credentials
#'     for a database service from the user's profile.
#'
#' @param dbservice [\code{character(1)}]\cr
#' Name of the database service, for example "PJS" or "EOS". For database
#'     services where one don't use the premade wrappers, the name can be
#'     chosen freely, but must be the same as used in \code{login} and
#'     \code{login_by_credentials}.
#' @return \code{set_credentials} The username and password for a database
#'     service are saved in the user profile at the current computer.
#'
#'     \code{remove_credentials} The username and password for a database
#'     service are deleted from the user profile at the current computer.
#'
#' @seealso \code{\link{login}} and \code{\link{login_by_credentials}}
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @examples
#' \dontrun{
#' set_credentials(dbservice = "dbservice")
#'
#' set_credentials_PJS()
#'
#' set_credentials("EOS")
#'
#' remove_credentials("PJS")
#' }
#'
set_credentials <- function(dbservice) {

  # ARGUMENT CHECKING dbservice ----
  checkmate::assert_character(x = dbservice, min.chars = 1, len = 1, any.missing = FALSE)

  # Removes previously set credentials for the database service
  remove_credentials(dbservice)

  # Open window for input of username to the given dbservice
  # username <- svDialogs::dlgInput(message = paste("Oppgi brukernavn for", dbservice))$res
  username <- askpass::askpass(prompt = paste("Oppgi brukernavn (username) for", dbservice))

  # ARGUMENT CHECKING username ----
  checkmate::assert_character(x = username, min.chars = 1, len = 1, any.missing = FALSE)

  # Open window for input of password to the given dbservice and saves username and password in user's profile
  keyring::key_set(service = dbservice, username = username)
}



#' @export
#' @rdname set_credentials

set_credentials_PJS <- function() {
  # General function to input username and password for a database service
  # Set database service to PJS
  set_credentials(dbservice = "PJS")
}
