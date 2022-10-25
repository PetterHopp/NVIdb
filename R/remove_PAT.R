#' @export
#' @rdname set_PAT

remove_PAT <- function(service) {

  # ARGUMENT CHECKING ----
  checkmate::assert_character(x = service, min.chars = 1, len = 1, any.missing = FALSE)

  # REMOVE ALL EXISTING CREDENTIALS FOR service
  # Checks if there are registered PAT for the database service
  # Removes the service until no more service are registered
  while (is.element(tolower(service), tolower(keyring::key_list()[, 1]))) {
  # Identifies the spelling of service with regard to lower and upper case
  # This is used in Connect-statement below to ensure correct spelling when fetching User ID
    services <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(service)), 1]
    usernames <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(service)), 2]

    # Removes the key for all combinations of service and username
      for (i in 1:length(services)) {
        keyring::key_delete(service = services[i], username = usernames[i])
      }
  }
}
