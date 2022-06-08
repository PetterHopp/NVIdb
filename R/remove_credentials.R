#' @export
#' @rdname set_credentials

remove_credentials <- function(dbservice) {

  # ARGUMENT CHECKING ----
  checkmate::assert_character(x = dbservice, min.chars = 1, len = 1, any.missing = FALSE)

  # REMOVE ALL EXISTING CREDENTIALS FOR dbservice
  # Checks if there are registered credentials for the database service
  # Removes the credentials until no more credentials are registered
  while (is.element(tolower(dbservice), tolower(keyring::key_list()[, 1]))) {
    # Identifies the spelling of service with regard to lower and upper case
    # This is used in Connect-statement below to ensure correct spelling when fetching User ID
    dbservices <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(dbservice)), 1]
    usernames <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(dbservice)), 2]


    # Removes the key for all combinations of dbservice and username
    for (i in 1:length(dbservices)) {
      keyring::key_delete(service = dbservices[i], username = usernames[i])
    }
  }
}
