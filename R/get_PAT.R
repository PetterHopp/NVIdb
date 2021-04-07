#' @export
#' @rdname set_PAT

get_PAT <- function(service){

  # Error handling
  # 1. keyring package is missing
  # Use of require is avoided as loading packages should be avoided in package functions
  # This implies that there is no check of keyring is correctly installed
  if(!is.element("keyring", utils::installed.packages()[,1])) {
    stop("Package keyring need to be installed for this function to work")
  }

  # 2. Credentials for service are missing from the user profile
  if(!is.element(tolower(service), tolower(keyring::key_list()[,1]))) {
    stop(paste("PAT for",
               service,
               "is not available for the current user on this computer"))
  }

  # Identifies the spelling of service with regard to lower and upper case
  # This is used in Connect-statement below to ensure correct spelling when fetching User ID and Password
  service <- keyring::key_list()[which(tolower(keyring::key_list()[,1])==tolower(service)),1]

  #fetch the PAT
  PAT <- keyring::key_get(service,as.character(keyring::key_list(service)[2]))

  return(PAT)
}
