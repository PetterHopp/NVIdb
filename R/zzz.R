# Give a startup message if NVIconfig less than v0.2.0 is not installed


#' @title Check if an argument is an installed package of required version
#' @description checks if an package is installed and if it is installed, check if the version is
#'     equal or higher than the required version.
#' @details This function will eventually be included in NVIcheckmate.
#'
#' @param x Object to check.
#' @param version The required version of the installed package. May be \code{NULL}.
#' @return If package of required version is installed, then TRUE, else aerror message
#' @noRd

check_package <- function(x, version = NULL) {
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_character (x, len = 1, min.char = 2)
  checkmate::assert_character(version, len = 1, null.ok = TRUE)
  # Report check-results
  checkmate::reportAssertions(checks)

  # PERFORM CHECK
  # if the package is not installed
  if (!nchar(system.file(package = x)))  {
    res <- paste0("The package '", x, "' is not installed")
  } else {
    # Check if the required version is  installed
    installed_version <- utils::packageDescription(x)$Version
    if (utils::compareVersion(installed_version, version) == -1) {
      res <- paste0("The package '", x, "' version '", installed_version, "' is installed, while version '", version, "' is required." )
    } else {
      res <- TRUE
    }
  }
}


# Do after loading NVIdb
.onAttach <- function(libname, pkgname){

  # check if "NVIconfig" is installed
  msg <- check_package(x = "NVIconfig", version = "0.3.1")

  # Print a startup message if not required version is installed
  if (msg != TRUE) {
    msg <- paste(msg,
                 "You can install 'NVIconfig' with remotes::install_github('NorwegianVeterinaryInstitute/NVIconfig',",
                 "                                                         auth_token = 'your_GitHub-PAT',",
                 "                                                         upgrade = FALSE,",
                 "                                                         build = TRUE)",
                 sep = "\n")
    packageStartupMessage(msg)
  }

  invisible()

}

