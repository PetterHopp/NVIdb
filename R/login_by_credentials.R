#' @export
#' @rdname login

login_by_credentials <- function(dbservice,
                                 dbdriver = NULL,
                                 db = NULL,
                                 dbserver = NULL,
                                 dbport = NULL,
                                 dbprotocol = NULL) {

  # Error handling
  # 1. keyring package is missing
  # Use of require is avoided as loading packages should be avoided in package functions
  # This implies that there is no check of keyring is correctly installed
  if (!is.element("keyring", utils::installed.packages()[, 1])) {
    stop("Package keyring need to be installed for this function to work")
  }

  # 2. Credentials for dbservice are missing from the user profile
  if (!is.element(tolower(dbservice), tolower(keyring::key_list()[, 1]))) {
    stop(paste("Username and password for",
               dbservice,
               "is not available for the current user on this computer"))
  }

  # 3. Parameters for db-connection is missing
  if ((is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) &
      !tolower(dbservice) %in% tolower(NVIconfig:::dbconnect$dbservice)) {
    stop(paste("Parameters for connection to",
               dbservice,
               "are missing and predefined parameters are not available"))
  }

  # Identifies connection parameters for predefined dbservices
  # Uses the predefined parameters only for parameters with NULL-value
  if (is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) {
    connect <- NVIconfig:::dbconnect[tolower(dbservice), ]
    if (is.null(dbdriver))   {dbdriver   <- connect[, "dbdriver"]}
    if (is.null(db))         {db         <- connect[, "db"]}
    if (is.null(dbserver))   {dbserver   <- connect[, "dbserver"]}
    if (is.null(dbport))     {dbport     <- connect[, "dbport"]}
    if (is.null(dbprotocol)) {dbprotocol <- connect[, "dbprotocol"]}
  }

  # Identifies the spelling of service with regard to lower and upper case
  # This is used in Connect-statement below to ensure correct spelling when fetching User ID and Password
  dbservice <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(dbservice)), 1]

  # Connects to journal_rapp using ODBC
  odbcConnection <- RODBC::odbcDriverConnect(paste0("DRIVER=", dbdriver,
                                                    ";Database=", db,
                                                    ";Server=", dbserver,
                                                    ";Port=", dbport,
                                                    ";PROTOCOL=", dbprotocol,
                                                    ";UID=", as.character(keyring::key_list(dbservice)[2]),
                                                    ";PWD=", keyring::key_get(dbservice, as.character(keyring::key_list(dbservice)[2]))))

  return(odbcConnection)
}




#' @export
#' @rdname login

login_by_credentials_PJS <- function() {

  odbcConnection <- NVIdb::login_by_credentials(dbservice = "PJS")

  return(odbcConnection)
}



#' @export
#' @rdname login

login_by_credentials_EOS <- function() {

  odbcConnection <- NVIdb::login_by_credentials(dbservice = "EOS")

  return(odbcConnection)
}

