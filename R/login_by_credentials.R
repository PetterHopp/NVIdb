#' @export
#' @rdname login

login_by_credentials <- function(dbservice,
                                 dbdriver = NULL,
                                 db = NULL,
                                 dbserver = NULL,
                                 dbport = NULL,
                                 dbprotocol = NULL,
                                 dbinterface = NULL) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # dbservice
  checkmate::assert_character(dbservice, min.chars = 1, len = 1, any.missing = FALSE, add = checks)


  # Identifies if predefined connection parameters are needed
  if (is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol) | is.null(dbinterface)) {
    # Identify if NVIconfig are installed and parameters for dbservice exists.
    NVIcheckmate::assert_package(x = "NVIconfig",
                                 comment = paste0("Parameters for logging into the database '",
                                                  dbservice,
                                                  "' is lacking and NVIconfig with predefined parameters is not installed"),
                                 add = checks)

    if (isTRUE(NVIcheckmate::check_package(x = "NVIconfig"))) {
      NVIcheckmate::assert_choice_character(x = dbservice,
                                            choices = NVIconfig:::dbconnect$dbservice,
                                            ignore.case = TRUE,
                                            comment = paste0("Predefined parameters for logging into the database '",
                                                             dbservice,
                                                             "' is not available in your version of NVIconfig"),
                                            add = checks)

      # Uses the predefined parameters only for parameters with NULL-value
      connect <- NVIconfig:::dbconnect[tolower(dbservice), ]
      if (is.null(dbdriver)) {dbdriver <- connect[, "dbdriver"]}
      if (is.null(db)) {db <- connect[, "db"]}
      if (is.null(dbserver)) {dbserver <- connect[, "dbserver"]}
      if (is.null(dbport)) {dbport <- connect[, "dbport"]}
      if (is.null(dbprotocol)) {dbprotocol <- connect[, "dbprotocol"]}
      if (is.null(dbinterface)) {dbinterface <- connect[, "dbinterface"]}
    }
  }

  # # Produce extra message in case parameters are lacking
  # x_msg <- NVIcheckmate::check_package(x = "NVIconfig")
  #
  # # if predefined connection parameters don't exist for dbservice
  # # TO DO: include possibility for extra message in assert_character
  # if (x_msg != TRUE) {
  # dbdriver
  checkmate::assert_character(dbdriver, min.chars = 1, len = 1, any.missing = FALSE, add = checks)
  # db
  checkmate::assert_character(db, min.chars = 1, len = 1, any.missing = FALSE, add = checks)
  # dbserver
  checkmate::assert_character(dbserver, min.chars = 1, len = 1, any.missing = FALSE, add = checks)
  # dbport
  checkmate::assert_character(dbport, len = 1, any.missing = FALSE, add = checks)
  # dbprotocol
  checkmate::assert_character(dbprotocol, min.chars = 1, len = 1, any.missing = FALSE, add = checks)
  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RODBC", "RPostgreSQL"), add = checks)
  # }

  # credentials
  NVIcheckmate::assert_credentials(dbservice, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # # Error handling
  # # 1. keyring package is missing
  # # Use of require is avoided as loading packages should be avoided in package functions
  # # This implies that there is no check of keyring is correctly installed
  # if (!is.element("keyring", utils::installed.packages()[, 1])) {
  #   stop("Package keyring need to be installed for this function to work")
  # }
  #
  # # 2. Credentials for dbservice are missing from the user profile
  # if (!is.element(tolower(dbservice), tolower(keyring::key_list()[, 1]))) {
  #   stop(paste("Username and password for",
  #              dbservice,
  #              "is not available for the current user on this computer"))
  # }
  #
  # # 3. Parameters for db-connection is missing
  # if ((is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) &
  #     !tolower(dbservice) %in% tolower(NVIconfig:::dbconnect$dbservice)) {
  #   stop(paste("Parameters for connection to",
  #              dbservice,
  #              "are missing and predefined parameters are not available"))
  # }

  # Identifies the spelling of service with regard to lower and upper case
  # This is used in Connect-statement below to ensure correct spelling when fetching User ID and Password
  dbservice <- keyring::key_list()[which(tolower(keyring::key_list()[, 1]) == tolower(dbservice)), 1]

  if (dbinterface == "odbc") {
    # Connects to db using odbc
    # use tryCatch to remove warning,
    #   look at https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
    connection <- DBI::dbConnect(drv = odbc::odbc(),
                                 Driver = dbdriver,
                                 Server = dbserver,
                                 port = dbport,
                                 Database = db,
                                 UID = as.character(keyring::key_list(dbservice)[2]),
                                 PWD = keyring::key_get(dbservice, as.character(keyring::key_list(dbservice)[2])))

    if (Sys.getenv("RSTUDIO") == "1") {
      # Opens connection pane in Rstudio.
      # This is not opened automatically when running dbconnect from within a function
      code <- c(match.call()) # This saves what was typed into R

      odbc:::on_connection_opened(
        connection,
        paste(c("library(internal_package)",
                paste("connection <-", gsub(", ", ",\n\t", code))),
              collapse = "\n"))
    }

  }

  if (dbinterface == "RODBC") {
    # Connects to journal_rapp using ODBC
    connection <- RODBC::odbcDriverConnect(paste0("DRIVER=", dbdriver,
                                                  ";Database=", db,
                                                  ";Server=", dbserver,
                                                  ";Port=", dbport,
                                                  ";PROTOCOL=", dbprotocol,
                                                  ";UID=", as.character(keyring::key_list(dbservice)[2]),
                                                  ";PWD=", keyring::key_get(dbservice, as.character(keyring::key_list(dbservice)[2]))))
  }

  if (dbinterface == "RPostgreSQL") {
    # Connects to journal_rapp using ODBC
    connection <- RPostgreSQL::dbConnect(drv = DBI::dbDriver(dbdriver),
                                         host = dbserver,
                                         port = dbport,
                                         dbname = db,
                                         user = as.character(keyring::key_list(dbservice)[2]),
                                         password = keyring::key_get(dbservice, as.character(keyring::key_list(dbservice)[2])))
  }

  return(connection)
}




#' @export
#' @rdname login

login_by_credentials_PJS <- function(dbinterface = NULL) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Identify if NVIconfig are installed.
  NVIcheckmate::assert_package(x = "NVIconfig", add = checks)
  # credentials
  NVIcheckmate::assert_credentials(x = "PJS", add = checks)
  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)


  connection <- NVIdb::login_by_credentials(dbservice = "PJS", dbinterface = dbinterface)

  return(connection)
}



#' @export
#' @rdname login

login_by_credentials_EOS <- function(dbinterface = NULL) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Identify if NVIconfig are installed.
  NVIcheckmate::assert_package(x = "NVIconfig", add = checks)
  # credentials
  NVIcheckmate::assert_credentials(x = "EOS", add = checks)
  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  connection <- NVIdb::login_by_credentials(dbservice = "EOS", dbinterface = dbinterface)

  return(connection)
}
