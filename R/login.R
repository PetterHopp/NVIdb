#' @title Log in to data base services
#' @description  Log in to NVI's data base services, in particular journal_rapp/PJS.
#' @details The NVI has access to several database services. These functions log
#'     in to such services. The functions provides methods to either log in using
#'     credentials set in the user profile by \code{\link{set_credentials}} or use
#'     input windows for username and password. Thereby the hard coding of
#'     username and password can be avoided.
#'
#'     \code{login} is a general function for connecting to databases, where all
#'     necessary connection parameters like server name and database name of the
#'     database must be input. The database provider can inform you on the
#'     connection parameters for their database. In the case that one login to
#'     a database service for which the connection parameters have been predefined
#'     (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
#'     to provide the parameter \code{dbservice}, for example \code{dbservice} =
#'     "EOS".
#'
#'     Depending on whether username and password have been saved in the users
#'     profile at the current computer or not, the user is asked to input credentials.
#'
#'     \code{login_by_input} is a general function for connecting to databases,
#'     where all necessary connection parameters like server name and database name
#'     of the database must be input. The database provider can inform you on the
#'     connection parameters for their database. In the case that one login to
#'     a database service for which the connection parameters have been predefined
#'     (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
#'     to provide the parameter \code{dbservice}. The user is always asked to input
#'     username and password.
#'
#'     \code{login_by_credentials} is a general function for connecting to databases,
#'     where all necessary connection parameters like server name and database name
#'     of the database must be input. The database provider can inform you on the
#'     connection parameters for their database. In the case that one login to
#'     a database service for which the connection parameters have been predefined
#'     (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
#'     to provide the parameter \code{dbservice}. The user is never asked for username
#'     and password, and the function can only be used when the credentials previously
#'     have been set in the user's profile at the current computer.
#'
#'     \code{login_PJS}, \code{login_by_input_PJS}, and \code{login_by_credentials_PJS}
#'     are wrappers for the functions above where the specifications for the
#'     database journal_rapp/PJS have been pre set. The user only need to input
#'     username and password. In the case that the username and password for
#'     journal_rapp/PJS have been stored in the user profile at the current
#'     computer, the user is automatically logged in to journal_rapp. If the
#'     password is no longer valid, an error occur. If so, the user must update
#'     the username and password by \code{\link{set_credentials_PJS}}.
#'
#'     The wrapper functions \code{login_EOS}, \code{login_by_input_EOS}, and
#'     \code{login_by_credentials_EOS} have been deprecated.
#'
#'     The login functions returns an open ODBC-channel to the database service.
#'     The database can then be queried by using functions in the package used for
#'     data base interface. The data base interface must be one of \code{odbc},
#'     \code{RODBC} or, \code{RPostgreSQL}. The default is given in NVIconfig and is
#'     \code{RODBC} for "SQL server" and \code{RPostgreSQL} for "PostgreSQL".
#'
#'     When the session is finished, the script shall close the ODBC-channel by
#'     \code{odbcClose("myodbcchannel")} or \code{odbcCloseAll} when using RODBC.
#'
#' @param dbservice [\code{character(1)}]\cr
#' Name of the database service, for example "PJS" or "EOS".
#'     For database services where one don't use the premade wrappers, the name
#'     can be chosen freely, but must be the same as used in \code{\link{set_credentials}}.
#' @param dbdriver [\code{character(1)}]\cr
#' Name of database engine. Defaults to \code{NULL}.
#' @param db [\code{character(1)}]\cr
#' Name of database. Defaults to \code{NULL}.
#' @param dbserver [\code{character(1)}]\cr
#' Name of database server. Defaults to \code{NULL}.
#' @param dbport [\code{character(1)}]\cr
#' Port. Defaults to \code{NULL}.
#' @param dbprotocol [\code{character(1)}]\cr
#' Protocol to be used. Defaults to \code{NULL}.
#' @param dbinterface [\code{character(1)}]\cr
#' The R-package that is used for interface towards the data
#'     base. Defaults to \code{NULL}.
#' @param dbtext [\code{character(1)}]\cr
#' Gives the possibility of showing another name than the dbservice
#'     in the windows asking for username and password when using
#'     \code{login_by_input}. Defaults to \code{NULL}.
#' @param \dots Other arguments to be passed from the wrappers to
#'     \code{login_by_credentials} or \code{login_by_input}.
#' @return An open ODBC-channel to the database service.
#' @family Log in functions
#' @seealso  \code{\link{set_credentials}}
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' library(RODBC)
#' journal_rapp <- login_PJS()
#' # Reads hensiktregistret from PJS
#' hensikter <- sqlQuery(journal_rapp,
#'                        "select * from v_hensikt",
#'                        as.is = TRUE,
#'                        stringsAsFactors = FALSE)
#' #
#' odbcClose(journal_rapp)
#' }
#'
login <- function(dbservice,
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
      NVIcheckmate::assert_choice_character(x = dbservice, choices = NVIconfig:::dbconnect$dbservice, ignore.case = TRUE,
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

  # Report check-results
  checkmate::reportAssertions(checks)

  # # Error handling
  #
  # # 3. Parameters for db-connection is missing
  # if ((is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) &
  #     !tolower(dbservice) %in% tolower(NVIconfig:::dbconnect$dbservice)) {
  #   stop(paste("Parameters for connection to",
  #              dbservice,
  #              "are missing and predefined parameters are not available"))
  # }
  #
  # # Identifies connection parameters for predefined dbservices
  # # Uses the predefined parameters only for parameters with NULL-value
  # if (is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) {
  #   connect <- NVIconfig:::dbconnect[tolower(dbservice), ]
  #   if (is.null(dbdriver)) {dbdriver <- connect[, "dbdriver"]}
  #   if (is.null(db)) {db <- connect[, "db"]}
  #   if (is.null(dbserver)) {dbserver <- connect[, "dbserver"]}
  #   if (is.null(dbport)) {dbport <- connect[, "dbport"]}
  #   if (is.null(dbprotocol)) {dbprotocol <- connect[, "dbprotocol"]}
  # }
  #

  #   # Check if credentials for PJS is stored in the user profile
  # if (!is.element(tolower(dbservice), tolower(keyring::key_list()[, 1]))) {
  #   # 2. Credentials for PJS are missing from the user profile
  #   login_by_input(dbservice,
  #                  dbdriver,
  #                  db,
  #                  dbserver,
  #                  dbport,
  #                  dbprotocol)
  # } else {
  #   login_by_credentials(dbservice,
  #                        dbdriver,
  #                        db,
  #                        dbserver,
  #                        dbport,
  #                        dbprotocol)
  # }
  # Use check for saved credentials to chose between login_by_input and login_by_credentials
  if (isTRUE(NVIcheckmate::check_credentials(dbservice))) {
    # If credentials are saved for the user profile
    login_by_credentials(dbservice,
                         dbdriver,
                         db,
                         dbserver,
                         dbport,
                         dbprotocol,
                         dbinterface)
  } else {
    # If credentials are missing from the user profile
    login_by_input(dbservice,
                   dbdriver,
                   db,
                   dbserver,
                   dbport,
                   dbprotocol)
  }

}



#' @export
#' @rdname login

login_PJS <- function(dbinterface = NULL, ...) {

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # dbinterface
  checkmate::assert_choice(dbinterface, choices = c("odbc", "RPostgreSQL", "RODBC"), null.ok = TRUE, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # Set service to PJS
  dbservice <- "PJS"

  # Use check for saved credentials to chose between login_by_input and login_by_credentials
  if (isTRUE(NVIcheckmate::check_credentials(dbservice))) {
    # If credentials are saved for the user profile
    login_by_credentials(dbservice, dbinterface = dbinterface, ...)
  } else {
    # If credentials are missing from the user profile
    login_by_input(dbservice, dbinterface = dbinterface, ...)
  }
}
