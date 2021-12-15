#' @export
#' @rdname login

login_by_input <- function(dbservice,
                           dbdriver = NULL,
                           db = NULL,
                           dbserver = NULL,
                           dbport = NULL,
                           dbprotocol = NULL,
                           dbtext = NULL) {

  # Error handling
  # 1. Parameters for db-connection is missing
  if ((is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) &
      !tolower(dbservice) %in% tolower(NVIconfig::dbconnect$dbservice)) {
    stop(paste("Parameters for connection to",
               dbservice,
               "are missing and predefined parameters are not available"))
  }

  # Identifies connection parameters for predefined dbservices
  # Uses the predefined parameters only for parameters with NULL-value
  if (is.null(dbdriver) | is.null(db) | is.null(dbserver) | is.null(dbport) | is.null(dbprotocol)) {
    connect <- NVIconfig::dbconnect[tolower(dbservice), ]
    if (is.null(dbdriver))   {dbdriver   <- connect[, "dbdriver"]}
    if (is.null(db))         {db         <- connect[, "db"]}
    if (is.null(dbserver))   {dbserver   <- connect[, "dbserver"]}
    if (is.null(dbport))     {dbport     <- connect[, "dbport"]}
    if (is.null(dbprotocol)) {dbprotocol <- connect[, "dbprotocol"]}
  }

  if (is.null(dbtext)) {dbtext <- dbservice}

  # Connects to database service using ODBC
  odbcConnection <- RODBC::odbcDriverConnect(paste0("DRIVER=", dbdriver,
                                                    ";Database=", db,
                                                    ";Server=", dbserver,
                                                    ";Port=", dbport,
                                                    ";PROTOCOL=", dbprotocol,
                                                    ";UID=",
                                                    svDialogs::dlgInput(message = paste("Oppgi brukernavn for", dbtext))$res,
                                                    ";PWD=",
                                                    getPass::getPass(msg = paste("Oppgi passord for", dbtext)))
  )

  return(odbcConnection)
}


#' @export
#' @rdname login

login_by_input_PJS <- function() {

  # Oppretterknytning mot journal_rapp
  odbcConnection <- login_by_input(dbservice = "PJS")

  return(odbcConnection)
}


#' @export
#' @rdname login

login_by_input_EOS <- function() {

  # Oppretterknytning mot EOS
  odbcConnection <- login_by_input(dbservice = "EOS")

  return(odbcConnection)
}


