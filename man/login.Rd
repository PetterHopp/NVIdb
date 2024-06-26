% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R, R/login_by_credentials.R,
%   R/login_by_input.R
\name{login}
\alias{login}
\alias{login_PJS}
\alias{login_by_credentials}
\alias{login_by_credentials_PJS}
\alias{login_by_input}
\alias{login_by_input_PJS}
\title{Log in to data base services}
\usage{
login(
  dbservice,
  dbdriver = NULL,
  db = NULL,
  dbserver = NULL,
  dbport = NULL,
  dbprotocol = NULL,
  dbinterface = NULL
)

login_PJS(dbinterface = NULL, ...)

login_by_credentials(
  dbservice,
  dbdriver = NULL,
  db = NULL,
  dbserver = NULL,
  dbport = NULL,
  dbprotocol = NULL,
  dbinterface = NULL
)

login_by_credentials_PJS(dbinterface = NULL, ...)

login_by_input(
  dbservice,
  dbdriver = NULL,
  db = NULL,
  dbserver = NULL,
  dbport = NULL,
  dbprotocol = NULL,
  dbinterface = NULL,
  dbtext = NULL
)

login_by_input_PJS(dbinterface = NULL, ...)
}
\arguments{
\item{dbservice}{[\code{character(1)}]\cr
Name of the database service, for example "PJS" or "EOS".
    For database services where one don't use the premade wrappers, the name
    can be chosen freely, but must be the same as used in \code{\link{set_credentials}}.}

\item{dbdriver}{[\code{character(1)}]\cr
Name of database engine. Defaults to \code{NULL}.}

\item{db}{[\code{character(1)}]\cr
Name of database. Defaults to \code{NULL}.}

\item{dbserver}{[\code{character(1)}]\cr
Name of database server. Defaults to \code{NULL}.}

\item{dbport}{[\code{character(1)}]\cr
Port. Defaults to \code{NULL}.}

\item{dbprotocol}{[\code{character(1)}]\cr
Protocol to be used. Defaults to \code{NULL}.}

\item{dbinterface}{[\code{character(1)}]\cr
The R-package that is used for interface towards the data
    base. Defaults to \code{NULL}.}

\item{\dots}{Other arguments to be passed from the wrappers to
\code{login_by_credentials} or \code{login_by_input}.}

\item{dbtext}{[\code{character(1)}]\cr
Gives the possibility of showing another name than the dbservice
    in the windows asking for username and password when using
    \code{login_by_input}. Defaults to \code{NULL}.}
}
\value{
An open ODBC-channel to the database service.
}
\description{
Log in to NVI's data base services, in particular journal_rapp/PJS.
}
\details{
The NVI has access to several database services. These functions log
    in to such services. The functions provides methods to either log in using
    credentials set in the user profile by \code{\link{set_credentials}} or use
    input windows for username and password. Thereby the hard coding of
    username and password can be avoided.

    \code{login} is a general function for connecting to databases, where all
    necessary connection parameters like server name and database name of the
    database must be input. The database provider can inform you on the
    connection parameters for their database. In the case that one login to
    a database service for which the connection parameters have been predefined
    (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
    to provide the parameter \code{dbservice}, for example \code{dbservice =
    "EOS"}.

    Depending on whether username and password have been saved in the users
    profile at the current computer or not, the user is asked to input credentials.

    \code{login_by_input} is a general function for connecting to databases,
    where all necessary connection parameters like server name and database name
    of the database must be input. The database provider can inform you on the
    connection parameters for their database. In the case that one login to
    a database service for which the connection parameters have been predefined
    (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
    to provide the parameter \code{dbservice}. The user is always asked to input
    username and password.

    \code{login_by_credentials} is a general function for connecting to databases,
    where all necessary connection parameters like server name and database name
    of the database must be input. The database provider can inform you on the
    connection parameters for their database. In the case that one login to
    a database service for which the connection parameters have been predefined
    (i.e. PJS, EOS, sea_sites, Fallvilt and Dataflex), it will be sufficient
    to provide the parameter \code{dbservice}. The user is never asked for username
    and password, and the function can only be used when the credentials previously
    have been set in the user's profile at the current computer.

    \code{login_PJS}, \code{login_by_input_PJS}, and \code{login_by_credentials_PJS}
    are wrappers for the functions above where the specifications for the
    database journal_rapp/PJS have been pre set. The user only need to input
    username and password. In the case that the username and password for
    journal_rapp/PJS have been stored in the user profile at the current
    computer, the user is automatically logged in to journal_rapp. If the
    password is no longer valid, an error occur. If so, the user must update
    the username and password by \code{\link{set_credentials_PJS}}.

    The wrapper functions \code{login_EOS}, \code{login_by_input_EOS}, and
    \code{login_by_credentials_EOS} have been deprecated.

    The login functions returns an open ODBC-channel to the database service.
    The database can then be queried by using functions in the package used for
    data base interface. The data base interface must be one of \code{odbc},
    \code{RODBC} or, \code{RPostgreSQL}. The default is given in NVIconfig and is
    \code{RODBC} for "SQL server" and \code{RPostgreSQL} for "PostgreSQL".

    When the session is finished, the script shall close the ODBC-channel by
    \code{odbcClose("myodbcchannel")} or \code{odbcCloseAll} when using \code{RODBC}.
}
\examples{
\dontrun{
library(RODBC)
journal_rapp <- login_PJS()
# Reads hensiktregistret from PJS
hensikter <- sqlQuery(journal_rapp,
                       "select * from v_hensikt",
                       as.is = TRUE,
                       stringsAsFactors = FALSE)
#
odbcClose(journal_rapp)
}

}
\seealso{
\code{\link{set_credentials}}
}
\author{
Petter Hopp Petter.Hopp@vetinst.no
}
\concept{Log in functions}
