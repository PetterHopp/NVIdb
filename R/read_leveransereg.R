#' @title Read Leveranseregisteret for slakt
#' @description Functions to read Leveranseregisteret for slakt.
#' @details The Leveranseregisteret for slakt includes information on carcasses
#'     delivered to slaughter. The register include identity of the farmer,
#'     slaughterhouse, date of slaughter, animal species, category (age group
#'     and sex), and weight. For poultry the individual animal is not reported,
#'     but number of slaughtered poultry per categories, slaughterhouse and date.
#'
#' \code{read_leveransereg} reads the Leveranseregisteret for slakt into a
#'     data frame. The standard settings will read in the files from NVI's
#'     internal network. If changing the \code{from_path}, the function can be
#'     used to read Leveranseregisteret from other directories. This can be
#'     useful if having a stand alone app with no connection the NVI's internal
#'     network. In other cases, it should be avoided.
#'
#' @param from_path Path for Leveranseregisteret
#' @param filename The name of the file with Leveranseregisteret
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[data.table:fread]{data.table::fread}}}{\code{data.table::fread}}.
#'
#' @return \code{read_LevReg} A data frame with Leveranseregisteret as in selected csv-file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' LevReg2019 <- read_leveransereg(filename = "LevReg_201901_201912.csv")
#' }
#'
read_leveransereg <- function(filename,
                              from_path = paste0(set_dir_NVI("LevReg"), "FormaterteData/"),
                              ...) {

  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_read_functions(filename = filename, from_path = from_path, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # READ DATA ----
  # reads header and identifies characters by using NVIdb::standardize_columns
  colclasses <- standardize_columns(data = file.path(from_path, filename), property = "colclasses")

  # Read leveranseregisteret
  # levreg <- read_csv_file(filename = filename,
  #                         from_path = from_path,
  #                         options = list(colClasses = colclasses, fileEncoding = "UTF-8"),
  #                         dec = ",",
  #                         ...)
  df1 <- data.table::fread(file = file.path(from_path, filename),
                           colClasses = colclasses,
                           encoding = "UTF-8",
                           dec = ",",
                           showProgress = FALSE,
                           data.table = FALSE,
                           ...)

  return(df1)
}
