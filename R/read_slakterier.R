#' @title Read register of slakterier
#' @description Read register of slakterier and includes information on name,
#'     municipality, codes used for slakterier in various registers, and 
#'     coordinates.
#' @details \code{read_slakterier} combines information in PJS adresseregister 
#'     with a separate register maintaining various codes in other registers, 
#'     mainly EFTA number and ID used in Leveranseregisteret for slakt. 
#'
#' \code{read_slakterier} reads the Leveranseregisteret for slakt into a
#'     data frame. The standard settings will read in the files from NVI's
#'     internal network. If changing the \code{from_path}, the function can be
#'     used to read the files from other directories. This can be
#'     useful if having a stand alone app with no connection the NVI's internal
#'     network. In other cases, it should be avoided.
#'
#' @param filename [\code{list}]\cr
#' File names of the source files for the translation table. Defaults to
#'     list("slakterier.xlsx").
#' @template from_path_add
#' @param \dots	Other arguments to be passed to
#'     \ifelse{html}{\code{\link[utils:read.csv2]{utils::read.csv2}}}{\code{utils::read.csv2}}.
#'
#' @return A data frame with slakterier and meta information.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' slakterier <- read_slakterier()
#' }
#'

read_slakterier <- function(filename,
                            from_path = file.path(set_dir_NVI("LevReg", slash = FALSE), "StotteData"),
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
  
  
  slakterikoder <- read.xlsx(xlsxFile = paste0(set_dir_NVI("LevReg"), "StotteData/slakterikoder.xlsx"))
  slakterikoder <- add_kommune_fylke(slakterikoder, code_column = "komnr", new_column = "gjeldende_komnr")
  
  journal_rapp <- login_by_credentials(dbservice = "PJS", dbinterface = "odbc")
  slakterier <- DBI::dbGetQuery(con = journal_rapp,
                                statement = paste("select *",
                                                  "from v_registere_alle",
                                                  "where kategoritype='AUTO' or kategoritype='EGGP'"))
  
  slakterier <- slakterier %>%
    mutate(navn = case_when(kategoritype == "EGGP" & identifikator == "103" ~ "Nortura Elverum",
                            kategoritype == "EGGP" & identifikator == "113" ~ "J\u00E6rkylling",
                            TRUE ~ navn))
  # Lukker kobling mot journal_rapp
  DBI::dbDisconnect(journal_rapp)
  
  slakterikoder <- slakterikoder %>%
    left_join(slakterier[, c("kategoritype", "identifikator", "navn")], by = c("kategoritype", "identifikator")) %>%
    mutate(eier_lokalitet = coalesce(navn, anleggnavn))
  
  # Return dataframe with data for slakterier
  return(slakterikoder)
}
