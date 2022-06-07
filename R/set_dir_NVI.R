#' @title Set directories for data sources at NVI
#' @description Set the directories for various data sources at NVI's network.
#' @details The Norwegian Veterinary Institute has standard data sources at fixed 
#'     directories. The function returns the standard directory for the given 
#'     data source. Thereby hard coding of the paths may be avoided. 
#'
#' @param datasource The data source that one want to access. The input can be 
#'      abbreviated and case is ignored. To identify short names for the available 
#'      directories, use \code{set_dir_NVI(datasource = "?")}.
#' @return The full path for the directory at NVI's network. The path ends with "/".
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @export
#' @examples
#' \dontrun{
#' # Set path_ProdTilskudd to path for Prodtilskudd at the NVI network
#' prodtilskudd_path <- set_dir_NVI(datasource = "ProdTilskudd")
#' }
#'
set_dir_NVI <- function(datasource) {
  
  # ARGUMENT CHECKING
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  datasource <- NVIcheckmate::match_arg(x = datasource, 
                                        choices = names(NVIconfig:::path_NVI), 
                                        several.ok = FALSE,
                                        ignore.case = TRUE, 
                                        add = checks)
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # # ERROR checking
  # # Check function input
  # datasource <- trimws(tolower(datasource))
  # if (!datasource %in% tolower(names(NVIconfig:::path_NVI))) {
  #   stop(paste0(datasource, " is not a valid input for datasource.","\n",
  #              "Valid inputs are (case insensitive): ", paste(names (NVIconfig:::path_NVI), collapse = ", "),"."))
  # }
  
  # The paths are defined in the package NVIconfig
  # pathname <- unname(NVIconfig:::path_NVI[which(names(NVIconfig:::path_NVI) == datasource)])
  pathname <- unname(NVIconfig:::path_NVI[datasource])
  
  return(pathname)
}
