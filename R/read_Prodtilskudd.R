#' @title Read Register for søknad om produksjonstilskudd
#' @description Functions to to read and copy versions of the produksjonstilskuddsregister.
#' @details The produksjonstilskuddsregister includes information on number of animals that the produsent has applied subsidies for at the
#'     counting dates. Since 2017, the counting dates are in March and October. Landbruksdirektoratet provides three to four versions of the
#'     register for each counting date. The functions automatically selects the last updated version of the register.
#'
#'     \code{read_Prodtilskudd} Reads the produksjonstilskuddsregister into a data frame. The function gives options to select year and season The standard
#'     settings will read in the files from NVI's internal network and select the latest updated file for both spring and autumn and combine them
#'     into one file. If changing the from_path, the function can be used to read the translation file from other directories. This can be useful
#'     if having a stand alone app with no connection the NVI's internal network. In other cases, it should be avoided.
#'
#'     \code{copy_Prodtilskudd} copies the source produksjonstilskuddsregister for each of the year and seasons selected to a given directory.
#'
#' @param from_path Path for the produksjonstilskuddsregister.
#' @param to_path Target path for the files with the produksjonstilskuddsregister.
#' @param Pkode_year The year(s) from which the register should be read. Options is "last", or a vector with one or more years.
#' @param Pkode_month the month for which the register should be read. The options are c("05", "10", "both", "last") for Pkode_year = 2017
#'     and c("03", "10", "both", "last") for Pkode_year >= 2018.
#'
#' @return \code{read_Prodtilskudd} One or more data frame(s) with the produksjonstilskuddsregister for each of the year and seasons selected.
#'     If the options Pkode_year = "last" and Pkode_month = "last" is given, one file with the last produksjonstilskuddsregister is given.
#'
#'     \code{copy_Prodtilskudd} copies the source produksjonstilskuddsregister for each of the year and seasons selected. If the target file
#'     already exists, the source files are copied only when newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' Pkode_last <- read_Prodtilskudd()
#'
#' # Reading from standard directory at NVI's network and selecting a specific version of the register
#' Pkode201903 <- read_Prodtilskudd(Pkode_year = "2019", Pkode_month = "03")
#' }
#'
read_Prodtilskudd <- function(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                              Pkode_year = "last",
                              Pkode_month = "both") {
  
  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  # if (endsWith(from_path, "/")) {
  #   checkmate::assert_directory_exists(substr(from_path, 1, nchar(from_path) - 1), access = "r", add = checks)
  # } else {
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # }
  checkmate::assert_subset(Pkode_month, choices = c("both", "last", "01", "03", "05", "07", "10", "12"), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  checkmate::assert(checkmate::check_integerish(as.numeric(Pkode_year[which(!grepl('[:alpha:]', Pkode_year))]),
                                                lower = 1995,
                                                upper = as.numeric(format(Sys.Date(), "%Y")),
                                                any.missing = FALSE,
                                                unique = TRUE),
                    # checkmate::check_character(Pkode_year, min.chars = 4, min.len = 1, any.missing = FALSE),
                    checkmate::check_choice(Pkode_year, choices = c("last")))
  
  
  # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE SELECTED VERSIONS OF EXTRACTS FROM PKODEREGISTERET
  filelist <- select_prodtilskudd_files(from_path = from_path,
                                        Pkode_year = as.character(Pkode_year),
                                        Pkode_month = Pkode_month)
  
  # Read data for the selected year and months from Pkoderegisteret and combine into one dataframe
  for (i in 1:dim(filelist)[1]) {
    
    # Identifies column names with fylke, kommune and prodnr
    # thereby these are flexible if input files changes.
    colchar <- utils::read.csv2(file.path(set_dir_NVI("Prodtilskudd"), "FormaterteData", filelist[i, "filename"]),
                                header = FALSE,
                                nrow = 1,
                                fileEncoding = "UTF-8")
    
    colchar <- colchar[which(regexpr("kom", colchar, ignore.case = TRUE) > 0 |
                               regexpr("fylk", colchar, ignore.case = TRUE) > 0 |
                               regexpr("prodn", colchar, ignore.case = TRUE) > 0)]
    
    colchars <- as.vector(rep("character", length(colchar)))
    names(colchars) <- colchar
    
    # read single files
    tempdf <- read_csv_file(filename = filelist[i, "filename"],
                            from_path = from_path,
                            options = list(colClasses = colchars,
                                           fileEncoding = "UTF-8"))
    if (exists("df1")) {
      df1[setdiff(names(tempdf), names(df1))] <- NA
      tempdf[setdiff(names(df1), names(tempdf))] <- NA
      df1 <- rbind(df1, tempdf)
    } else {
      df1 <- tempdf
    }
  }
  
  # TO DO: COMBINE SPRING AND AUTOMN INTO ONE FILE IF month = "both"
  
  # Standardize column names
  # To be replaced by standardizing column names in the source files
  columnnames <- colnames(df1)
  columnnames <- sub("Prodnr", "prodnr", columnnames)
  colnames(df1) <- columnnames
  
  # Return dataframe with data for all selected year and months
  return(df1)
}
