#' @title Read Register for søknad om produksjonstilskudd
#' @description Functions to to read and copy versions of the 
#'     produksjonstilskuddsregister.  
#' @details The produksjonstilskuddsregister includes information on number of 
#'     animals that the produsent has applied subsidies for at the counting 
#'     dates. Since 2017, the counting dates are in March and October. 
#'     Landbruksdirektoratet provides three to four versions of the register for 
#'     each counting date. The functions automatically selects the last updated 
#'     version of the register.
#'
#'     \code{read_Prodtilskudd} reads the produksjonstilskuddsregister into a 
#'     data frame. The function gives options to select year and season The 
#'     standard settings will read in the files from NVI's internal network and 
#'     select the latest updated file for both spring and autumn and combine 
#'     them into one file. If changing the from_path, the function can be used 
#'     to read the translation file from other directories. This can be useful
#'     if having a stand alone app with no connection the NVI's internal network. 
#'     In other cases, it should be avoided.
#'     
#'     \code{extracted_date} is used if specific versions of the register is required, 
#'     for example to reproduce the generation of data previously performed 
#'     using an older version of the register.You should also write in the 
#'     \code{extracted_date} in the script to document which version of the 
#'     register that was used. If so, first extract the last available version 
#'     of the register. Find the uttrekkdato in the data, and write in the 
#'     uttrekkdato in \code{extracted_date}. 
#'
#'     \code{copy_Prodtilskudd} copies the source produksjonstilskuddsregister 
#'     for each of the year and seasons selected to a given directory.
#'
#' @param from_path [\code{character(1)}]\cr
#'     Path for the produksjonstilskuddsregister. Defaults to the standard 
#'     directory at the NVI network.
#' @param to_path [\code{character(1)}]\cr 
#'     Target path for the files with the produksjonstilskuddsregister.
#' @param Pkode_year [\code{character}] | [\code{numeric}]\cr 
#'     The year(s) from which the register should be read. Options is "last", or
#'     a vector with one or more years. Defaults to "last".
#' @param Pkode_month [\code{character}]\cr 
#'     The month for which the register should be read. The options are 
#'     c("05", "10", "both", "last") for Pkode_year = 2017 and 
#'     c("03", "10", "both", "last") for Pkode_year >= 2018. Defaults to "both".
#' @param extracted_date [\code{character}]\cr 
#'     The date the data was extracted from the database of the Norwegian 
#'     Agricultural Agency. The format should be "yyyy-mm-dd". Defaults to 
#'     \code{NULL}.
#'
#' @return \code{read_Prodtilskudd} reads one or more data frame(s) with the produksjonstilskuddsregister for each of the year and seasons selected.
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
#' # Reading from standard directory at NVI's network and
#' #     selecting a specific version of the register
#' Pkode201903 <- read_Prodtilskudd(Pkode_year = "2019", Pkode_month = "03")
#' }
#'
read_Prodtilskudd <- function(from_path = paste0(set_dir_NVI("Prodtilskudd"), "FormaterteData/"),
                              Pkode_year = "last",
                              Pkode_month = "both",
                              extracted_date = NULL) {

  # PREPARE ARGUMENT ----
  # Removing ending "/" and "\\" from pathnames
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  # from_path
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  # If extracted_date = NULL, then input "both" and "last" are accepted
  if (is.null(extracted_date)) {
    # Pkode_month
    checkmate::assert_subset(Pkode_month, choices = c("both", "last", "01", "03", "05", "07", "10", "12"), add = checks)
    # Pkode_year
    checkmate::assert(checkmate::check_integerish(as.numeric(Pkode_year[grep('[[:alpha:]]', Pkode_year, invert = TRUE)]),
                                                  lower = 1995,
                                                  upper = as.numeric(format(Sys.Date(), "%Y")),
                                                  any.missing = FALSE,
                                                  all.missing = FALSE,
                                                  unique = TRUE),
                      # checkmate::check_character(Pkode_year, min.chars = 4, min.len = 1, any.missing = FALSE),
                      checkmate::check_choice(Pkode_year, choices = c("last")),
                      add = checks)
  }
  # If extracted_date != NULL, then input "both" and "last" are not accepted
  if (is.null(extracted_date)) {
    # Pkode_month
    NVIcheckmate::assert_subset(Pkode_month,
                                choices = c("01", "03", "05", "07", "10", "12"),
                                comment = "The inputs 'both' and 'last' are not accepted when 'extracted_date' is given",
                                add = checks)
    # Pkode_year
    NVIcheckmate::assert_integerish(as.numeric(Pkode_year[grep('[[:alpha:]]', Pkode_year, invert = TRUE)]),
                                    lower = 1995,
                                    upper = as.numeric(format(Sys.Date(), "%Y")),
                                    any.missing = FALSE,
                                    all.missing = FALSE,
                                    unique = TRUE,
                                    comment = "The input 'last' is not accepted when 'extracted_date' is given",
                                    add = checks)
  }
  # Report check-results
  checkmate::reportAssertions(checks)



  # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE SELECTED VERSIONS OF EXTRACTS FROM PKODEREGISTERET
  filelist <- select_prodtilskudd_files(from_path = from_path,
                                        Pkode_year = as.character(Pkode_year),
                                        Pkode_month = Pkode_month,
                                        extracted_date = extracted_date)

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

###   ----

### select_prodtilskudd_files ----


#' @title List selected files from Søknad om register for produksjonstilskudd
#' @description List selected files with extracts from Søknad om register for produksjonstilskudd.
#' @details Reads the filenames of files with extracts from Søknad om register for produksjonstilskudd into a data frame.
#'     The function gives options to select year and month and path for the files. The function is called from read_Prodtilskudd
#'     and copy_Prodtilskudd.
#'
#' @param from_path Path for the source translation table for PJS-codes
#' @param Pkode_year The year(s) from which the register should be read. Options is "last", or a vector with one or more years.
#' @param Pkode_month the month for which the register should be read. The options are c("05", "10", "both", "last") for Pkode_year = 2017
#'     and c("03", "10", "both", "last") for Pkode_year >= 2018.
#'
#' @return A data frame with filenames of the files with the selected extracts of Prodtilskudd.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @examples
#' \dontrun{
#' # Making the filelist for read_Prodtilskudd or copy_Prodtilskudd
#' filelist <- select_prodtilskudd_files(from_path = from_path,
#'                                       Pkode_year = Pkode_year,
#'                                       Pkode_month = Pkode_month)
#' }
#' @keywords internal

select_prodtilskudd_files <- function(from_path,
                                      Pkode_year,
                                      Pkode_month,
                                      extracted_date) {
  # READ IN ALL FILES IN THE DIRECTORY AND MAKE A LIST OF THE LAST VERSION OF ALL UTREKK FRO PKODEREGISTERET
  filelist <- as.data.frame(list.files(path = from_path, pattern = "csv", ignore.case = TRUE, include.dirs = FALSE),
                            stringsAsFactors = FALSE)
  colnames(filelist) <- "filename"
  filelist$fileinfo <- sub("per ", "", filelist$filename)
  filelist$fileinfo <- sub("fra ", "", filelist$fileinfo)
  filelist$fileinfo <- sub(".csv", "", filelist$fileinfo)
  filelist$pkodeaar <- substr(filelist$fileinfo, 6, 9)
  filelist$pkodemonth <- substr(filelist$fileinfo, 10, 11)
  filelist$content <- sapply(filelist$fileinfo, FUN = find_n_th_word, position = 2)
  filelist <- subset(filelist,
                     tolower(substr(filelist$fileinfo, 1, 5)) == "pkode" &
                       filelist$pkodeaar > "1994" & filelist$pkodeaar < "2099" &
                       filelist$content %in% c("Koordinater", "Uttrekk"))
  filelist$contenttype <- sapply(filelist$fileinfo, FUN = find_n_th_word, position = 4)
  filelist <- subset(filelist, filelist$contenttype == "UTF8")

  filelist$uttrekk_dato <- as.Date(sapply(filelist$fileinfo, FUN = find_n_th_word, position = 3), format = "%Y%m%d")
  max_uttrekk_dato <- stats::aggregate(filelist$uttrekk_dato, by = list(filelist$pkodeaar, filelist$pkodemonth), FUN = max)
  filelist <- merge(filelist, max_uttrekk_dato, by.x = c("pkodeaar", "pkodemonth"), by.y = c("Group.1", "Group.2"))
  filelist <- filelist[, c("filename", "pkodeaar", "pkodemonth", "uttrekk_dato")]
  filelist <- filelist[order(filelist$pkodeaar, filelist$pkodemonth, filelist$uttrekk_dato, decreasing = TRUE), ]

  if (!is.null(extracted_date)) {
    if ("last" %in% Pkode_year) {
      filelist <- filelist[c(1:2), ]
      if (!"both" %in% Pkode_month) {
        if ("last" %in% Pkode_month) {
          filelist <- filelist[1, ]
        } else {
          filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
        }
      }
    }
    if (!"last" %in% Pkode_year) {
      filelist <- subset(filelist, filelist$pkodeaar %in% Pkode_year)
      if (!"both" %in% Pkode_month) {
        if ("last" %in% Pkode_month) {
          filelist <- filelist[1, ]
        } else {
          filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
        }
      }
    }
  }
  # Selection for uttrekk_dato
  if (!is.null(extracted_date)) {
    filelist <- subset(filelist, filelist$pkodeaar %in% Pkode_year)
    filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
    checkmate::assert_choice(as.Date(extracted_date), choices = filelist$uttrekk_dato)
    filelist <- subset(filelist, filelist$uttrekk_dato %in% as.Date(extracted_date))
  }

  return(filelist)
}
