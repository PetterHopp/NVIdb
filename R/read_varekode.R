
read_varekode <- function(from_path = paste0(set_dir_NVI("LevReg")), period, data_source = "formatted") {
  
  # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING ----
  from_path <- sub("/+$|\\\\+$", "", from_path)
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  checkmate::assert_subset(data_source, 
                           choices = c("formatted", "raw"), 
                           add = checks)
  checkmate::assert(checkmate::check_integerish(as.numeric(period[which(!grepl("[:alpha:]", period))]), 
                                                lower = 1995, 
                                                upper = as.numeric(format(Sys.Date(),"%Y")), 
                                                any.missing = FALSE, 
                                                unique = TRUE), 
                    checkmate::check_choice(period, choices = c("last")), 
                    add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # IMPORT VAREKODEREGISTER FORMATTED DATA ----
  if (data_source == "formatted") {
    
    from_path <- file.path(from_path, "FormaterteData")
    
    # READ DATA ----
    df1 <- read_csv_file(filename = "varekode.csv",
                         from_path = from_path,
                         options = list(colClasses = "character",
                                        fileEncoding = "UTF-8"))
  }
  
  # IMPORT VAREKODEREGISTER RAW DATA ----
  if (data_source == "raw") {
    sub_path <- "RaData"
    # Generate data frame listing all versions of the varekoderegister
    filnavn <- list.files(path = file.path(from_path, sub_path), recursive = TRUE, pattern = "varekode", ignore.case = TRUE)
    filnavn <- as.data.frame(filnavn)
    # Find start and end date that the register is valid for
    filnavn$fra_dato <- as.Date(paste0(substr(filnavn$filnavn, 1, 6), "01"), format = "%Y%m%d")
    filnavn$til_dato <- as.Date(cut(as.Date(paste0(substr(filnavn$filnavn, 8, 13), "01"), format = "%Y%m%d") +32, "month")) - 1
    filnavn$n_days <- as.numeric(filnavn$til_dato - filnavn$fra_dato + 1)
    filnavn$aar <- as.numeric(format(filnavn$til_dato, "%Y"))
    # Order in decending order by date
    filnavn <- arrange(filnavn, desc(fra_dato), desc(til_dato))
    
    # filnavn <- filnavn[c(2:dim(filnavn)[1]), ]
    # Select varekoder for time period
    # Selects from year
    if (period != "last") {
      rownr <- which(filnavn[, "n_days"] >= 365 & filnavn[, "aar"] == period)
    }
    # Selects last 12 months
    if (period == "last") {
      if (filnavn[1, "n_days"] >= 365) {rownr <- 1}
      if (filnavn[1, "n_days"] < 365) {rownr <- c(1, 2)}
    }
    
    # # Select varekode raw data for import
    if(exists("df1")) {rm(df1)}
    for(i in rownr) {
      # i <- 1
      check_header <- readLines(con = paste0(set_dir_NVI("LevReg"), "RaData/", filnavn[i, "filnavn"]), n = 1)
      # "vare" in first line, then header = TRUE
      header <- grepl("vare", check_header, ignore.case = TRUE)
      # identify delimiter
      if (grepl(";", check_header))  {delimiter <- ";"}
      if (grepl(",", check_header))  {delimiter <- ","}
      
      tempdf <- read.delim(paste0(set_dir_NVI("LevReg"), sub_path, "/", filnavn[i, "filnavn"]),
                           header = header,
                           sep = delimiter)
      
      # if no national characters (represented by "å"), then read again using UTF-8 encoding
      if(isFALSE(any(grepl("\u00E5", tempdf[, 2], ignore.case = TRUE)))) {
        tempdf <- read.delim(paste0(set_dir_NVI("LevReg"), sub_path, "/", filnavn[i, "filnavn"]),
                             header = header,
                             sep = delimiter,
                             fileEncoding = "UTF-8")
      }
      colnames(tempdf) <- c("varekode", "beskrivelse", "dyreslag")
      if(exists("df1")) {
        df1 <- rbind(df1, tempdf)
      } else {
        df1 <- tempdf
      }
      df1 <- unique(df1)
    }
  }
  
  return(df1)
}
