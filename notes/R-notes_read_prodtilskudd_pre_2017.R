#  TELLE ANTALL GEITEBESETNINGER FORDELT PÅ KATEGORI 2006-2016

# Benytter data fra produksjonstilskudd.
# Kategorisering mellom ammegeit og melkegeit er kun mulig for telledato i mars fordi det mangler data for ammegeit ved telledata i oktober

library(NVIdb)
library(dplyr)
library(tidyr)
library(haven)
# library(sas7bdat)
# library(poorman)
library(openxlsx)

Pkode_year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
# Pkode_year <- c("2006")
Pkode_month <- "both"

filelist <- as.data.frame(list.files(path = paste0(NVIdb:::path_Prodtilskudd,"FormaterteData/SASdata/"),
                                     pattern = "sas7bdat",
                                     ignore.case = TRUE,
                                     include.dirs = FALSE),
                          stringsAsFactors = FALSE)

colnames(filelist) <- "filename"
filelist$from_path <- paste0(NVIdb:::path_Prodtilskudd,"FormaterteData/SASdata/")
filelist$fileinfo <- sub(".sas7bdat", "", filelist$filename)
filelist$data <- NA
filelist[which(substr(filelist$fileinfo,1,3) == "adr"), "data"] <- "adr"
filelist[which(substr(filelist$fileinfo,1,3) == "pko"), "data"] <- "pkode"
filelist <- subset(filelist, nchar(fileinfo) == (nchar(data) + 5) )
filelist$fileinfo <- sub("adr", "", filelist$fileinfo)
filelist$fileinfo <- sub("pkode", "", filelist$fileinfo)
filelist$pkodeaar <- substr(filelist$fileinfo, 1, 4)
filelist$pkodemonth <- "07"
filelist[which(endsWith(filelist$fileinfo, "b")), "pkodemonth"] <- "12"
filelist$telledato <- paste0(filelist$pkodeaar, "-", filelist$pkodemonth, "-31")
filelist$telledato <- as.Date(filelist$telledato)
# filelist$pkodeaar <- as.numeric(filelist$pkodeaar)
# Telledata endret til 1.1.2002 for andre søkeomgang i 2001
filelist[which(filelist$pkodemonth == "12" & filelist$pkodeaar > "2000"), "telledato"] <- filelist[which(filelist$pkodemonth == "12" & filelist$pkodeaar > "2000"), "telledato"] + 1

filelist <- filelist[order(filelist$pkodeaar, filelist$pkodemonth, filelist$data, decreasing = TRUE),]

if (!"last" %in% Pkode_year) {
  filelist <- subset(filelist, filelist$pkodeaar %in% Pkode_year)
  if (Pkode_month != "both") {
    filelist <- subset(filelist, filelist$pkodemonth %in% Pkode_month)
  }
}


if (exists("resdf")) {rm(resdf)}
# Read data for the selected year and months from Pkoderegisteret and combine into one dataframe
for (i in 1:floor(dim(filelist)[1] / 2)) {

  # read single files
  tpkode <- haven::read_sas(data_file = paste0(filelist[i * 2 - 1, "from_path"], filelist[i * 2 - 1, "filename"]))
  colnames(tpkode) <- gsub("PKODE", "P", colnames(tpkode), ignore.case = TRUE)
  tadr   <- haven::read_sas(data_file = paste0(filelist[i * 2, "from_path"], filelist[i * 2, "filename"]))
  tadr[, c("periode")] <- NULL
  tempdf <- merge(tadr, tpkode, by = c("gardidnr", "lopenr", "komnr", "prodnr"))
  tempdf <- tempdf %>%
    poorman::rename(`søknadsår` = gardidnr, telledato = lopenr) %>%
    poorman::mutate(`søknadsår` = filelist[i * 2 - 1, "pkodeaar"]) %>%
    poorman::mutate(telledato = filelist[i * 2 - 1, "telledato"])

  tempdf$`søknadsår` <- filelist[i * 2 - 1, "pkodeaar"]
  tempdf$telledato <- filelist[i * 2 - 1, "telledato"]
  tempdf[, c("periode")] <- NULL

  if (exists("resdf")) {
    resdf[setdiff(names(tempdf), names(resdf))] <- NA
    tempdf[setdiff(names(resdf), names(tempdf))] <- NA
    resdf <- rbind(resdf, tempdf)
  } else {
    resdf <- tempdf
  }
}

# Standardize column names



Prodtype <- as.data.frame(rbind(c(0,0,1,"Annet"),
                                c(1,0,0,"Melkebesetning"),
                                c(0,1,0,"Ammegeitbesetning"),
                                c(1,0,1,"Melkebesetning"),
                                c(0,1,1,"Ammegeitbesetning"),
                                c(1,1,0,"Kombinert besetning"),
                                c(1,1,1,"Kombinert besetning")),
                          stringsAsFactors = FALSE)
colnames(Prodtype) <- c("P140","P142","P144","GeitProdtype")
Prodtype[, "P140"] <- as.numeric(Prodtype[, "P140"])
Prodtype[, "P142"] <- as.numeric(Prodtype[, "P142"])
Prodtype[, "P144"] <- as.numeric(Prodtype[, "P144"])

