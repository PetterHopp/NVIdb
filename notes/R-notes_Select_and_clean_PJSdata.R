# TUTORIAL: SELECT AND CLEAN PJS-DATA

## SET UP R-ENVIRONMENT ----
# Attach packages
library(NVIbatch)
use_pkg(pkg = c("RODBC", "compareDF"))
use_NVIverse(pkg = "NVIdb")

# Global variables

# Import support registers


## LOGIN TO PJS ----
# 1. Set username and password (credentials) for PJS. 
#    The credentials are saved in the users profile (at the current machine). 
#    Do once. Must be repeated when password to PJS changes or if you use another PC
set_credentials_PJS()

# 2. Login to PJS
journal_rapp <- login_by_credentials_PJS()


## READ DATA FROM PJS ----
# 1. Construct a selection-statement
#    Example: All saker related to one disease in 2021
#             ~ all saker with rabies-specific hensikt
#             + all saker with analytt rabies or lyssavirus for konklusjon or resultat
#             + all saker med metode specific for rabies


# Set selection parameters
purpose <- "Rabies"
aar <- c(2020)
hensikt2select <- c("0100101020", "0100102026", "0100103026", "0200144", "06035")
metode2select <- c("030010", "070017", "070180", "070245")
analytt2select <-  c("011402%","1502010241")    # Lyssavirus med underliggende og rabies


# Build select statement 
select_statement <- build_query_one_disease(year = aar,
                                            analytt = analytt2select, 
                                            hensikt = hensikt2select,
                                            metode = metode2select)


# Read data from v2_sak_m_res
PJSrawdata <- sqlQuery(journal_rapp,
                       query = select_statement["selection_v2_sak_m_res"],
                       as.is = TRUE,
                       stringsAsFactors = FALSE)


# Read data from sakskonklusjon
PJSsakskonklusjon <- sqlQuery(journal_rapp,
                              query = select_statement["selection_sakskonklusjon"],
                              as.is = TRUE,
                              stringsAsFactors = FALSE)


# Close connection to PJS
odbcClose(journal_rapp)


## STANDARDIZE DATA ----
# - The unnecessary columns konkl_provenr and vet_distriktnr are removed
# - The column names are standardized using standardize_columns
# - Numeric variables are transformed to numbers
# - Date variables are transformed to date format
# - Character variables are trimmed for leading and trailing spaces
# - The variables saksnr and, if possible, fagnr are generated
# - Test data, i.e. saker with ansvarlig_seksjon in c("14", "99") are deleted

# Example with only PJSrawdata as no data in PJSsakskonklusjon.
PJSdata <- standardize_PJSdata(PJSdata = PJSrawdata)

# Compare column names in new and old data sets
columns <- as.data.frame(cbind(colnames(PJSrawdata[which(!colnames(PJSrawdata) %in% c("konkl_provenr", "vet_distriktnr"))]),
                 colnames(PJSdata)))
colnames(columns) <- c("old", "new")
columns <- subset(columns, old != new)

# Exclude ring trials, quality assurance and samples from abroad
PJSdata <- exclude_from_PJSdata(PJSdata = PJSdata, 
                                abroad = "exclude", 
                                quality = "exclude")

## KEEP VARIABLES IN SAK, PROVE AND KONKLUSJON -LEVEL ----
PJSdata <- choose_PJS_levels(data = PJSdata,
                             levels = c("sak", "prove", "konklusjon"),
                             keep_col = NULL,
                             remove_col = c("konklnr", "konkl_type"),
                             unique_rows = TRUE)

## TRANSLATE CODES INTO DESCRIPTIVE TEXT  ----
### Import support registers
PJS_codes_2_text <- read_PJS_codes_2_text()

### Translate PJS-codes to code descriptions
PJSdata <- add_PJS_code_description(data = PJSdata,
                                    translation_table = PJS_codes_2_text,
                                    PJS_variable_type = c("hensikt", "art", "driftsform", "provetype",
                                                          "provemateriale", "kjonn", "kjennelse", "analytt"),
                                    code_colname = c("hensiktkode", "artkode", "driftsformkode", "provetypekode",
                                                     "provematerialekode", "kjonn", "konkl_kjennelsekode", "konkl_analyttkode"),
                                    new_column = c("hensikt", "art", "driftsform", "provetype",
                                                   "provemateriale", "kjonn2", "konkl_kjennelse", "konkl_analytt"),
                                    position = "right",
                                    overwrite = FALSE)





