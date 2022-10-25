# generate_PJS_levels

# GENERATE rda-file WITH VARIABLES PER PJS-LEVEL ----

# DESIGN ----
# Set up R-environment
# Read Excel sheet with column standars
# Select data for OK-programmes
# Transform data
# Save data to ./data/OK_column_standards.rds

library(openxlsx)

# READS AND TRANSFORMS EXCEL SHEET WITH VARIABLES PER PJS-LEVEL ----
PJS_levels <- read.xlsx(xlsxFile = "./data-raw/PJS_levels.xlsx")
for (i in c(2:dim(PJS_levels)[2])) {
  PJS_levels[which(is.na(PJS_levels[, i])), i] <- 0
}

# SAVE IN PACKAGE DATA ----
usethis::use_data(name = PJS_levels, overwrite = TRUE, internal = FALSE)

# REMOVE DATA FROM ENVIONMENT TO AVOID CONFLICTS WHEN ATTACHING PACKAGE ----
rm(PJS_levels)
