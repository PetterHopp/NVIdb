#' @title Generate vectors for fylke and fylkenr sorted in logical order for reporting
#' @details If the order of the fylker should be changed, one should edit the variable
#'     sortering_fylke in the source file.

# SET UP R-EINVIRONMENT ----
library(NVIdb)

# READ RAW DATA FOR FYLKE ----
fylke <- read.csv2(file.path(set_dir_NVI("GrunndataLand", slash = FALSE),
                             "FormaterteData", "Fylke_UTF8.csv"),
                   colClasses = c("fylkenr" = "character"))

# Ensure that the fylker are in correct order
fylke <- fylke[order(fylke$sortering_fylke), ]

# GENERATE ORDERED VECTORS ----
# for fylke
order_fylke <- unique(fylke[, "fylke"])
# order_fylke <- unique(fylke[which(is.na(fylke$utgatt_dato)), "fylke"])

# for fylkenr
order_fylkenr <- unique(fylke[, "fylkenr"])
# order_fylkenr <- unique(fylke[which(is.na(fylke$utgatt_dato)), "fylkenr"])

# SAVE IN PACKAGE DATA ----
# usethis::use_data(name = order_fylke_all, overwrite = TRUE, internal = FALSE)
usethis::use_data(name = order_fylke, overwrite = TRUE, internal = FALSE)
# usethis::use_data(name = order_fylkenr_all, overwrite = TRUE, internal = FALSE)
usethis::use_data(name = order_fylkenr, overwrite = TRUE, internal = FALSE)


# REMOVE DATA FROM ENVIONMENT TO AVOID CONFLICTS WHEN ATTACHING PACKAGE ----
rm(order_fylke, order_fylkenr)
