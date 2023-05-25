

transform_code_combinations <- function(data, 
                                        from_values, 
                                        to_values, 
                                        from_columns, 
                                        to_columns, 
                                        impute_when_missing_from = NULL) {
  
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_list(from_values, min_len = 1, add = checks)
  checkmate::assert_list(to_values, min_len = 1, add = checks)
  checkmate::assert_character(from_columns, min_len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(to_columns, min_len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(impute_when_missing_from, max_len = length(to_columns), null.ok = TRUE, add = checks)
  checkmate::assert_subset(impute_when_missing_from, choices = from_columns, max.len = length(to_columns), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # CREATE TRANSLATION TABLE WITH FROM AND TO VALUES ----
translation_table <- data.frame(unlist(from_values[1]))
colnames(translation_table) <- from_columns[1]
for (i in 2:length(from_values)) {
  translation_table[, from_columns[i]] <- as.data.frame(unlist(from_values[i]))
}
for (i in 1:length(to_values)) {
  translation_table[, to_columns[i]] <- as.data.frame(unlist(to_values[i]))
}
# translation_table[is.na(translation_table)] <- "_NA_"

# CREATE SUBSET TO TRANSLATE ----
subdata <- data[, from_columns]
# subdata[is.na(subdata)] <- "_NA_"
subdata$sort_order <- 1:nrow(subdata)

# PERFORM TRANSLATION ----
subdata <- merge(subdata, translation_table, by = c("art", "driftsform"), all.x = TRUE)

if (!is.null(impute_when_missing_from)) {
  subdata[which(is.na(subdata[, to_columns])), to_columns[1:length(impute_when_missing_from)]] <- 
    subdata[which(is.na(subdata[, to_columns])), impute_when_missing_from]
}
subdata <- subdata[order(subdata$sort_order), ]

# RETURN DATA WITH TRANSLATED COLUMNS
data <- cbind(data, as.data.frame(subdata[, to_columns])) #, names = to_columns))
if (length(to_columns) == 1) {
colnames(data)[ncol(data)] <- to_columns
}
return(data)
}

# examples
from_values <- list(c("hjort", "rein", "rein", NA), 
                    c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA))
to_values <- list(c("oppdrettshjort", "villrein", "tamrein", "Ukjent"))
from_columns <- c("art", "driftsform")
to_columns <- c("art2")
impute_from_when_missing <- "art"

PJSdata <- as.data.frame(cbind(c("hjort", "rein", "rein", "elg", "hjort", "rein", "rein", NA),
                               c("produksjonsdyr", "ville dyr", "produksjonsdyr", "ville dyr", "ville dyr", "produksjonsdyr", "ville dyr", NA)))
colnames(PJSdata) <- c("art", "driftsform")
data <- PJSdata

