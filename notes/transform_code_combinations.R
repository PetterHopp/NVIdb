

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
  checkmate::assert_list(from_values, min.len = 1, add = checks)
  checkmate::assert_list(to_values, min.len = 1, add = checks)
  checkmate::assert_character(from_columns, min.len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(to_columns, min.len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(impute_when_missing_from, max.len = length(to_columns), null.ok = TRUE, add = checks)
  checkmate::assert_subset(impute_when_missing_from, choices = from_columns, add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)
  
  # CREATE TRANSLATION TABLE WITH FROM AND TO VALUES ----
  to_columns_temp <- paste0(rep("tcc_V", length(to_columns)), as.character(1:length(to_columns)))
  translation_table <- data.frame(unlist(from_values[1]))
  colnames(translation_table) <- from_columns[1]
  for (i in 2:length(from_values)) {
    translation_table[, from_columns[i]] <- as.data.frame(unlist(from_values[i]))
  }
  for (i in 1:length(to_values)) {
    translation_table[, to_columns_temp[i]] <- as.data.frame(unlist(to_values[i]))
  }
  # translation_table[is.na(translation_table)] <- "_NA_"
  
  # CREATE SUBSET TO TRANSLATE ----
  subdata <- data[, from_columns]
  # subdata[is.na(subdata)] <- "_NA_"
  subdata$sort_order <- 1:nrow(subdata)
  
  # PERFORM TRANSLATION ----
  subdata <- merge(subdata, translation_table, by = c(from_columns), all.x = TRUE)
  
  if (!is.null(impute_when_missing_from)) {
    subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), to_columns_temp[1:length(impute_when_missing_from)]] <- 
      subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), impute_when_missing_from]
  }
  subdata <- subdata[order(subdata$sort_order), ]
  
  # RETURN DATA WITH TRANSLATED COLUMNS
  data[, to_columns] <- subdata[, to_columns_temp]
  return(data)
}

# # examples
# # two code values to one new varable
# from_values <- list(c("hjort", "rein", "rein", NA), 
#                     c("produksjonsdyr", "ville dyr", "produksjonsdyr", NA))
# to_values <- list(c("oppdrettshjort", "villrein", "tamrein", "Ukjent"))
# from_columns <- c("art", "driftsform")
# to_columns <- c("art2")
# impute_when_missing_from <- "art"
# 
# PJSdata <- as.data.frame(cbind(c("hjort", "rein", "rein", "elg", "hjort", "rein", "rein", NA),
#                                c("produksjonsdyr", "ville dyr", "produksjonsdyr", "ville dyr", "ville dyr", "produksjonsdyr", "ville dyr", NA)))
# colnames(PJSdata) <- c("art", "driftsform")
# data <- PJSdata
# 
# 
# #  A code combination of two is tranformed to another code combination of two
# from_values <- list(c("Detected"), 
#                     c("M. kansasii"))
# to_values <- list(c("Not detected"), 
#                   c("M. bovis"))
# from_columns <- c("kjennelse", "analytt")
# to_columns <- c("kjennelse", "analytt")
# impute_when_missing_from <- c("kjennelse", "analytt")
# 
# PJSdata <- as.data.frame(cbind(c("Detected", "Detected", "Not detected", NA), 
#                                c("M. bovis", "M. kansasii", "M. bovis", NA)))
# colnames(PJSdata) <- c("kjennelse", "analytt")
# data <- PJSdata
# 
