

exclude_PJSdata_for_value <- function(data,
                                      code_column,
                                      value_2_check,
                                      missing_value, 
                                      invert = FALSE) {
  
  data <- PJSdata
  code_column <- "hensiktkode"
  value_2_check <- hensikt2delete
  missing_value = "include" # include, exclude, for_spesific_hensikt
  inverse = FALSE
  


# transform value_2_check to regular expressions
value_2_check <- paste0("^", value_2_check)
value_2_check <- gsub(pattern = "%", replacement = "[[:digit:]]*", x = value_2_check, fixed = TRUE)

# Identifies all variables in the index taking into consideration the PJS-levels of the code_column(s)
index <- c("aar", "ansvarlig_seksjon", "innsendelsenr", "saksnr")
for (k in 1:length(code_column)) {
  index <- union(index,
                 NVIdb::PJS_levels[which(NVIdb::PJS_levels[1:10, which(NVIdb::PJS_levels[which(NVIdb::PJS_levels$variable == code_column[k]), ] == 1)[1]] == 1), "variable"])
}
# Keeps only variables that exist in PJSdata. Necessary as resnr will not be in PJSdata.
index <- base::intersect(index, colnames(data))

# Generate data frame for check that only contains the relevant variables
ktr <- data[, unique(c(index, code_column))]
ktr <- unique(ktr)

# Combine the codes that should be checked into one variable
# if (code_column == "hensiktkode" & length(code_column) == 1) {
#   ktr$combined_codes <- ktr[, c(code_column)]
# } else {
#   ktr$combined_codes <- apply(ktr[, c("hensiktkode", code_column)], 1, FUN = paste, collapse = "-")
# }
if(length(code_column) > 1) {
  ktr$combined_codes <- apply(ktr[, c(code_column)], 1, FUN = paste, collapse = "-")
} else {
  ktr$combined_codes <- ktr[, code_column]
}


# Find records deviating from detected code values
ktr <- ktr %>%
  dplyr::rowwise() %>%
  dplyr::mutate(exclude = max(unlist(lapply(value_2_check, grep, x = combined_codes)), 0)) 

  if (missing_value == "exclude" & length(combined_codes == 1)) {
    ktr[which(is.na(ktr[, combined_codes])), "exclude"] <- 1
  }
  ktr$exclude <- as.logical(ktr$exclude)
  if (isTRUE(inverse)) {ktr$exclude <- !ktr$exclude}
  
  ktr <- subset(ktr, exclude == FALSE)
  ktr[, c("combined_codes", "exclude")] <- c(NULL, NULL)

  column_names <- colnames(data)
  data <- merge(x = ktr, y = data, by = c(index, code_column), all.x = TRUE, all.y = FALSE, sort = TRUE)
  data <- data[, column_names]
  
return(data)
}
