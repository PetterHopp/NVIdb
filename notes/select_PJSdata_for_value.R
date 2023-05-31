

select_PJSdata_for_value <- function(data,
                                     code_column,
                                     value_2_check,
                                     include_missing_for = NULL, 
                                     keep_selected = TRUE) {
  
  # data <- PJSdata
  # code_column <- "hensiktkode"
  # value_2_check <- hensikt2delete
  # include_missing_for = NULL
  # keep_selected = TRUE
  
  
  
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
    ktr[is.na(ktr$combined_codes), "combined_codes"] <- "NA"
  }
  
  
  # Find records deviating from detected code values
  ktr <- ktr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(select = max(unlist(lapply(value_2_check, grep, x = combined_codes)), 0)) 
  
  # if (!is.null(include_missing_for) & length(code_column == 1)) {
  #   ktr[which(is.na(ktr[, "combined_codes"])), "select"] <- 1
  # }
  
  ktr$select <- as.logical(ktr$select)
  if (isFALSE(keep_selected)) {ktr$select <- !ktr$select}
  
  ktr <- subset(ktr, select == TRUE)
  ktr[, c("combined_codes", "select")] <- c(NULL, NULL)
  
  column_names <- colnames(data)
  data <- merge(x = ktr, y = data, by = c(index, code_column), all.x = TRUE, all.y = FALSE, sort = TRUE)
  data <- data[, column_names]
  
  return(data)
}
