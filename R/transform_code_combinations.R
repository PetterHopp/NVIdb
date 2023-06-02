#' @title Transform combinations of code values into new values 
#' @description Transforms combinations of code values into new values in a data
#'     frame. This is intended for use when only a few code value combinations
#'     should be changed and one will avoid building translation tables or code
#'     with several if, which or case_when statements. In particularly it was 
#'     inspired by the need of changing a few code combinations in PJS data when 
#'     reporting surveillance programmes. 
#' @details The function builds a transformation table based on the input. The 
#'     `from_values` and the `to_values` give the data to a transformation table,
#'     and the `from_columns` and the `to_columns` give the column names for the 
#'     transformation table. 
#'
#' The `from_values` is a list of one or more vectors. Each vector represents one 
#'     column variable with code values. The first  entry in each vector  
#'     constitute one code combination to be transformed, the second entry 
#'     constitutes the next code combinations. Likewise, is the `to_values` a list 
#'     of one or more vectors. Each vector represents one column variable with 
#'     code values to which the code combinations in the `from_values` should be 
#'     transformed. 
#'
#' The `from_columns` is a vector of column names for the codes that should be
#'     transformed. The column names will name each vector with code values in
#'     the `from_values` list. The column names must exist in the data. Likewise,
#'     the `to_columns` is a vector of column names for the columns with the
#'     transformed code values. The `to_columns` can be the same as the 
#'     from_columns, in which case the transformed combinations will replace
#'     the original entries. If the `to_columns` don't exist in data, the column
#'     will be added to the data. 
#'     
#' If the codes are not transformed, these will be kept in the data.
#'     `impute_when_missing_from` gives the column names of the columns from which
#'     to impute. Normally this will be the same as the `from_columns.` However,
#'     if the number of columns in `to_columns` is less than in from_columns, it
#'     will be necessary to give the columns from which to keep the code.
#' 
#' @param data \[\code{data.frame}\]\cr
#' Data with code values that should be transformed. 
#' @param from_values \[\code{list}\]\cr
#' List with vector(s) of code values that should transformed, see details and examples.
#' @param to_values \[\code{list}\]\cr
#' List with vector(s) of code values that should be the results of the transformation, 
#'     see details and examples.
#' @param from_columns \[\code{character}\]\cr
#' Column names for the code variables that should be transformed. 
#' @param to_columns \[\code{character}\]\cr
#' Column names for the code variables with the results of the transformation.
#' @param impute_when_missing_from \[\code{character}\]\cr
#' Column names for the code variables from which code values should be copied if no 
#'     transformation is performed. Defaults to `from_columns`.
#'
#' @return A `data.frame`.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @md
#' @export


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
