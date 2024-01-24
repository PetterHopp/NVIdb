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
#' The `from_values` is a list of one or more vectors. Each vector is named with
#'     the column name and represents one column variable with code values. The
#'     first entry in each vector constitute one code combination to be
#'     transformed, the second entry constitutes the next code combinations.
#'
#' Likewise, is the `to_values` a list of one or more named vectors. Each
#'     vector is named and represents one column variable with
#'     code values to which the code combinations in the `from_values` should be
#'     transformed. The name of the vector is the name of the columns with the
#'     transformed values. The transformed values can be put in the original columns,
#'     in which case the transformed combinations will replace
#'     the original entries. If the transformed column names don't exist in data,
#'     the columns will be added to the data.
#'
#' If the codes are not transformed, these can be kept in the data.
#'     `impute_when_missing_from` gives the column names of the columns from which
#'     to impute. Normally this will be the same as the original columns. However,
#'     if the number of transformed columns is less than the original columns, it
#'     will be necessary to give the columns from which to keep the code.
#'
#' @param data \[\code{data.frame}\]\cr
#' Data with code values that should be transformed.
#' @param from_values \[\code{list}\]\cr
#' List with named vector(s) of code values that should transformed, see details and examples.
#' @param to_values \[\code{list}\]\cr
#' List with named vector(s) of code values that should be the results of the transformation,
#'     see details and examples.
#' @param impute_when_missing_from \[\code{character}\]\cr
#' Column names for the code variables from which code values should be copied if no
#'     transformation is performed. Defaults to the original column names.
#'
#' @return A `data.frame`.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @md
#' @export
#' @examples
#' library(NVIdb)
#'
#' #  A code combination of two is tranformed to another code combination of two
#'   data <- as.data.frame(cbind(
#' c("Detected", "Detected", "Not detected", NA),
#'                               c("M. bovis", "M. kansasii", "M. bovis", NA)
#' ))
#'   colnames(data) <- c("kjennelse", "analytt")
#'
#'   data <- transform_code_combinations(data = data,
#'                                       from_values = list("kjennelse" = c("Detected"),
#'                                                          "analytt" = c("M. kansasii")),
#'                                       to_values = list("kjennelse" = c("Not detected"),
#'                                                        "analytt" = c("M. bovis")),
#'                                       impute_when_missing_from = c("kjennelse", "analytt"))
#'
#' # two code values to one new variable
#' data <- as.data.frame(cbind(c("hjort", "rein", "elg", "hjort", NA),
#'                             c("produksjonsdyr", "ville dyr", "ville dyr", "ville dyr", NA)))
#' colnames(data) <- c("art", "driftsform")
#'
#' data <- transform_code_combinations(
#'              data = data,
#'              from_values = list("art" = c("hjort", "rein", NA),
#'                                 "driftsform" = c("produksjonsdyr", "ville dyr", NA)),
#'              to_values = list("art2" = c("oppdrettshjort", "villrein", "ukjent")),
#'              impute_when_missing_from = "art")
transform_code_combinations <- function(data,
                                        from_values,
                                        to_values,
                                        impute_when_missing_from = NULL) {
  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_list(from_values, min.len = 1, add = checks)
  checkmate::assert_list(to_values, min.len = 1, add = checks)
  # checkmate::assert_character(from_columns, min.len = 1, min.chars = 1, add = checks)
  # checkmate::assert_character(to_columns, min.len = 1, min.chars = 1, add = checks)
  checkmate::assert_character(impute_when_missing_from, max.len = length(to_values), null.ok = TRUE, add = checks)
  checkmate::assert_subset(impute_when_missing_from, choices = names(from_values), add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # CREATE TRANSLATION TABLE WITH FROM AND TO VALUES ----
  to_columns_temp <- paste0(rep("tcc_V", length(to_values)), as.character(1:length(to_values)))
  translation_table <- data.frame(to_values)
  colnames(translation_table) <- to_columns_temp
  translation_table <- cbind(data.frame(from_values), translation_table)

  # CREATE SUBSET TO TRANSLATE ----
  subdata <- as.data.frame(data[, names(from_values)])
  colnames(subdata) <- names(from_values)
  # subdata[is.na(subdata)] <- "_NA_"
  subdata$sort_order <- 1:nrow(subdata)

  # PERFORM TRANSLATION ----
  subdata <- merge(subdata, translation_table, by = names(from_values), all.x = TRUE)

  if (!is.null(impute_when_missing_from)) {
    if (length(to_columns_temp) == 1) {
      subdata[is.na(subdata[, to_columns_temp]), to_columns_temp] <-
        subdata[is.na(subdata[, to_columns_temp]), impute_when_missing_from]
    } else {
      subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), to_columns_temp[1:length(impute_when_missing_from)]] <-
        subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), impute_when_missing_from]
    }
  }
  subdata <- subdata[order(subdata$sort_order), ]

  # RETURN DATA WITH TRANSLATED COLUMNS
  data[, names(to_values)] <- subdata[, to_columns_temp]
  return(data)
}

#
#
# transform_code_combinations <- function(data,
#                                         from_values,
#                                         to_values,
#                                         from_columns,
#                                         to_columns,
#                                         impute_when_missing_from = NULL) {
#   # ARGUMENT CHECKING ----
#   # Object to store check-results
#   checks <- checkmate::makeAssertCollection()
#   # Perform checks
#   checkmate::assert_data_frame(data, add = checks)
#   checkmate::assert_list(from_values, min.len = 1, add = checks)
#   checkmate::assert_list(to_values, min.len = 1, add = checks)
#   checkmate::assert_character(from_columns, min.len = 1, min.chars = 1, add = checks)
#   checkmate::assert_character(to_columns, min.len = 1, min.chars = 1, add = checks)
#   checkmate::assert_character(impute_when_missing_from, max.len = length(to_columns), null.ok = TRUE, add = checks)
#   checkmate::assert_subset(impute_when_missing_from, choices = from_columns, add = checks)
#   # Report check-results
#   checkmate::reportAssertions(checks)
#
#   # CREATE TRANSLATION TABLE WITH FROM AND TO VALUES ----
#   to_columns_temp <- paste0(rep("tcc_V", length(to_columns)), as.character(1:length(to_columns)))
#   translation_table <- data.frame(unlist(from_values[1]))
#   colnames(translation_table) <- from_columns[1]
#   if (length(from_columns) > 1) {
#     for (i in 2:length(from_values)) {
#       translation_table[, from_columns[i]] <- as.data.frame(unlist(from_values[i]))
#     }
#   }
#   for (i in 1:length(to_values)) {
#     translation_table[, to_columns_temp[i]] <- as.data.frame(unlist(to_values[i]))
#   }
#   # translation_table[is.na(translation_table)] <- "_NA_"
#
#   # CREATE SUBSET TO TRANSLATE ----
#   subdata <- as.data.frame(data[, from_columns])
#   colnames(subdata) <- from_columns
#   # subdata[is.na(subdata)] <- "_NA_"
#   subdata$sort_order <- 1:nrow(subdata)
#
#   # PERFORM TRANSLATION ----
#   subdata <- merge(subdata, translation_table, by = c(from_columns), all.x = TRUE)
#
#   if (!is.null(impute_when_missing_from)) {
#     if (length(to_columns_temp) == 1) {
#       subdata[is.na(subdata[, to_columns_temp]), to_columns_temp] <-
#         subdata[is.na(subdata[, to_columns_temp]), impute_when_missing_from]
#     } else {
#       subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), to_columns_temp[1:length(impute_when_missing_from)]] <-
#         subdata[rowSums(is.na(subdata[, to_columns_temp])) == length(to_columns_temp), impute_when_missing_from]
#     }
#   }
#   subdata <- subdata[order(subdata$sort_order), ]
#
#   # RETURN DATA WITH TRANSLATED COLUMNS
#   data[, to_columns] <- subdata[, to_columns_temp]
#   return(data)
# }
#
