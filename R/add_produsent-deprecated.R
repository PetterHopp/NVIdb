#' @title Manage translation from prodnr8 into gjeldende_prodnr8
#' @description Function to add a column with gjeldende_prodnr8. In addition there are
#'    functions to read and copy the translation tables.
#' @details \code{add_produsentnr} can be used to translate the prodnr8 into gjeldende_prodnr8 and/or geo-koordinater.
#'
#'     \code{position =} is used to give the place if the new columns in the data.frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for \code{position = "left"} the new variables are placed to the left of the
#'     code_variable. If \code{position = "first"} or \code{position = "last"} the new columns are placed first or last, respectively, in the
#'     data.frame. A special case occurs for \code{position = "keep"} which only has meaning when the new column has the same name as an existing
#'     column and overwrite = TRUE. In these cases, the existing column will be overwritten with new data and have the same position.
#'
#'     \code{read_prodnr_2_current_prodnr} reads the file "Prodnr2GjeldendeProdnr.csv" into a data frame that can be used by
#'     other routines. Standard setting will the file read in the latest updated file from NVI's internal network. If changing
#'     the from_path, the function can be used to read the translation file from other directories. This can be useful if having a stand alone
#'     app with no connection the NVI's internal network. In other cases, it should be avoided.
#'
#'     \code{copy_prodnr_2_current_prodnr} copies the file "Prodnr2GjeldendeProdnr.csv" to a chosen directory.
#'
#'     \code{read_prodnr_2_coodinates} reads the file "Prodnr2Koordinater.csv" into a data frame that can be used to merge with data frames with
#'     prodnr8. Standard setting will the file read in the latest updated file from NVI's internal network. If changing the from_path, the function
#'     can be used to read the translation file from other directories. This can be useful if having a stand alone app with no connection the
#'     NVI's internal network. In other cases, it should be avoided.
#'
#' @param data Data frame with data with a column with a prodnr8
#' @param translation_table Data frame with the table for translating from prodnr8 to gjeldende_prodnr8.
#' @param code_column The column with the coded value. Valid values are one of c("prodnr8"). If the column in
#'     data has another name, it can be input as a named vector, see examples.
#' @param new_column The new columns that should be included into the dataframe. The new columns can be up to
#'     c("gjeldende_prodnr8") depending on \code{code_column}. If the new columns in the result dataframe
#'     should have other names, \code{new_column} can be input as a named vector, see examples.
#' @param position position for the new columns, can be one of c("first", "left", "right", "last", "keep")
#' @param overwrite When the new column(s) already exist, the content in the existing column(s) is replaced by new data if overwrite = TRUE.
#'     If the new columns already exists and overwrite = FALSE, then an error is issued.
#' @param filename a list with the filenames of the source files with the tables for generating the translation table.
#' @param from_path Path for the source files for the translation table.
#' @param to_path Path for the target translation table when copying the translation table.
#'
#' @return \code{add_produsentnr} A data frame where the gjeldende_prodnr8. has been added in the column to the
#'     right of the column with the prodnr8.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @name add_produsent-deprecated
#' @usage add_produsent(data,
#'                      translation_table,
#'                      code_column,
#'                      new_column,
#'                      position,
#'                      overwrite)
#' @keywords internal
#' @examples
#' \dontrun{
#' #CURRENT PRODNR8
#' # Reading from standard directory at NVI's network
#' prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr()
#'
#' # Copy standard file from standard location to the subdirectory Data below the working directory
#' copy_prodnr_2_current_prodnr(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' prodnr_2_gjeldende_prodnr <- read_prodnr_2_current_prodnr(from_path = "./Data/")
#'
#' prodnr8 <- c("09140087", "14260856", "17020818", "50060129")
#' olddata <- as.data.frame(prodnr8)
#'
#' # Add new column with current prodnr8
#' newdata <- add_produsent(olddata,
#'                         translation_table = prodnr_2_gjeldende_prodnr,
#'                         code_column = "prodnr8",
#'                         new_column = "gjeldende_prodnr8",
#'                         position = "left")
#'
#' # COORDINATES
#' # Reading from standard directory at NVI's network
#' prodnr_2_koordinater <- read_prodnr_2_coordinates()
#'
#' newdata <- add_produsent(newdata,
#'                         translation_table = prodnr_2_koordinater,
#'                         code_column = "prodnr8",
#'                         new_column = c("longitude" = "geo_eu89_o", "latitude" = "geo_eu89_n")
#'
#' }
#'
NULL


#' @title add_produsent is Deprecated
#' @description add_produsent was deprecated 2022-05-02. as other properties 
#'     than 'gjeldende_prodnr8' could not be included without breaking backward 
#'     compatibility. Use \code{add_produsent_properties} instead and ensure 
#'     to set the parameter \code{impute_old_when_missing = TRUE} when translating 
#'     from "prodnr8" to "gjeldende_prodnr8" and set the parameter 
#'     \code{impute_old_when_missing = FALSE} when translating from "prodnr8" to 
#'     other properties.
#' @details The old help pages can be found at \code{help("add_produsent-deprecated")}. 
#'     Information on deprecated function can be found at \code{help("NVIdb-deprecated")}.
#' @param data Data frame with data with a column with a prodnr8
#' @param translation_table Data frame with the table for translating from prodnr8 to gjeldende_prodnr8.
#' @param code_column The column with the coded value. Valid values are one of c("prodnr8"). If the column in
#'     data has another name, it can be input as a named vector, see examples.
#' @param new_column The new columns that should be included into the dataframe. The new columns can be up to
#'     c("gjeldende_prodnr8") depending on \code{code_column}. If the new columns in the result dataframe
#'     should have other names, \code{new_column} can be input as a named vector, see examples.
#' @param position position for the new columns, can be one of c("first", "left", "right", "last", "keep")
#' @param overwrite When the new column(s) already exist, the content in the existing column(s) is replaced by new data if overwrite = TRUE.
#'     If the new columns already exists and overwrite = FALSE, then an error is issued.
#' @export
#' @keywords internal
#'
add_produsent <- function(data,
                          translation_table,
                          code_column,
                          new_column,
                          position = "right",
                          overwrite = FALSE) {
  
  .Deprecated(new = "add_produsent_properties",
              package = "NVIdb",
              msg = paste("'add_produsent' is replaced by 'add_produsent_properties' to achieve",
                          "more flexibility and correct errors for other properties than 'gjeldende_prodnr8.",
                          "Remember to set the input parameter 'impute_old_when_missing' when using",
                          "'add_produsent_properties'."))
  
  data <- add_produsent_properties(data = data,
                                  translation_table = translation_table,
                                  code_column = code_column,
                                  new_column = new_column,
                                  position = position,
                                  overwrite = overwrite,
                                  impute_old_when_missing = TRUE)
  
  return(data)
}
