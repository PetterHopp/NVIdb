#' @title Manage translation of PJS-codes to descriptive text
#' @description Functions to adds a column with descriptive text for a column with PJS-codes in a dataframe with PJS-data.
#'    In addition there are functions to read and copy an updated version of the PJS code registers.
#' @details Export of data from PJS will produce dataframes in which many columns have coded data. These need to be translated
#'     into descriptive text to increase readability.
#'
#'     \code{add_PJS_code_description} can be used to translate the codes into descriptive text. In a data.frame with coded values,
#'     the function can return a data.frame with the descriptive text in a new column. The descriptive text is input in a new column
#'     to the right of the column with codes.
#'
#'     The function uses a premade translation table (PJS_codes_2_text.csv) that normally is updated every night from PJS.
#'
#'     Currently, the translation table has PJS-codes and the corresponding description for the following PJS variable types:
#' \itemize{
#'   \item hensikt
#'   \item utbrudd
#'   \item registertype (categories for locations and addresses)
#'   \item seksjon
#'   \item art (species and breed codes to species name)
#'   \item artrase (species and breed codes to species or breed name)
#'   \item driftsform
#'   \item provetype
#'   \item provemateriale
#'   \item kjonn
#'   \item forbehandling
#'   \item fysiologisk_stadium
#'   \item metode
#'   \item konkl_type
#'   \item kjennelse
#'   \item analytt.
#' }
#'
#'     \code{position =} is used to give the place if the new columns in the data.frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for \code{position = "left"} the new variables are placed to the left of the
#'     code_variable. If \code{position = "first"} or \code{position = "last"} the new columns are placed first or last, respectively, in the
#'     data.frame. A special case occurs for \code{position = "keep"} which only has meaning when the new column has the same name as an existing
#'     column and overwrite = TRUE. In these cases, the existing column will be overwritten with new data and have the same position.
#'
#'     \code{read_PJS_code_registers} reads the file "PJS_codes_2_text.csv" into a dataframe that can be used by \code{add_PJS_code_description}.
#'     In standard setting will the file read in the latest updated file from NVI's internal network. If changing the \code{from_path}, the function can
#'     be used to read the translation file from other directories. This can be useful if having a stand alone app with no connection the NVI's
#'     internal network. In other cases, it should be avoided.
#'
#'     PJS_codes_2_text.csv has the following columns: c("type", "kode", "navn", "utgatt_dato"), where "type" is the PJS variable type
#'     as listed above (for example hensikt), "kode" is the variable with the PJS-code, "navn" is the text describing the code, and "utgatt_dato"
#'     is the date for last date that the code was valid (NA if still valid). If translation tables are needed for other PJS variables, a dataframe
#'     with the same definition can be constructed to translate new variables.
#'
#'     \code{copy_PJS_code_registers} copies the file pjsCodeDescriptions.csv to a chosen directory.
#'
#'
#' @param data Dataframe with PJS-data with a column with codes for a PJS-variable
#' @param translation_table Dataframe with the code and the description for PJS-variables
#' @param PJS_variable_type A vector with PJS-variables, for example "hensikt". See details for a list of all PJS-variables included in
#'     the premade translation table pjscode_2_descriptions.csv. If more than one code should be translated, they can be given in the vector.
#' @param code_colname The name of the column with codes that should be translated. If several codes should be translated, a vector with the
#'     names of the coded variables should be given.
#' @param new_column The name of the new column with the text describing the code. If several codes should be translated, a vector with the
#'     new column names should be given.
#' @param position position for the new columns, can be one of c("first", "left", "right", "last", "keep"). If several codes should be translated,
#'     either one value to be applied for all may be given or a vector with specified position for each code to be translated should be given.
#' @param overwrite When the new column(s) already exist, the content in the existing column(s) is replaced by new data if overwrite = TRUE.
#'     If the new columns already exists and overwrite = FALSE, then an error is issued.
#' @param filename Filename of the source file for the translation table for PJS-codes
#' @param from_path Path for the source translation table for PJS-codes
#' @param to_path Path for the target translation table for PJS-codes when copying the table
#'
#' @return \code{add_PJS_code_description} A dataframe where the description text for the PJS-code has been added in the column to the
#'     right of the column with the code. If the input is a tibble, it will be transformed to a dataframe.
#'
#'     \code{read_PJS_codes_2_text} A dataframe with the translation table for PJS-codes as read from the source csv-file. If not changing standard
#'     input, the standard file at NVI's internal network is read.
#'
#'     \code{copy_PJS_codes_2_text} Copies the source translation table for PJS-codes to another location. If the target file already exists
#'     the source file is only copied if it is newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' PJS_codes_2_text <- read_PJS_codes_2_text()
#'
#' # Copy standard file from standard location to the subdirectory Data below the working directory
#' copy_PJS_codes_2_text(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' PJS_codes_2_text <- read_PJS_codes_2_text("PJS_codes_2_text.csv", "./Data/")
#'
#' # Translating artkode into art
#' newdata <- add_PJS_code_description(olddata, PJS_codes_2_text, "art", "artkode", "art")
#'
#' # Translating hensiktkode into Hensikt and konklusjonkode to Konklusjonskjennelse
#' newdata <- add_PJS_code_description(olddata,
#'                                     PJS_codes_2_text,
#'                                     PJS_variable_type = c("hensikt", "kjennelse"),
#'                                     code_colname = c("hensiktkode", "konklusjonkode"),
#'                                     new_column = c("Hensikt", "Konklusjonskjennelse"))
#' }
#'
add_PJS_code_description <- function(data,
                                     translation_table = PJS_codes_2_text,
                                     PJS_variable_type,
                                     code_colname,
                                     new_column,
                                     position = "right",
                                     overwrite = FALSE) {


  # ERROR check
  # error:
  if (length(intersect(code_colname, new_column)) > 0) {
    # issue error if names already exists
    stop(paste0("You cannot give the new column the same name as the code_colname '", code_colname, "' in the data frame '", deparse(substitute(data)), "`."))
  }

  # check_exist_colname(df_name = deparse(substitute(data)), df_columns = colnames(data), new_column = new_column, overwrite = overwrite)
  if (length(intersect(colnames(data), new_column)) > 0 & overwrite == FALSE) {
    # issue error if names already exists
    stop(paste(paste0("The column name(s): '", intersect(colnames(data), names(new_column)), "' already exist in '", deparse(substitute(data)), "`."),
               paste0("Either give new column name(s) for the column(s) called '", intersect(colnames(data), names(new_column)), "' or"),
               "Specify overwrite = TRUE to replace values in the existing column(s) with new content.", sep = "\n"))
  }


  # Transforms position to a vector with the same length as number of PJS-variables to be translated
  if (length(position) == 1 & length(PJS_variable_type) > 1) {position <- rep(position, length(PJS_variable_type))}

  # runs the translation for several PJS-variables at a time if wanted
  for (i in 1:length(PJS_variable_type)) {

    # Make a subset with only the codes that is relevant for the actual variabel
    code_2_description <- translation_table[base::which(translation_table$type == PJS_variable_type[i]), ]
    # code_2_description <- translation_table[base::which(translation_table$type == PJS_variable_type[i] & is.na(translation_table$utgatt_dato)), ]

    # # Changes the name of navn to text wanted in the df (txtvarname)
    # base::colnames(code_2_description)[base::which(base::colnames(code_2_description)=="navn")] <- new_column

    # Calls function that adds description to the right of the code
    data <- add_new_column(data,
                           ID_column = code_colname[i],
                           new_colname = new_column[i],
                           translation_tables = list(code_2_description),
                           ID_column_translation_table = c("kode"),
                           to_column_translation_table = c("navn"),
                           position = position[i],
                           overwrite = overwrite
    )

  }

  return(data)
}
