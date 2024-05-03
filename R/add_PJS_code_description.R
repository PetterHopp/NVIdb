#' @title Manage translation of PJS codes to descriptive text
#' @description Functions to adds a column with descriptive text for a column
#'    with PJS codes in a data frame with PJS data. You may also use backwards
#'    translation from descriptive text to PJS code. In addition there are
#'    functions to read and copy an updated version of the PJS code registers.
#' @details Export of data from PJS will produce data frames in which many columns
#'     have coded data. These need to be translated into descriptive text to
#'     increase readability.
#'
#'     \code{add_PJS_code_description} can be used to translate the codes into
#'     descriptive text. In a data frame with coded values, the function can
#'     return a data frame with the descriptive text in a new column. As default,
#'     the descriptive text is input in a new column to the right of the column
#'     with codes.
#'
#'     \code{add_PJS_code_description} uses the pre made translation table
#'     "PJS_codes_2_text.csv". The data need to be loaded by
#'     \code{read_PJS_codes_2_text} before running \code{add_PJS_code_description},
#'     see example. The file "PJS_codes_2_text.csv" is normally updated every night
#'     from PJS.
#'
#'     Currently, the translation table has PJS codes and the corresponding
#'     description for the PJS variable types given in the first column in the table
#'     below. The standardized PJS column name is given in the column "code colname" for
#'     which the "PJS variable type" will translate into descriptive text. The standard
#'     new column name is given in the column "new column".
#'
#' \tabular{llll}{
#'   \strong{PJS variable type} \tab \strong{code colname} \tab \strong{new column} \tab \strong{remark} \cr
#'   seksjon \tab ansvarlig_seksjon \tab ansvarlig_seksjon_navn \tab \cr
#'   seksjon \tab utf_seksjon \tab utforende_seksjon_navn \tab \cr
#'   hensikt \tab hensiktkode \tab hensikt \tab \cr
#'   utbrudd \tab utbruddnr \tab utbrudd \tab translates NVI's outbreak number \cr
#'   registertype \tab rekvirenttype \tab rekvirenttype_navn \tab categories of locations and addresses \cr
#'   registertype \tab eier_lokalitettype \tab eier_lokalitettype_navn \tab categories of locations and addresses \cr
#'   registertype \tab annen_aktortype \tab annen_aktortype_navn \tab categories of locations and addresses \cr
#'   art \tab artkode \tab art \tab species and breed codes to species name \cr
#'   artrase \tab artkode \tab art \tab species and breed codes to species or breed name \cr
#'   fysiologisk_stadium \tab fysiologisk_stadiumkode \tab fysiologisk_stadium \tab \cr
#'   kjonn \tab kjonn \tab kjonn_navn \tab \cr
#'   driftsform \tab driftsformkode \tab driftsform \tab \cr
#'   oppstalling \tab oppstallingkode \tab oppstalling \tab \cr
#'   provetype \tab provetypekode \tab provetype \tab \cr
#'   provemateriale \tab provematerialekode \tab provemateriale \tab \cr
#'   forbehandling \tab forbehandlingkode \tab forbehandling \tab \cr
#'   metode \tab metodekode \tab metode \tab \cr
#'   metode \tab subund_metodekode \tab submetode \tab \cr
#'   konkl_type \tab konkl_typekode \tab konkl_type \tab \cr
#'   kjennelse \tab sakskonkl_kjennelsekode \tab sakskonkl_kjennelse \tab \cr
#'   kjennelse \tab konkl_kjennelsekode \tab konkl_kjennelse \tab \cr
#'   kjennelse \tab res_kjennelsekode \tab res_kjennelse \tab \cr
#'   kjennelse \tab subres_kjennelsekode \tab subres_kjennelse \tab \cr
#'   analytt \tab sakskonkl_analyttkode \tab sakskonkl_analytt \tab \cr
#'   analytt \tab konkl_analyttkode \tab konkl_analytt \tab \cr
#'   analytt \tab res_analyttkode \tab res_analytt \tab \cr
#'   analytt \tab subres_analyttkode \tab subres_analytt \tab \cr
#'   enhet \tab enhetkode \tab enhet \tab \cr
#'   enhet \tab subres_enhetkode \tab subres_enhet \tab \cr
#' }
#'
#'     If \code{code_colname} is a vector of standardized PJS column names
#'     and a subset of "code column" in the table above, you may facilitate
#'     coding by setting \code{PJS_variable_type = "auto"} and/or
#'     \code{new_colname = "auto"}. Then the \code{PJS_variable_type} will be
#'     automatically set according to the table above (for "artkode"
#'     \code{PJS_variable_type = "art"} will be chosen). Likewise, the
#'     \code{new_column} will be automatically set according to the table above.
#'
#'     \code{position} is used to give the position if the new columns in the
#'     data frame. For \code{position = "right"} the new variables are placed
#'     to the right of the code_variable. Likewise, for \code{position = "left"}
#'     the new variables are placed to the left of the code_variable. If
#'     \code{position = "first"} or \code{position = "last"} the new columns are
#'     placed first or last, respectively, in the data frame. A special case
#'     occurs for \code{position = "keep"} which only has meaning when the new
#'     column has the same name as an existing column and overwrite = TRUE. In
#'     these cases, the existing column will be overwritten with new data and
#'     have the same position.
#'
#'     \code{backward = TRUE} can be used to translate from descriptive text and
#'     back to PJS codes. This intended for cases where the PJS code has been lost
#'     (for example in EOS data) or when data from other sources should be translated
#'     to codes to be able to use the code hierarchy for further processing of the
#'     data. Back translation ignores case. Be aware that the back translation is
#'     most useful for short descriptive text strings, as longer strings may have been
#'     shortened and the risk of misspelling and encoding problems is larger. For some
#'     descriptive text strings, there are no unique translation. In these cases,
#'     the code value is left empty.
#'
#'     \code{read_PJS_codes_2_text} reads the file "PJS_codes_2_text.csv" into a
#'     data frame that can be used by \code{add_PJS_code_description}. In standard
#'     setting will the file read in the latest updated file from NVI's internal
#'     network. If changing the \code{from_path}, the function can be used to read
#'     the translation file from other directories. This can be useful if having a
#'     stand alone app with no connection the NVI's internal network. In other cases,
#'     it should be avoided.
#'
#'     "PJS_codes_2_text.csv" has the following columns: c("type", "kode", "navn",
#'     "utgatt_dato"), where "type" is the PJS variable type as listed above (for
#'     example hensikt), "kode" is the variable with the PJS code, "navn" is the text
#'     describing the code, and "utgatt_dato" is the date for last date that the
#'     code was valid (\code{NA} if still valid). If translation tables are needed for
#'     other PJS variables, a data frame with the same column definition can be
#'     constructed to translate new variables.
#'
#'     \code{copy_PJS_codes_2_text} copies the file "pjsCodeDescriptions.csv" to
#'     a given directory.
#'
#' @param data [\code{data.frame}] \cr
#'     PJS data with at least one column that have codes for a PJS variable.
#' @param translation_table [\code{data.frame}] \cr
#'     Table with the code and the description for PJS variables. Defaults to
#'     "PJS_codes_2_text".
#' @param PJS_variable_type [\code{character}] \cr
#'     One or more PJS variables, for example "hensikt". See details for a list
#'     of all PJS variables included in the pre made translation table
#'     "pjscode_2_descriptions.csv". If more than one code type should be translated,
#'     they can be given in the vector. You may also use argument
#'     \code{PJS_variable_type = "auto"}, if \code{code_colname} have standardized
#'     PJS column names only, see details.
#' @param code_colname [\code{character}] \cr
#'     The name of the column with codes that should be translated. If several codes
#'     should be translated, a vector with the names of the coded variables should be given.
#' @param new_column [\code{character}] \cr
#'     The name of the new column with the text describing the code. If several
#'     codes should be translated, a vector with the new column names should be
#'     given. You may also use argument \code{new_column = "auto"}, if \code{code_colname}
#'     have standardized PJS column names only, see details.
#' @param position [\code{character}] \cr
#'     Position for the new columns, can be one of c("first", "left", "right",
#'     "last", "keep"). If several codes should be translated, either one value
#'     to be applied for all may be given or a vector with specified position
#'     for each code to be translated should be given. Defaults to "right".
#' @template overwrite
#' @param backward [\code{logical(1)}] \cr
#'     If \code{TRUE}, it translates from descriptive text and back to PJS code,
#'     see details. Defaults to \code{FALSE}.
#' @param impute_old_when_missing [\code{logical(1)}] \cr
#'     Should existing value be transferred if no value for the code is found?
#'     Defaults to \code{FALSE}.
#' @param filename [\code{character(1)}] \cr
#'     File name of the source file for the translation table for PJS codes.
#' @param from_path [\code{character(1)}] \cr
#'     Path for the source translation table for PJS codes.
#' @param to_path [\code{character(1)}] \cr
#'     Path for the target translation table for PJS codes when copying the
#'     translation table.
#'
#' @return \code{add_PJS_code_description} A data frame where the description text
#'     for the PJS code has been added in the column to the right of the column
#'     with the code. If the input is a tibble, it will be transformed to a data frame.
#'
#'     \code{read_PJS_codes_2_text} A data frame with the translation table for PJS
#'     codes as read from the source csv-file. If not changing standard input, the
#'     standard file at NVI's internal network is read.
#'
#'     \code{copy_PJS_codes_2_text} Copies the source translation table for PJS codes
#'     to another location. If the target file already exists the source file is only
#'     copied if it is newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @importFrom magrittr %>%
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
#' newdata2 <- add_PJS_code_description(olddata,
#'                                     PJS_codes_2_text,
#'                                     PJS_variable_type = c("hensikt", "kjennelse"),
#'                                     code_colname = c("hensiktkode", "konklusjonkode"),
#'                                     new_column = c("hensikt", "konklusjonskjennelse"))
#'
#' # Translating hensiktkode into hensikt and konklusjonkode to konklusjonskjennelse using "auto"
#' newdata3 <- add_PJS_code_description(olddata,
#'                                     PJS_codes_2_text,
#'                                     PJS_variable_type = c("auto"),
#'                                     code_colname = c("artkode", "hensiktkode", "konklusjonkode"),
#'                                     new_column = c("auto"))
#'
#' # Translating art with species and breed names to only species names
#' # First the text in art is back-translated to the artkode
#' newdata4 <- add_PJS_code_description(data = olddata,
#'                                      PJS_variable_type = "artrase",
#'                                      code_colname = "art",
#'                                      new_column = "artkode",
#'                                      backward = TRUE,
#'                                      impute_old_when_missing = TRUE)
#'
#' # Thereafter, the code is translated to art
#' # By using `impute_old_when_missing = TRUE`, you ensure that text that cannot
#' # be translated back to code, is reported as text in the end result.
#' newdata4 <- add_PJS_code_description(data = newdata4,
#'                                      PJS_variable_type = "art",
#'                                      code_colname = "artkode",
#'                                      new_column = "art",
#'                                      position = "keep",
#'                                      overwrite = TRUE,
#'                                      impute_old_when_missing = TRUE)
#' }
#'
add_PJS_code_description <- function(data,
                                     translation_table = PJS_codes_2_text,
                                     PJS_variable_type,
                                     code_colname,
                                     new_column,
                                     position = "right",
                                     overwrite = FALSE,
                                     backward = FALSE,
                                     impute_old_when_missing = FALSE) {

  if (PJS_variable_type[1] == "auto" | new_column[1] == "auto") {
    code_description_colname <- NVIdb::PJS_code_description_colname
    if (isTRUE(backward)) {
      code_description_colname <- dplyr::rename(code_description_colname, new_column = code_colname, code_colname = new_column)
    }
    PJS_types_selected <- as.data.frame(code_colname) %>%
      dplyr::left_join(code_description_colname, by = "code_colname")
    PJS_types_selected <- subset(PJS_types_selected, !is.na(PJS_types_selected$type))
  }

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()

  # Perform checks
  # data
  checkmate::assert_data_frame(data, add = checks)
  # translation_table
  checkmate::assert_data_frame(translation_table, add = checks)
  # code_colname
  checkmate::assert_vector(code_colname, any.missing = FALSE, min.len = 1, add = checks)
  NVIcheckmate::assert_names(code_colname,
                             type = "named",
                             subset.of = colnames(data),
                             comment = paste0("The code_colname must be a column in the data",
                                              # deparse(substitute(data)),
                                              ", but '",
                                              base::setdiff(code_colname, colnames(data)),
                                              "' is not a column in the data"),
                             add = checks)
  # auto and PJS_variable_type/new_column
  if (PJS_variable_type[1] == "auto" | new_column[1] == "auto") {
    NVIcheckmate::assert_subset_character(code_colname,
                                          choices = unique(code_description_colname$code_colname),
                                          comment = paste("when 'PJS_variable_type' or 'new_column' equals 'auto'",
                                                          "the code_colnames must be standardized PJS column names.",
                                                          "You can use NVIdb::standardize_PJSdata to standardize."),
                                          add = checks)
  }
  # PJS_variable_type
  if (PJS_variable_type[1] != "auto") {
    checkmate::assert_subset(PJS_variable_type,
                             choices = unique(translation_table$type),
                             add = checks)
  }
  # new_column
  checkmate::assert_vector(new_column, any.missing = FALSE, min.len = 1, add = checks)
  if (new_column[1] == "auto") {
    new_column <- PJS_types_selected$new_column
  }
  if (isFALSE(overwrite)) {
    NVIcheckmate::assert_names(new_column,
                               type = "named",
                               disjunct.from = setdiff(colnames(data), code_colname),
                               comment = paste0("The column name(s): '",
                                                intersect(colnames(data), new_column),
                                                "' already exist in '",
                                                deparse(substitute(data)),
                                                "`. Either give new column name(s) for the column(s) called '",
                                                intersect(colnames(data), new_column),
                                                "' or specify overwrite = TRUE to replace values in the existing column(s) with new content"),
                               add = checks)
  }
  NVIcheckmate::assert_names(new_column,
                             type = "named",
                             disjunct.from = code_colname,
                             comment = paste0("You cannot give any of the new column(s) the same name as the code_colname '",
                                              code_colname,
                                              "' in the data" # ,
                                              # deparse(substitute(data)), "`"
                             ),
                             add = checks)

  # position
  NVIcheckmate::assert_subset_character(x = unique(position), choices = c("first", "left", "right", "last", "keep"), add = checks)
  # overwrite
  checkmate::assert_flag(overwrite, add = checks)
  # backward
  checkmate::assert_flag(backward, add = checks)
  # impute_old_when_missing
  checkmate::assert_flag(impute_old_when_missing, add = checks)

  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE ARGUMENTS ----
  # Generates PJS_variable_type if "auto".
  # new_column was generated above because the new column names should be checked in the argument checking
  if (PJS_variable_type[1] == "auto") {
    PJS_variable_type <- PJS_types_selected$type
  }


  # Transforms position to a vector with the same length as number of PJS variables to be translated
  if (length(position) == 1 & length(code_colname) > 1) {position <- rep(position, length(code_colname))}

  # In bakcward translation, imputation of old if missing must be performed after original case have been restored,
  #  i.e. it cannot be done by add_new_column, but must be done afterwards.
  impute_old_when_missing_backward <- impute_old_when_missing
  if (isTRUE(backward) & isTRUE(impute_old_when_missing)) {
    impute_old_when_missing <- FALSE
  }


  # runs the translation for several PJS variables at a time if wanted
  for (i in 1:length(code_colname)) {

    # Make a subset with only the codes that is relevant for the actual variabel
    code_2_description <- translation_table[base::which(translation_table$type == PJS_variable_type[i]), ]

    # Transform the translation file in the case that backward translation should be used
    if (isTRUE(backward)) {
      # Removes breeds from table if type = "art"
      if (PJS_variable_type[i] == "art") {
        code_2_description[which(substr(code_2_description$kode, 1, 2) %in% c("03", "05") &
                                   !substr(code_2_description$kode, 1, 8) %in% c("03100203")), "kode"] <-
          substr(code_2_description[which(substr(code_2_description$kode, 1, 2) %in% c("03", "05") &
                                            !substr(code_2_description$kode, 1, 8) %in% c("03100203")), "kode"], 1, 11)
      }
      # Removes non-unique description text, usually levels without name, i.e. "-"
      # Swips navn - kode
      code_2_description$navn <- tolower(code_2_description$navn)
      code_2_description <- unique(code_2_description)
      column_names <- colnames(code_2_description)
      navn_nr <- which(column_names == "navn")
      kode_nr <- which(column_names == "kode")
      column_names[c(navn_nr, kode_nr)] <- c("kode", "navn")
      colnames(code_2_description) <- column_names
      code_2_description <- code_2_description %>%
        dplyr::add_count(dplyr::across(c("type", "kode")), name = "antall")


      # code_2_description <- code_2_description %>%
        # dplyr::mutate(navn = tolower(.data$navn)) %>%
        # dplyr::distinct() %>%
        # dplyr::rename(kode = .data$navn, navn = .data$kode) %>%
        # dplyr::filter(is.na(.data$utgatt_dato)) %>%
        # dplyr::add_count(.data$type, .data$kode, name = "antall") %>%
        # dplyr::filter(.data$antall == 1) %>%
        # dplyr::select(-.data$antall)
      code_2_description <- subset(code_2_description, code_2_description$antall == 1)
      code_2_description$antall <- NULL

      # Transforms code_colname in data to lower case.
      data$code_colname_org_case <- data[, code_colname[i]]
      data[, code_colname[i]] <- sapply(data[, code_colname[i]], FUN = tolower)
    }

    # code_2_description <- translation_table[base::which(translation_table$type == PJS_variable_type[i] & is.na(translation_table$utgatt_dato)), ]

    # # Changes the name of navn to text wanted in the df (txtvarname)
    # base::colnames(code_2_description)[base::which(base::colnames(code_2_description)=="navn")] <- new_column

    # Calls function that adds description at the position = position in the relation to the code
    data <- add_new_column(data,
                           ID_column = code_colname[i],
                           new_colname = new_column[i],
                           translation_tables = list(code_2_description),
                           ID_column_translation_table = c("kode"),
                           to_column_translation_table = c("navn"),
                           position = position[i],
                           overwrite = overwrite,
                           impute_old_when_missing = impute_old_when_missing)


    if (isTRUE(backward)) {
      # Restores original case in code_colname
      data[, code_colname[i]] <- data$code_colname_org_case
      data$code_colname_org_case <- NULL
      # Imputes old if missing for backward translation
      if (isTRUE(impute_old_when_missing_backward)) {
        data[which(is.na(data[, new_column[i]])), new_column[i]] <- data[which(is.na(data[, new_column[i]])), code_colname[i]]
      }
      # Fix of difficulties translating NA correct. Should probably be fixed elsewhere
      data[which(is.na(data[, code_colname[i]])), new_column[i]] <- NA
    }
  }
  return(data)
}

# To avoid checking of the variable kommune_fylke as default input argument in the function
utils::globalVariables("PJS_codes_2_text")
