#' @title Manage translation from komnr to kommune, fylke and current komnr
#' @description Function to add columns with kommune (name), fylkenr, fylke (name), gjeldende_komnr, gjeldende_kommune,
#'    gjeldende_fylkenr, and gjeldende_fylke. In addition there are functions to read and copy the translation tables.
#'
#' @details Data sources, like PJS, may provide data with komnr. These functions manage translating komnr to current komnr,
#'     kommune and fylke.
#'
#'     \code{add_kommune_fylke} can be used to translate komnr into kommune (name), fylkenr, fylke (name), gjeldende_komnr,
#'     gjeldende_kommune, gjeldende_fylkenr, and gjeldende_fylke. The function can also be used to translate fylkenr into fylke (name),
#'     gjeldende_fylkenr, and gjeldende_fylke.
#'
#'     One has to ensure that the code in the dataset represents a komnr or fylkenr. The function will translate any 4 and 2 digits
#'     that has the same ID as a kommune or fylke, respectively.
#'
#'     Standard name for the komnr is komnr. If the column with the komnr that should be translated has another name, the
#'     parameter \code{code_column =} can be input as a named vector. Standard names for the new columns are c("kommune", "fylkenr", "fylke",
#'     "gjeldende_komnr", "gjeldende_kommune", "gjeldende_fylkenr", "gjeldende_fylke"). Likewise, if the new columns should be given
#'     other names than, the parameter \code{new_column =} can be input as a named vector, see examples.
#'
#'     The function uses a premade translation tables that is made based on information in PJS adresseregister. The translation table
#'     is updated when informed that know there is a need, typically when there have been changes in kommune-structure.
#'
#'     \code{position =} is used to give the place if the new columns in the data.frame. For \code{position = "right"} the new variables are
#'     placed to the right of the code_variable. Likewise, for \code{position = "left"} the new variables are placed to the left of the
#'     code_variable. If \code{position = "first"} or \code{position = "last"} the new columns are placed first or last, respectively, in the
#'     data.frame. A special case occurs for \code{position = "keep"} which only has meaning when the new column has the same name as an existing
#'     column and overwrite = TRUE. In these cases, the existing column will be overwritten with new data and have the same position.
#'
#'     \code{read_kommune_fylke} read the files "komnr_2_gjeldende_komnr_UTF8.csv", Kommune_UTF8.csv, and Fylke_UTF8.csv, into a single data
#'     frame that can be used by \code{add_kommune_fylke}. Standard setting will read in the file from NVI's internal network. If changing the
#'     from_path, the function can be used to read the translation files from other directories. This can be useful if having a stand alone app
#'     with no connection the NVI's internal network. In other cases, it should be avoided.
#'
#'     \code{copy_kommune_fylke} copy the files komnr_2_gjeldende_komnr_UTF8.csv, Kommune_UTF8.csv, and Fylke_UTF8.csv, respectively, to a given
#'     directory.
#'
#'
#' @param data Data frame with data with a column with old komnr
#' @param translation_table Data frame with the translation table for old komnr to current komnr
#' @param code_column The name of the column with the old komnr
#' @param new_column The name of the new column that should contain the current komnr
#' @template position
#' @template overwrite
#' @param filename Filename of the translation table for old komnr to current komnr
#' @param from_path Path for the source translation table
#' @param to_path Path for the target translation table when copying the translation table
#'
#' @return \code{add_kommune_fylke} A data frame where one or more of the columns c("kommune", "fylkenr", "fylke", "gjeldende_komnr",
#'     "gjeldende_kommune", "gjeldende_fylkenr", "gjeldende_fylke") have been added in the column(s) to the right of the column with the
#'     komnr.
#'
#'     \code{read_kommune_fylke} A data frame with the original komnr and the corresponding kommune, fylkenr, fylke, and the current
#'     komnr, kommune, fylkenr, fylke. If not changing standard input to the function, the standard file at NVI's internal network is read.
#'
#'     \code{copy_kommune_fylke} copies the source translation table for komnr to kommune, for old komnr to current komnr, and for
#'     fylkenr to fylke to given directory. If the target file already exists, the source file is copied only when it is newer than the target file.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' kommune_fylke <- read_kommune_fylke()
#'
#' # Copy standard file from standard location to the subdirectory Data below the working directory
#' copy_kommune_fylke(to_path = "./Data/")
#'
#' # Reading from the subdirectory Data below the working directory
#' kommune_fylke <- read_kommune_fylke(from_path = "./Data/")
#'
#' # Add new column with current komnr and kommune
#' # The variable gammelt_komnr should be translated and the new variables with gjeldende_komnr" and
#' # "gjeldende_kommune" is named komnr and kommune, respectively.
#' newdata <- add_kommune_fylke(olddata,
#'                              translation_table = kommune_fylke,
#'                              code_column = c("gammelt_komnr" = "komnr"),
#'                              new_column = c("komnr" = "gjeldende_komnr",
#'                                             "kommune" = "gjeldende_kommune"))
#' }
#'
add_kommune_fylke <- function(data,
                              translation_table = kommune_fylke,
                              code_column = c("komnr"),
                              new_column = c("gjeldende_komnr", "gjeldende_kommune", "gjeldende_fylkenr", "gjeldende_fylke"),
                              position = "right",
                              overwrite = FALSE) {

  # Ensure that code_column and new_column are named vectors by using the internal function set_name_vector()
  # Thereby, the following code can assume these to be named vectors
  code_column <- set_name_vector(code_column)
  new_column <- set_name_vector(new_column)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checks <- assert_add_functions(data = data,
                                 translation_table = translation_table,
                                 code_column = code_column,
                                 new_column = new_column,
                                 overwrite = overwrite,
                                 add = checks)
  # position
  position <- NVIcheckmate::match_arg(x = position,
                                      choices = c("first", "left", "right", "last", "keep"),
                                      several.ok = TRUE,
                                      ignore.case = FALSE,
                                      add = checks)
  # Report check-results
  checkmate::reportAssertions(checks)

  # PREPARE TRANSLATION TABLE ----
  # Makes the translation table with code_column and new_column. unique() is necessary to avoid duplicate
  # rows when code_column is not "komnr"
  code_2_new <- unique(translation_table[, c(unname(code_column), unname(new_column))])

  if (code_column == "fylkenr") {
    code_2_new <- merge(code_2_new, translation_table[, c("fylkenr", new_column, "komnr")], by = c("fylkenr", new_column))
    code_2_new <- stats::aggregate(stats::as.formula(paste("komnr", "~", paste(c(code_column, new_column), collapse = " + "))), data = code_2_new, FUN = length)

    # For fylkenr, select the fylke where most kommuner is within the fylke. This to avoid fylkenr to be translated to fylker
    # where one or a few kommuner has been relocated.
    # code_2_new <- code_2_new %>%
    #   dplyr::rename(antall = dplyr::all_of("komnr")) %>%
    #   dplyr::distinct() %>%
    #   dplyr::group_by(.data$fylkenr) %>%
    #   dplyr::mutate(maxantall = max(.data$antall)) %>%
    #   dplyr::ungroup() # %>%
    colnames(code_2_new)[which(colnames(code_2_new) == "komnr")] <- "antall"
    code_2_new <- unique(code_2_new)
    aggregated_data <- stats::aggregate(stats::as.formula("antall ~ fylkenr"), data = code_2_new, FUN = max)
    colnames(aggregated_data)[2] <- "max_antall"
    code_2_new <- merge(code_2_new, aggregated_data, by = "fylkenr", all.x = TRUE)
    # code_2_new <- code_2_new[order(filnavn$fra_dato, filnavn$til_dato, decreasing = TRUE), ]

    code_2_new <- subset(code_2_new, code_2_new$max_antall == code_2_new$antall)
    code_2_new[, c("antall", "maxantall")] <- c(NULL, NULL)
      # dplyr::filter(.data$maxantall == .data$antall) %>%
    # dplyr::select(-.data$antall, -.data$maxantall)

    # Removes tibble in case it makes trouble later
    code_2_new <- as.data.frame(code_2_new)

  }

  # ADD NEW COLUMN(S) ----
  # Set up of parameters for the internal function add_new_column(). names() is used to select the column names
  # in the input data and unname() is used to select the column names in the translation table. n_columns_at_once
  # is the number of new columns that should be added.
  data <- add_new_column(data,
                         ID_column = names(code_column),
                         new_colname = names(new_column),
                         translation_tables = list(code_2_new),
                         ID_column_translation_table = unname(code_column),
                         to_column_translation_table = unname(new_column),
                         position = position,
                         overwrite = overwrite,
                         n_columns_at_once = length(new_column)
  )


  return(data)
}

# To avoid checking of the variable kommune_fylke as default input argument in the function
utils::globalVariables("kommune_fylke")
