#' @title Manage translation table for varekoder til leveransregisteret
#' @description Read the translation table for varekoder til leveransregisteret.
#' @details The translation table for varekoder comprises the variables:
#'     the leveranseaar, varekode, vare (descriptive text), dyreslag,
#'     vareart, dyrekategori, and varekategorikode. The register covers 2016
#'     and later.
#'
#'     \code{read_varekoder} with the argument \code{type = "formatted"}
#'     reads the formatted "varekoder.csv" into a data frame. The standard
#'     settings will read the file from NVI's internal network. If changing
#'     the from_path, the function can be used to read the translation file
#'     from other directories. This can be useful if having a stand alone
#'     app with no connection the NVI's internal network. In other cases,
#'     it should be avoided.
#'
#'     \code{read_varekoder} with the argument \code{type = "raw"} reads
#'     the raw data as supplied from Landbruksdirektoratet into a data frame.
#'     Thereafter, these can be used to generate the formatted version.
#'     The standard settings will read the file from NVI's internal network
#'     and changing the path should be avoided.
#'
#' @param filename Name of the translation table, defaults to "varekoder.csv".
#'     The input is only used when \code{data_source = "formatted"}.
#' @param from_path Path for the translation table for varekoder.
#' @param year Year(s) for fetching the varekoderegister.
#' @param data_source Reads formatted data or raw data. deafult is formatted.
#'
#' @return \code{read_varekoder} A data frame with the translation table for
#'     varekoder to descriptive text and metadata.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # Reading from standard directory at NVI's network
#' varekoder <- read_varekode()
#' }
#'
read_varekode <- function(filename = "varekoder.csv",
                          from_path = paste0(set_dir_NVI("LevReg")),
                          year = NULL,
                          data_source = "formatted") {

  # PREPARE ARGUMENTS BEFORE ARGUMENT CHECKING ----
  from_path <- sub("/+$|\\\\+$", "", from_path)

  # ARGUMENT CHECKING ----
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  # Perform checks
  checkmate::assert_character(from_path, len = 1, min.chars = 1, add = checks)
  checkmate::assert_directory_exists(from_path, access = "r", add = checks)
  if (data_source == "formatted") {
    checkmate::assert_character(filename, len = 1, min.chars = 1, null.ok = FALSE, add = checks)
    checkmate::assert_file_exists(x = file.path(from_path, "FormaterteData", filename), access = "r", add = checks)
  } else {
    checkmate::assert_character(filename, len = 1, min.chars = 1, null.ok = TRUE, add = checks)
  }
  checkmate::assert_subset(data_source,
                           choices = c("formatted", "raw"),
                           add = checks)
  if (data_source == "formatted") {
    checkmate::assert(checkmate::check_integerish(as.numeric(year[grep("[[:alpha:]]", year, invert = TRUE)]),
                                                  lower = 1995,
                                                  upper = as.numeric(format(Sys.Date(), "%Y")),
                                                  any.missing = FALSE,
                                                  all.missing = FALSE,
                                                  unique = TRUE,
                                                  null.ok = FALSE),
                      checkmate::check_choice(year, choices = c("last"), null.ok = TRUE),
                      add = checks)
  } else {
    checkmate::assert(checkmate::check_integerish(as.numeric(year[grep("[[:alpha:]]", year, invert = TRUE)]),
                                                  lower = 1995,
                                                  upper = as.numeric(format(Sys.Date(), "%Y")),
                                                  len = 1,
                                                  any.missing = FALSE,
                                                  all.missing = FALSE,
                                                  unique = TRUE,
                                                  null.ok = FALSE),
                      checkmate::check_choice(year, choices = c("last"), null.ok = TRUE),
                      add = checks)
  }
  # Report check-results
  checkmate::reportAssertions(checks)

  # IMPORT VAREKODEREGISTER FORMATTED DATA ----
  if (data_source == "formatted") {

    from_path <- file.path(from_path, "FormaterteData")

    # READ DATA ----
    df1 <- read_csv_file(filename = filename,
                         from_path = from_path,
                         options = list(colClasses = c("varekode" = "character"),
                                        fileEncoding = "UTF-8"))

    if (!is.null(year)) {
      if (year[1] == "last") {year <- max(df1$leveranseaar)}
      df1 <- df1[which(df1$leveranseaar %in% as.numeric(year)), ]
    }
  }

  # IMPORT VAREKODEREGISTER RAW DATA ----
  if (data_source == "raw") {
    sub_path <- "RaData"
    # Generate data frame listing all versions of the varekoderegister
    filnavn <- list.files(path = file.path(from_path, sub_path), recursive = TRUE, pattern = "varekode", ignore.case = TRUE)
    filnavn <- as.data.frame(filnavn)
    # Find start and end date that the register is valid for
    filnavn$fra_dato <- as.Date(paste0(substr(filnavn$filnavn, 1, 6), "01"), format = "%Y%m%d")
    filnavn$til_dato <- as.Date(cut(as.Date(paste0(substr(filnavn$filnavn, 8, 13), "01"), format = "%Y%m%d") + 32, "month")) - 1
    filnavn$n_days <- as.numeric(filnavn$til_dato - filnavn$fra_dato + 1)
    filnavn$aar <- as.numeric(format(filnavn$til_dato, "%Y"))
    # Order in decending order by date
    # filnavn <- filnavn %>%
    #   dplyr::group_by(.data$aar) %>%
    #   dplyr::mutate(max_n_days = max(.data$n_days)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::arrange(dplyr::desc(.data$fra_dato), dplyr::desc(.data$til_dato))
    max_n_days <- aggregate(n_days ~ aar, data = filnavn, FUN = max)
    colnames(max_n_days)[2] <- "max_n_days"
    filnavn <- merge(filnavn, max_n_days, by = "aar", all.x = TRUE)
    filnavn <- filnavn[order(filnavn$fra_dato, filnavn$til_dato, decreasing = TRUE), ]
    

    # filnavn <- filnavn[c(2:dim(filnavn)[1]), ]
    # Select varekoder for time period
    # Selects from year
    if (year != "last") {
      # rownr <- which(filnavn[, "n_days"] >= 365 & filnavn[, "aar"] %in% as.numeric(year))
      rownr <- which(filnavn[, "n_days"] == filnavn[, "max_n_days"] & filnavn[, "aar"] == as.numeric(year))
    }
    # Selects from the years covering the last 12 months
    if (year == "last") {
      if (filnavn[1, "n_days"] >= 365) {rownr <- 1}
      if (filnavn[1, "n_days"] < 365) {rownr <- c(1, 2)}
    }

    # # Select varekode raw data for import
    # if (exists("df1")) {rm(df1)}
    for (i in rownr) {
      # i <- 3
      check_header <- readLines(con = paste0(set_dir_NVI("LevReg"), "RaData/", filnavn[i, "filnavn"]), n = 1)
      # "vare" in first line, then header = TRUE
      header <- grepl("vare", check_header, ignore.case = TRUE)
      # identify delimiter
      if (grepl(";", check_header)) {delimiter <- ";"}
      if (grepl(",", check_header)) {delimiter <- ","}

      tempdf <- utils::read.delim(paste0(set_dir_NVI("LevReg"), sub_path, "/", filnavn[i, "filnavn"]),
                                  header = header,
                                  sep = delimiter)

      # if no national characters (represented by "å"), then read again using UTF-8 encoding
      if (isFALSE(any(grepl("\u00E5", tempdf[, 2], ignore.case = TRUE)))) {
        if (isTRUE(any(grepl("<c5>", tempdf[, 2], ignore.case = TRUE)))) {
          file_encoding <- "latin1"
        } else {
          file_encoding <- "UTF-8"
        }
        tempdf <- utils::read.delim(paste0(set_dir_NVI("LevReg"), sub_path, "/", filnavn[i, "filnavn"]),
                                    header = header,
                                    sep = delimiter,
                                    fileEncoding = file_encoding)
      }
      if (dim(tempdf)[2] > 3) {tempdf <- tempdf[, c(1:3)]}
      colnames(tempdf) <- c("varekode", "vare", "dyreslag")
      if (exists("df1")) {
        df1 <- rbind(df1, tempdf)
      } else {
        df1 <- tempdf
      }
      df1 <- unique(df1)
    }
  }

  return(df1)
}
