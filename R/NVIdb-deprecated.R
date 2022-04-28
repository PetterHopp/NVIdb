#' @title Deprecated Functions in Package NVIdb 
#' @description These functions are provided for compatibility with older 
#'     versions of NVIdb only, and may be defunct as soon as the next release.
#' @details \code{add_produsent} was deprecated 2022-05-02 as other properties 
#'     than 'gjeldende_prodnr8' could not be included without breaking backward 
#'     compatibility. Use \code{add_produsent_properties} instead and ensure 
#'     to set the parameter \code{impute_old_when_missing = TRUE} when translating 
#'     from "prodnr8" to "gjeldende_prodnr8" and set the parameter 
#'     \code{impute_old_when_missing = FALSE} when translating from "prodnr8" to 
#'     other properties. 
#'
#' @param \dots (arguments) 
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @rdname NVIdb-deprecated
#' @name NVIdb-deprecated
#' #' @examples
#' \dontrun{
#' add_produsent(...)   ### -- use add_produsent_properties(...)   instead
#' }
#'

#' @rdname NVIdb-deprecated
#' @aliasis add_produsent-deprecated
#' @export
#' 
add_produsent <- function(...) {
  
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

