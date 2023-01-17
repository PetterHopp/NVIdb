#' @title standardize_eos_data 
#'
#'
#'
#'

standardize_eos_data <- function(data,
                                 dbsource = deparse(substitute(data)), 
                                 standard = NULL, 
                                 breed2species = TRUE, 
                                 adjust_n_examined = TRUE,
                                 ...) {
  
  # standardize_columns
  data <- standardize_columns(data = data,
                                  dbsource = dbsource,
                                  standard = standard,
                                  property = "colnames",
                                  ...) 
    
    
  
  
  # remove double rows
  scrapie <- scrapie %>% add_count(saksnr, name = "ant_per_sak") %>% add_count(saksnr, rekvirent_nr, name = "ant_per_MT") %>% 
    mutate(id = case_when(ant_per_sak == 2 & ant_per_MT == 1 ~ NA_integer_, 
                          TRUE ~ as.integer(id))) %>%
    select(-c(rekvirent_type, rekvirent_nr, rekvirent, ant_per_sak, ant_per_MT)) %>%
    distinct() 
  
  
  # backtranslate breed to species
  add_PJS_code_description(PJS_variable_type = "art",
                           code_colname = c("art" = "dyrekategori"),
                           new_column = c("artkode" = "ok_artkode"),
                           backward = TRUE) 
    
  
  # adjust number of examined
  
  
}

