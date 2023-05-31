# Remove NOTE when running CMD check and checking dependencies
# Namespaces in Imports field not imported from:
#   'NVIrpackages' 'rlang' 'rmarkdown' 'shiny' 'tidyr'
# All declared Imports should be used.


ignore_unused_imports <- function() {
  # Removes NOTE because of packages needed for building vignette: "Contribute to ..."
  rmarkdown::html_vignette
  knitr::opts_chunk
  NVIrpackages::NVIpackages
  # R.rsp is used when adding the pdf reference manual as a vignette
  R.rsp::rfile
}
