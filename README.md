NVIdb: facilitating the use of the Norwegian Veterinary Institute's databases
================

  - [Overview](#overview)
  - [Package installation and update](#package-installation-and-update)
  - [Attaching packages](#attaching-packages)

# Overview
The Norwegian Veterinary Institute has several internal databases as well as access to data from external databases. `NVIdb` provides functions to facilitate downloading and processing of data from such databases, in particular PJS and EOS. 
`NVIdb` comprises four categories of functions: 
    1. Manage credentials (i.e. password and username), 
    2. Login functions for database services, 
    3. Read, copy and update different in-house data registers,
    4. Translate codes into descriptions.
    
By combining the functions for managing username and password and the login functions, you are able to login to PJS and EOS (and potentially other databases) without hard coding the password in the script. Thereafter, you may extract data from PJS and EOS using the R-package `RODBC`.

The read copy and update functions for data from NVI's internal databases gives you access to these data without any need of knowing where the data are stored. Currently you get access to registers of administrative regions (Kommune, Fylke, Postnummer, Mattilsynets avdelinger og regioner), produsentinformasjon (Produksjonstilskuddsregister and new and old prodnr) and translation tables for PJS-codes into descriptive text.

There are also functions to use these registers to translate variables with codes into names and others. You can translate
  - PJS-codes into descriptive text, 
  - kommunenr into kommune, fylke, as well as current kommunenr, kommune, fylkenr and fylke
  - kommunenr into Mattilsynets avdeling and region
  - postnr into poststed and poststedets kommunenr
  - old produsentnr into current produsentnr
New functionality is continuously added, please check the NEWs.


# Package installation and update

The easiest way to install the `NVIdb` package is from https://github.com/NorwegianVeterinaryInstitute. 
The package is publicly available. However, you will also need to install the private package `NVIconfig` 
that keep specific information on NVI's paths and databases. `NVIconfig` is expected to be relatively 
stable and you will not need to update `NVIconfig` every time `NVIdb` is updated. To install `NVIdb`
you will need:
  - R version > 3.5.0
  - R packages "devtools"
  - Rtools 3.5 or 4.0 depending on your R-version
In addition, to install `NVIconfig` you will need:
  - A personal GitHub account
  - To be member of the organisation NorwegianVeterinaryInstitute at GitHub
  - A personal access token (PAT) for downloading packages from GitHub
  
This requires installing the `devtool` package first.  

``` r
install.packages("devtools")
```

Then attach the `devtool` package:

``` r
library(devtools)
```

In order to install (or update) the `NVIdb` package, run the following code:

``` r
remotes::install_github("NorwegianVeterinaryInstitute/NVIdb", 
	upgrade = FALSE, 
	build = TRUE,
	build_manual = TRUE)
```

In order to install (or update) the `NVIconfig` package, run the following code:


``` r
remotes::install_github("NorwegianVeterinaryInstitute/NVIdb", 
	auth_token = "PAT"
	upgrade = FALSE, 
	build = TRUE,
	build_manual = TRUE)
```

where PAT is your personal access token.

# Attaching packages

The `NVIdb` package needs to be attached.

``` r
library(NVIdb)
```

The list of available functions and datasets can be accessed by typing

``` r
help(package="NVIdb")
```

