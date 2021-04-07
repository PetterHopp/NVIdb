# generate_PJS_testdata
# read PJSdata and save as testdata

library(RODBC)
library(NVIdb)

journal_rapp <- login_PJS()
PJS_testdata <- sqlQuery(journal_rapp,
                         "select * from v2_sak_m_res where aar = 2020 and innsendelsesnummer = 10",
                         as.is = TRUE,
                         stringsAsFactors = FALSE)
odbcClose(journal_rapp)

# Anonymize names and ID's
PJS_testdata <- PJS_testdata %>%
  dplyr::filter(provenummer < 10) %>%
  # dplyr::mutate(antall = nchar(rekvirentnr)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(rekvirentnr = substr("1234567890", 1, nchar(rekvirentnr)),
                eier_lokalitetnr = substr("12345678901234567890", 1, nchar(eier_lokalitetnr)),
                id_nr = substr("12345678901234567890", 1, nchar(id_nr)),
                postnr = substr("12345678901234567890", 1, nchar(postnr)),
                kommunenr = substr("12345678901234567890", 1, nchar(kommunenr)),
                tilleggsnr = substr("12345678901234567890", 1, nchar(tilleggsnr)),
                landnr = substr("12345678901234567890", 1, nchar(landnr)),
                kartreferanse = NA_character_,
                eierreferanse = NA_character_,
                innsenderreferanse = NA_character_,
                merking_id = "123456",
                fodselsdato = NA_character_,
                eksportland = NA_character_,
                tidl_eier = 0,
                skrottnr = NA_character_,
                navn = "Xx Xx")


saveRDS(PJS_testdata, "./tests/PJS_testdata.rds")
