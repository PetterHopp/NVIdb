# generate_PJS_testdata
# read PJSdata and save as testdata

library(RODBC)
library(dplyr)
library(NVIdb)

journal_rapp <- login_PJS()
PJS_testdata <- sqlQuery(journal_rapp,
                         "select * from v2_sak_m_res where aar = 2020 and innsendelsesnummer <= 300 and provenummer <= 3",
                         as.is = TRUE,
                         stringsAsFactors = FALSE)
odbcClose(journal_rapp)


# Select journals
# Define test categories
PJS_selected <- PJS_testdata %>%
  dplyr::mutate(category = case_when(eier_lokalitetstype == "LAND" ~ "LAND",
                                     eier_lokalitetstype != "LAND" & substr(hensiktkode, 1, 2) == "09" ~ "QA",
                                     TRUE ~ ansvarlig_seksjon)) %>%
  # Unique journals
  dplyr::select(aar, ansvarlig_seksjon, innsendelsesnummer, category) %>%
  dplyr::distinct() %>%
  dplyr::arrange(aar, ansvarlig_seksjon, innsendelsesnummer)

# Make new column with random number. By initilising the seed this is reproducable
set.seed(2)
PJS_selected$random <- stats::runif(n = dim(PJS_selected)[1])
PJS_selected$innsnr <- round(stats::runif(n = dim(PJS_selected)[1], min = 1, max = 300))
PJS_selected$weeknr <- round(stats::runif(n = dim(PJS_selected)[1], min = 1, max = 52))

# Selects one journal per category
PJS_selected <- PJS_selected %>%
  dplyr::group_by(category) %>%
  dplyr::slice_min(random) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(category, random))

# mottatt_datoX = format(as.Date(PJS_testdata[1, "mottatt_dato"], "%d.%m.%y") -
#                          ((as.numeric(substr(ISOweek::ISOweek(as.Date(PJS_testdata[1, "mottatt_dato"], "%d.%m.%y"))) -
#                              PJS_testdata[1, "weeknr"]) * 7), "%d.%m.%y")

# Anonymize names and ID's
PJS_testdataX <- PJS_testdata %>%
  dplyr::right_join(PJS_selected, by = c("aar", "ansvarlig_seksjon", "innsendelsesnummer")) %>%
  filter(ansvarlig_seksjon != "70") %>%
  # dplyr::filter(innsendelsesnummer == 10 |
  #                 (innsendelsesnummer == 45 & substr(hensiktkode, 1, 2) == "09") |
  #                 (innsendelsesnummer == 257 & eier_lokalitetstype == "LAND")) %>%
  # dplyr::mutate(provenummer = as.numeric(provenummer)) %>%
  # dplyr::filter(provenummer < 3) %>%
  # dplyr::mutate(provenummer = as.character(provenummer)) %>%
  # dplyr::mutate(antall = nchar(rekvirentnr)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(date_correction = (weeknr - as.numeric(substr(ISOweek::ISOweek(as.Date(mottatt_dato, "%d.%m.%y")), 7, 8))) * 7,
                rekvirenttype = "TYPE",
                rekvirentnr = substr("12345678901234567890", 1, nchar(rekvirentnr)),
                eier_lokalitetnr = substr("12345678901234567890", 1, nchar(eier_lokalitetnr)),
                mottatt_dato = format(as.Date(mottatt_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                # uttatt_dato = format(as.Date(uttatt_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                # avsluttet_dato = format(as.Date(avsluttet_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                # godkjent_dato = format(as.Date(godkjent_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                # undersokt_dato = format(as.Date(undersokt_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                postnr = substr("12345678901234567890", 1, nchar(postnr)),
                kommunenr = substr("12345678901234567890", 1, nchar(kommunenr)),
                tilleggsnr = substr("12345678901234567890", 1, nchar(tilleggsnr)),
                merknad = NA_character_,
                fagansvarlig_person = "XXX",
                landnr = substr("ABCDEFGHIJKL", 1, nchar(landnr)),
                kartreferanse = NA_character_,
                eierreferanse = NA_character_,
                innsenderreferanse = NA_character_,
                epi_id = NA_character_,
                merking_id = "123456",
                fodselsdato = NA_character_,
                eksportland = NA_character_,
                importdato = NA_character_,
                tidl_eier = 0,
                avkom_imp_dyr = NA,
                merknad_til_materiale = NA_character_,
                skrottnr = NA_character_,
                provemerknad = NA_character_,
                id_nr = substr("12345678901234567890", 1, nchar(id_nr)),
                stamme = NA_character_,
                navn = "Xxxx Xxxxxxx",
                innsendelsesnummer = innsnr,
                fagnr = ceiling(innsnr / 2)) # %>%

# dplyr::select(-c(innsnr, weeknr))


saveRDS(PJS_testdata, "./tests/testthat/PJS_testdata.rds")
