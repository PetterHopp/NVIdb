# generate_PJS_testdata
# read PJSdata and save as testdata

library(RODBC)
library(dplyr)
library(NVIdb)

# Retrieve PJS data as template for testdata
journal_rapp <- login_PJS()
PJS_testdata <- sqlQuery(journal_rapp,
                         "select * from v2_sak_m_res where aar = 2020 and innsendelsesnummer <= 3000",
                         as.is = TRUE,
                         stringsAsFactors = FALSE)
odbcClose(journal_rapp)

# Count number of samples per sak. Used to exclude saker depending on number of samples
sak <- PJS_testdata %>%
  select(aar, ansvarlig_seksjon, innsendelsesnummer, provenummer) %>%
  distinct() %>%
  add_count(aar, ansvarlig_seksjon, innsendelsesnummer, name = "ant_prover_per_sak") %>%
  select(-provenummer) %>%
  distinct()

PJS_testdata <- PJS_testdata %>%
  left_join(sak, by = c("aar", "ansvarlig_seksjon", "innsendelsesnummer"))

# Select journals
# Define test categories
PJS_selected <- PJS_testdata %>%
  dplyr::mutate(category = case_when(eier_lokalitetstype == "LAND" ~ "LAND",
                                     eier_lokalitetstype != "LAND" & substr(hensiktkode, 1, 2) == "09" ~ "QA",
                                     TRUE ~ ansvarlig_seksjon)) %>%

  dplyr::mutate(category = case_when(substr(hensiktkode, 1, 7) == "0200102" & substr(konklusjonkode, 1, 2) != "02" ~ "OKV1",
                                     substr(hensiktkode, 1, 7) == "0200102" & substr(konklusjonkode, 1, 2) == "02" & trimws(konkl_analyttkode) == "01150101" ~ "OKV2",
                                     substr(hensiktkode, 1, 7) == "0200102" & substr(konklusjonkode, 1, 2) == "02" & trimws(konkl_analyttkode) == "01150101070103" ~ "OKV3",
                                     TRUE ~ category)) %>%

  dplyr::mutate(category = case_when(substr(hensiktkode, 1, 7) == "0200124" & substr(konklusjonkode, 1, 2) != "02" ~ "OKS1",
                                     substr(hensiktkode, 1, 7) == "0200124" & substr(konklusjonkode, 1, 2) == "02" & trimws(konkl_analyttkode) == "1502010103" ~ "OKS2",
                                     substr(hensiktkode, 1, 7) == "0200124" & substr(konklusjonkode, 1, 2) == "02" & trimws(konkl_analyttkode) == "150201010301" ~ "OKS3",
                                     TRUE ~ category)) %>%
  # Filter journals
  dplyr::filter(!category %in% c("06", "22", "23", "40", "50", "70", "80", "93")) %>%
  dplyr::filter((ant_prover_per_sak <= 2 & !category %in% c("OKV1", "OKV2")) |
                  (ant_prover_per_sak <= 1 & category %in% c("OKV1", "OKV2"))) %>%
  # Unique journals
  dplyr::select(aar, ansvarlig_seksjon, innsendelsesnummer, category) %>%
  dplyr::distinct() %>%
  dplyr::arrange(aar, ansvarlig_seksjon, innsendelsesnummer)

# Make new column with random number. By initialising the seed this is reproducible
set.seed(2)
PJS_selected$random <- stats::runif(n = dim(PJS_selected)[1])
PJS_selected$innsnr <- round(stats::runif(n = dim(PJS_selected)[1], min = 1, max = 3000))
PJS_selected$weeknr <- round(stats::runif(n = dim(PJS_selected)[1], min = 1, max = 52))

# Selects one journal per category
PJS_selected <- PJS_selected %>%
  dplyr::group_by(category) %>%
  dplyr::slice_min(random) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(random))


# Prepare test data
PJS_testdata <- PJS_testdata %>%
  # Select saker
  dplyr::right_join(PJS_selected, by = c("aar", "ansvarlig_seksjon", "innsendelsesnummer")) %>%
  dplyr::filter(!category %in% c("OKV1", "OKV2") |
                  (category %in% c("OKV1", "OKV2") &
                     substr(konkl_analyttkode, 1, 8) %in% c("01150101", "01050204") &
                     substr(analyttkode_funn, 1, 8) %in% c("01150101", "01050204"))) %>%
  # Anonymize names and ID's
  dplyr::rowwise() %>%
  dplyr::mutate(date_correction = (weeknr - as.numeric(substr(ISOweek::ISOweek(as.Date(mottatt_dato, "%d.%m.%y")), 7, 8))) * 7,
                rekvirenttype = "TYPE",
                rekvirentnr = substr("12345678901234567890", 1, nchar(rekvirentnr)),
                eier_lokalitetnr = substr("12345678901234567890", 1, nchar(eier_lokalitetnr)),
                mottatt_dato = format(as.Date(mottatt_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                uttaksdato = format(as.Date(uttaksdato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                sak_avsluttet = format(as.Date(sak_avsluttet, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                uttaksdato_parprove = format(as.Date(uttaksdato_parprove, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                mottatt_dato_parprove = format(as.Date(mottatt_dato_parprove, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                godkjent_dato = format(as.Date(godkjent_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                avsluttet_dato = format(as.Date(avsluttet_dato, "%d.%m.%y") + date_correction, "%d.%m.%y"),
                # avsluttet_dato_sens = format(as.Date(avsluttet_dato_sens, "%d.%m.%y") + date_correction, "%d.%m.%y"),
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
                alder = NA_character_,
                eksportland = NA_character_,
                importdato = NA_character_,
                tidl_eier = 0,
                avkom_imp_dyr = NA,
                merknad_til_materiale = NA_character_,
                skrottnr = NA_character_,
                provemerknad = NA_character_,
                id_nr = substr("12345678901234567890", 1, nchar(id_nr)),
                stamme = NA_character_,
                innsendelsesnummer = innsnr,
                fagnr = ceiling(innsnr / 2),
                navn = "Xxxx Xxxxxxx",
                postnr = substr("12345678901234567890", 1, nchar(postnr))) %>%
  dplyr::select(-c(innsnr, weeknr, ant_prover_per_sak, category, date_correction))

# Save testdata
saveRDS(PJS_testdata, "./tests/testthat/PJS_testdata.rds")

# REMOVE DATA FROM ENVIONMENT TO AVOID CONFLICTS WHEN ATTACHING PACKAGE ----
rm(PJS_testdata)
