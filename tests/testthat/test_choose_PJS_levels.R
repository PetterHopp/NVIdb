library(NVIdb)
library(testthat)

test_that("Choose variables from PJS levels", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  # Standardisere kolonnenavn
  PJStest <- standardize_columns(data = PJStest, property = "colnames")
  # Removes vet_distriktnr and konkl_provenr
  PJStest$vet_distriktnr <- NULL
  PJStest$konkl_provenr <- NULL

  # test levels
  correct_result <- c("aar", "ansvarlig_seksjon", "innsendelsenr", "hensiktkode", "rekvirenttype",
                      "rekvirentnr", "eier_lokalitettype", "eier_lokalitetnr", "mottatt", "uttatt",
                      "avsluttet", "landnr", "komnr", "gruppenr",
                      "annen_aktortype", "annen_aktornr", "merknad", "fagkode", "fagnr",
                      "fagansvarlig_person", "karantene", "kartreferanse", "eierreferanse", "innsenderreferanse",
                      "okt_dodelighet", "epi_id", "utbrudd_id", "utbrudd_aar", "utbruddnr",
                      "provenr", "driftsformkode", "artkode", "provematerialekode", "forbehandlingkode",
                      "provetypekode", "fysiologisk_stadiumkode", "alder", "alder_enhet", "ant_prover",
                      "ant_i_samleprove", "uttatt_parprove", "mottatt_parprove", "merking_id", "kjonn",
                      "fodselsdato", "hjelpeprove", "gruppeprove", "eksportland", "importdato",
                      "tidl_eier", "avkom_imp_dyr", "oppstallingkode", "merknad_prove", "okologisk_drift",
                      "skrottnr", "dyrnr", "provemerknad", "id_nr", "id_nr_type",
                      "stamme", "resirkuleringsanlegg", "salinitet", "temperatur", "dybde",
                      "sjosatt_tid", "vaksine", "vaksine_tid", "fortype", "lengde",
                      "helsestatuskode", "konklnr", "konkl_kjennelsekode", "konkl_analyttkode", "konkl_typekode",
                      "eier_lokalitet", "postnr")
  sak_konkl <- choose_PJS_levels(PJStest, levels = c("sak", "prove", "konklusjon"))

  expect_identical(colnames(sak_konkl), correct_result)

  sak_res_konkl <- choose_PJS_levels(PJStest,
                                     levels = c("sak", "prove", "delprove", "undersokelse", "resultat", "konklusjon"))

  expect_identical(colnames(sak_res_konkl), colnames(PJStest))

  PJS_codes_2_text <- read_PJS_codes_2_text()
  PJStest <- add_PJS_code_description(PJStest,
                                      translation_table = PJS_codes_2_text,
                                      PJS_variable_type = c("hensikt"),
                                      code_colname = c("hensiktkode"),
                                      new_column = c("hensikt"))

  sak <- choose_PJS_levels(PJStest, levels = c("sak"))

  expect_identical(colnames(sak),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "hensiktkode", "hensikt", "rekvirenttype",
                     "rekvirentnr", "eier_lokalitettype", "eier_lokalitetnr", "mottatt", "uttatt",
                     "avsluttet", "landnr", "komnr", "gruppenr",
                     "annen_aktortype", "annen_aktornr", "merknad", "fagkode", "fagnr",
                     "fagansvarlig_person", "karantene", "kartreferanse", "eierreferanse", "innsenderreferanse",
                     "okt_dodelighet", "epi_id", "utbrudd_id", "utbrudd_aar", "utbruddnr",
                     "eier_lokalitet", "postnr"))

  prove <- choose_PJS_levels(PJStest, levels = c("prove"))

  expect_identical(colnames(prove),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "driftsformkode",
                     "artkode", "provematerialekode", "forbehandlingkode", "provetypekode",
                     "fysiologisk_stadiumkode", "alder", "alder_enhet", "ant_prover",
                     "ant_i_samleprove", "uttatt_parprove", "mottatt_parprove", "merking_id", "kjonn",
                     "fodselsdato", "hjelpeprove", "gruppeprove", "eksportland", "importdato",
                     "tidl_eier", "avkom_imp_dyr", "oppstallingkode", "merknad_prove", "okologisk_drift",
                     "skrottnr", "dyrnr", "provemerknad", "id_nr", "id_nr_type", "stamme",
                     "resirkuleringsanlegg", "salinitet", "temperatur", "dybde", "sjosatt_tid",
                     "vaksine", "vaksine_tid", "fortype", "lengde", "helsestatuskode"))

  konklusjon <- choose_PJS_levels(PJStest, levels = c("konklusjon"))
  expect_identical(colnames(konklusjon),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "konklnr",
                     "konkl_kjennelsekode", "konkl_analyttkode", "konkl_typekode"))


  delprove <- choose_PJS_levels(PJStest, levels = c("delprove"))
  expect_identical(colnames(delprove),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "delprovenr",
                     "utf_seksjon", "delpr_provematerialekode", "delpr_provetypekode",
                     "ant_delprover", "ant_i_samledelprove", "delpr_forbehandling_kode"))


  undersokelse <- choose_PJS_levels(PJStest, levels = c("undersokelse"))
  expect_identical(colnames(undersokelse),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "delprovenr", "undnr",
                     "metodekode", "und_godkjent", "und_avsluttet"))


  resultat <- choose_PJS_levels(PJStest, levels = c("resultat"))
  expect_identical(colnames(resultat),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "delprovenr", "undnr", "resnr",
                     "res_analyttkode", "verdi_mengde", "enhetkode", "res_kjennelsekode", "det_grense"))

})

test_that("Choose variables from PJS levels subundersokelse og subresultat", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata2.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata2.rds")

  # Standardisere kolonnenavn
  PJStest <- standardize_columns(data = PJStest, property = "colnames")
  # Removes vet_distriktnr and konkl_provenr
  PJStest$vet_distriktnr <- NULL
  PJStest$konkl_provenr <- NULL

  subundersokelse <- choose_PJS_levels(PJStest, levels = c("subundersokelse"))
  expect_identical(colnames(subundersokelse),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "delprovenr", "undnr",
                     "resnr", "subundnr",
                     "subund_metodekode", "subund_avsluttet"))


  subresultat <- choose_PJS_levels(PJStest, levels = c("subresultat"))
  expect_identical(colnames(subresultat),
                   c("aar", "ansvarlig_seksjon", "innsendelsenr", "provenr", "delprovenr", "undnr",
                     "resnr", "subundnr", "subresnr",
                     "subres_analyttkode", "subres_verdi_mengde", "subres_enhetkode", "subres_kjennelsekode"))

})


test_that("Choose variables from PJS levels subundersokelse og subresultat", {
  # skip if no connection to 'FAG' have been established
  skip_if_not(dir.exists(set_dir_NVI("FAG")))

  PJStest <- readRDS(file.path(".", "PJS_testdata.rds"))
  # PJStest <- readRDS("./tests/testthat/PJS_testdata.rds")

  # Standardisere kolonnenavn
  PJStest <- standardize_columns(data = PJStest, property = "colnames")
  # Removes vet_distriktnr and konkl_provenr
  PJStest$vet_distriktnr <- NULL
  PJStest$konkl_provenr <- NULL

  sak_konkl <- choose_PJS_levels(PJStest, levels = c("sak", "prove", "konklusjon"))

  # test keep_col
  prove <- choose_PJS_levels(PJStest, levels = c("sak", "prove", "konklusjon"), keep_col = "delprovenr")
  expect_true(dim(sak_konkl)[1] < dim(prove)[1])
  expect_identical(dim(sak_konkl)[2], dim(prove)[2] - 1L)

  # # test remove_col
  # sak_konkl2 <- choose_PJS_levels(prove, levels = c("sak" , "prove" , "konklusjon"), remove_col = "konklnr")
  #
  # # test remove_col and unique_col
  # sak_konkl <- choose_PJS_levels(PJStest, levels = c("sak" , "prove" , "konklusjon"), remove_col = "konklnr", unique_rows = FALSE)
})
