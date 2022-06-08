# test_build_query_PJS_one_disease

library(NVIdb)
library(testthat)
context("PJS query one disease")

test_that("build query PD", {
  query <- build_query_one_disease(year = 2020,
                                   analytt = c("01220104%", "1502010235"),
                                   hensikt = c("0100108018", "0100109003", "0100111003", "0800109"),
                                   metode = c("070070", "070231", "010057", "060265"))

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND",
                           "( hensiktkode IN ('0100108018', '0100109003', '0100111003', '0800109') OR",
                           "metodekode IN ('070070', '070231', '010057', '060265') OR",
                           "konkl_analyttkode = '1502010235' OR konkl_analyttkode LIKE '01220104%' OR",
                           "analyttkode_funn = '1502010235' OR analyttkode_funn LIKE '01220104%' )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( analyttkode = '1502010235' OR analyttkode LIKE '01220104%' )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})


test_that("build query CMS", {
  query <- build_query_one_disease(year = 2020,
                                   analytt = c("012601%", "15020804"),
                                   metode = "070183")

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND",
                           "( metodekode = '070183' OR",
                           "konkl_analyttkode = '15020804' OR konkl_analyttkode LIKE '012601%' OR",
                           "analyttkode_funn = '15020804' OR analyttkode_funn LIKE '012601%' )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( analyttkode = '15020804' OR analyttkode LIKE '012601%' )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})


test_that("build query Vibrio anguillarum", {
  query <- build_query_one_disease(year = 2020,
                                   analytt = "0406020202%")

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND",
                           "( konkl_analyttkode LIKE '0406020202%' OR analyttkode_funn LIKE '0406020202%' )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( analyttkode LIKE '0406020202%' )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})


test_that("build query Gjellemykose", {
  query <- build_query_one_disease(year = 2020,
                                   analytt = "0702060406")

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND",
                           "( konkl_analyttkode = '0702060406' OR analyttkode_funn = '0702060406' )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( analyttkode = '0702060406' )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})



test_that("build query one disease error testing", {

  expect_error(build_query_one_disease(analytt = "0406020202"),
               regexp = 'argument "year" is missing')

  # expect_error(build_query_one_disease(year = 2020, metode = "070183"),
  #              regexp = 'argument "analytt" is missing')

  expect_error(build_query_one_disease(year = (as.numeric(format(Sys.Date(), "%Y")) + 1), analytt = "0406020202"),
               regexp = "Variable 'year': Element ")

})
