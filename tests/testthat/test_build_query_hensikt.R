# test_build_query_hensikt

library(NVIdb)
library(testthat)
context("PJS query hensikt")

test_that("build query hensikt", {
  query <- build_query_hensikt(year = 2020, hensikt = "0200102")

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND ( hensiktkode = '0200102' )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( sak.hensiktkode = '0200102' )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)


  # Test more than one hensikt
  query <- build_query_hensikt(year = 2020, hensikt = c("0200135", "0200142"))

  correct_result <- paste("SELECT *",
                           "FROM v2_sak_m_res",
                           "WHERE aar = 2020 AND ( hensiktkode IN ('0200135', '0200142') )")

  expect_equivalent(query["selection_v2_sak_m_res"], correct_result)

  correct_result <- paste("SELECT v_sakskonklusjon.*, sak.mottatt_dato, sak.uttaksdato, sak.sak_avsluttet,",
                           "sak.hensiktkode, sak.eier_lokalitetstype, sak.eier_lokalitetnr",
                           "FROM v_innsendelse AS sak",
                           "INNER JOIN v_sakskonklusjon",
                           "ON (v_sakskonklusjon.aar = sak.aar AND",
                           "v_sakskonklusjon.ansvarlig_seksjon = sak.ansvarlig_seksjon AND",
                           "v_sakskonklusjon.innsendelsesnummer = sak.innsendelsesnummer)",
                           "WHERE sak.aar = 2020 AND ( sak.hensiktkode IN ('0200135', '0200142') )")

  expect_equivalent(query["selection_sakskonklusjon"], correct_result)

})



test_that("build query hensikt error testing", {

  expect_error(build_query_hensikt(hensikt = "0200102"),
               regexp = 'argument "year" is missing')

  expect_error(build_query_hensikt(year = 2020),
               regexp = 'argument "hensikt" is missing')

  expect_error(build_query_hensikt(year = (as.numeric(format(Sys.Date(), "%Y")) + 1), hensikt = "0200102"),
               regexp = "Variable 'year': Element ")

})
