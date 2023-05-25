# generate_PJS_code_description_colname

# GENERATE rda-file with the corresponding standard colnames for description to PJS code variables

# GENERATE DATA ----
PJS_code_description_colname <- as.data.frame(matrix(rbind(c("sakskonkl_kjennelsekode", "kjennelse", "sakskonkl_kjennelse"),
                                                           c("sakskonkl_analyttkode", "analytt", "sakskonkl_analytt"),
                                                           c("ansvarlig_seksjon", "seksjon", "ansvarlig_seksjon_navn"),
                                                           c("utf_seksjon", "seksjon", "utforende_seksjon_navn"),
                                                           c("hensiktkode", "hensikt", "hensikt"),
                                                           c("utbruddnr", "utbrudd", "utbrudd"),
                                                           c("rekvirenttype", "registertype", "rekvirenttype_navn"),
                                                           c("eier_lokalitettype", "registertype", "eier_lokalitettype_navn"),
                                                           c("annen_aktortype", "registertype", "annen_aktortype_navn"),
                                                           c("artkode", "art", "art"),
                                                           c("fysiologisk_stadiumkode", "fysiologisk_stadium", "fysiologisk_stadium"),
                                                           c("kjonn", "kjonn", "kjonn_navn"),
                                                           c("driftsformkode", "driftsform", "driftsform"),
                                                           c("oppstallingkode", "oppstalling", "oppstalling"),
                                                           c("provetypekode", "provetype", "provetype"),
                                                           c("provematerialekode", "provemateriale", "provemateriale"),
                                                           c("forbehandlingkode", "forbehandling", "forbehandling"),
                                                           c("konkl_typekode", "konkl_type", "konkl_type"),
                                                           c("konkl_kjennelsekode", "kjennelse", "konkl_kjennelse"),
                                                           c("konkl_analyttkode", "analytt", "konkl_analytt"),
                                                           c("metodekode", "metode", "metode"),
                                                           c("res_kjennelsekode", "kjennelse", "res_kjennelse"),
                                                           c("res_analyttkode", "analytt", "res_analytt"),
                                                           c("enhetkode", "enhet", "enhet"),
                                                           c("subund_metodekode", "metode", "submetode"),
                                                           c("subres_kjennelsekode", "kjennelse", "subres_kjennelse"),
                                                           c("subres_analyttkode", "analytt", "subres_analytt"),
                                                           c("subres_enhetkode", "enhet", "subres_enhet")),
                                                     ncol = 3,
                                                     dimnames = list(NULL, c("code_colname", "type", "new_column"))))

# SAVE IN PACKAGE DATA ----
usethis::use_data(name = PJS_code_description_colname, overwrite = TRUE, internal = FALSE)

# REMOVE DATA FROM ENVIONMENT TO AVOID CONFLICTS WHEN ATTACHING PACKAGE ----
rm(PJS_code_description_colname)
