library(NVIdb)
# library(reshape2)
# library(dplyr)

# Formål: Kobinere siste data for hver dyreart i én rapport uavhengig av vår og høst
# Design
# 1. identifisere siste og nest siste versjon av prodtilskudd
# 2. laste forklaringer
# 3. Finne siste og nest siste koder
# 4. Identifisere arter i nest siste som ikke er i siste
# 5. Inkludere alle koder for arter som mangler
#    I praksis bifolk om våren og sau om høsten, usikkert hva man gjør med geit.

ptdata <- read_Prodtilskudd(Pkode_year = "last", Pkode_month = "both")

# pkode201803 <- read_Prodtilskudd(Pkode_year = "2020", Pkode_month = "03")
# pkode201810 <- read_Prodtilskudd(Pkode_year = "2020", Pkode_month = "10")

combine = c("last", "all")

ptdata$Telledato <- as.Date(ptdata$Telledato)

counting_dates <- unique(ptdata$Telledato)

ptlast <- subset(ptdata, ptdata$Telledato == max(counting_dates))
ptcombine <- subset(ptdata, ptdata$Telledato == min(counting_dates))

Pkode_2_txt <-  read_Pkode_2_text(filename = "Produksjonstilskuddskoder2_UTF8.csv")
Pkode_2_txt$telledato <- as.Date(Pkode_2_txt$telledato)


pkoder <- subset(Pkode_2_txt, Pkode_2_txt$telledato %in% counting_dates &
                   Pkode_2_txt$enhet %in% c("Dyr", "Bifolk") &
                   Pkode_2_txt$unike_dyr == 1)
pkoder[which(pkoder$Pkode == "P139"), "art"] <- "Melkesau"

# arter <- unique(pkoder[, c("art", "telledato")])
# arter <- arter[which(arter$telledato == min(counting_dates) & !arter$art %in% arter[which(arter$telledato == max(counting_dates)), "art"]),]
# # part <- part[order(part$dato, decreasing = TRUE),]
#
# pkoder2 <- merge(pkoder, arter, by = c("art", "telledato"))

as_vector <- function(data, variable) {
  as.vector(data[, variable])
}

Pkode_2_select <- pkoder %>%
  poorman::select(c("art", "telledato")) %>%
  poorman::distinct() %>%
  poorman::filter(telledato == min(counting_dates) &
                    !art %in% pkoder[which(pkoder$telledato == max(counting_dates)), "art"]) %>%
  poorman::inner_join(pkoder, by = c("art", "telledato")) %>%
  as_vector("Pkode")

col <- colnames(ptcombine)
admcol <- col[which(!col %in% Pkode_2_txt$art & !col %in% Pkode_2_txt$Pkode & !col %in% paste0(Pkode_2_txt$art, "Prodtype"))]
selcol <- col[which(col %in% admcol | col %in% Pkode_2_select)]

ptcombine <- ptcombine[, selcol]
ptcombine$sum <- rowSums(ptcombine[, Pkode_2_select], na.rm = TRUE)
ptcombine <- ptcombine[which(ptcombine$sum > 0), selcol]


pt <- ptlast %>%
  poorman::bind_rows(ptcombine[which(!ptcombine$gjeldende_prodnr8 %in% ptlast$gjeldende_prodnr8), admcol]) %>%
  poorman::left_join(ptcombine[, c("gjeldende_prodnr8", "Orgnr", Pkode_2_select)], by = c("gjeldende_prodnr8", "Orgnr"))

art_2_select <- pkoder %>%
  poorman::select(c("art", "telledato")) %>%
  poorman::distinct() %>%
  poorman::filter(telledato == min(counting_dates) &
                    !art %in% pkoder[which(pkoder$telledato == max(counting_dates)), "art"]) %>%
  # poorman::inner_join(pkoder, by = c("art", "telledato")) %>%
  as_vector("art")

