library(dplyr)

data.path <- "Y:/iGB/Projects/AD4GD/Work-packages/WP6_berlin_pilot/r2q_erweiterung/GIS"
data.path_ABIMO_2025 <- "Y:/iGB/Projects/AMAREX/Work-packages/AP_4/ABIMO_Daten/results/berlin_2025"

isu.path <- "Y:/iGB/Projects/AD4GD/Work-packages/WP6_berlin_pilot/r2q_erweiterung/GIS/isu5_code_only"

#########Files einlesen-------------

#Tabelle mit ABIMO output einlesen
  table_ABIMO_out <- read.table(file.path(data.path, "isu5_2020_combined_ABIMO_out/results-rabimo.csv"),
                          sep = ";", dec = ".", as.is = TRUE, header = TRUE)
  my_classes <- sapply(table_ABIMO_out, class)
  my_classes[1] <- "character"
  table_ABIMO_out <- read.table(file.path(data.path, "isu5_2020_combined_ABIMO_out/results-rabimo.csv"),
                                sep = ";", dec = ".", as.is = TRUE, header = TRUE, colClasses = my_classes)
# neue ABIMO-Ergebnisse 2025 aus AMAREX einlesen
  table_ABIMO_2025_out <- foreign::read.dbf(file = file.path(data.path_ABIMO_2025, "results_2025.dbf"),
                                            as.is = TRUE)

#ABIMO_input einlesen
  ABIMO_in <- foreign::read.dbf(file = file.path(data.path, "isu5_2020_combined_ABIMO_in/isu5_2020_abimo_hyras9120_amarex.dbf"),
                                as.is = TRUE)

#Regenwassereinzugsgebiete
  RW_EZG <- foreign::read.dbf(file = file.path(data.path, "Regenwassereinzugsgebiete/RegenwasserEZG.dbf"),
                              as.is = TRUE)

#dbf mit isu-Basis einlesen
  isu_all <- foreign::read.dbf(file = file.path(isu.path, "isu5_2020_abimo_all_fl_CODE_only.dbf"), as.is = TRUE)

#Zuordnung OgRe-Typen
  Typ_to_OgReTyp <- read.csv2("C:/Users/dwicke/Documents/Work/OGRE/Data-Work packages/AP2_Frachtmodellierung/Flaechen_Volumen_final/Hilfsfiles/OgRe_Typen_def.csv", as.is = TRUE)

#dbf mit Seennamen
  Gewaesser <- foreign::read.dbf(file = file.path(data.path, "Seen_Code/lakes_with_code.dbf"),
                              as.is = TRUE)

#####dbf-File zusammenstellen----------------------

#Code # eindeutige Kennung aller Blockteil- und Strassenflaechen
  result_dbf <- isu_all

#Gewaesser # Name Gewaessereinzugsgebiete (nur im Trenngebiet und nur für angeschlossene vers. Flaechen)
  index <- match(result_dbf$CODE, RW_EZG$schl5)

  result_dbf$EAG <- RW_EZG$eag[index]
  result_dbf$EAG_nr <- RW_EZG$eag_nr[index]

# Hinzufügen der Spalten ageb1" (EZG Regenkanalisation - Gebiete), "ageb1_nr", "bereich", "bereich_nr" zu result_dbf
  result_dbf <- result_dbf %>%
    dplyr::left_join(y = RW_EZG %>%
                       dplyr::select(schl5, ageb1, ageb1_nr, bereich, bereich_nr),
                     by = c("CODE" = "schl5")
  )

#Kanalart # misch oder trennkanal
  result_dbf$kanart_kla <- RW_EZG$kanart_kla[index]

#Flaeche # Flaeche der Gesamtflaeche in m2
  index <- match(result_dbf$CODE, ABIMO_in$code)

  result_dbf$area_tot <- ABIMO_in$flges[index]

#ang. vers. Flaeche bebaut (Geabaeude/Dachflaeche) in m2

  result_dbf$AU_roof <- ABIMO_in$flges[index] * ABIMO_in$probau[index]/100 * ABIMO_in$kan_beb[index]/100

#ang. vers. Flaeche unbebaut (Hof) in m2

  result_dbf$AU_yard <- ABIMO_in$flges[index] * ABIMO_in$provgu[index]/100 * ABIMO_in$kan_vgu[index]/100

  result_dbf$art <- ABIMO_in$art[index]

  index_yard <- which(is.na(result_dbf$art))

  result_dbf$AU_yard[-index_yard] <- NA

#ang. vers. Flaeche unbebaut (Strasse) in m2

  result_dbf$AU_road <- ABIMO_in$flges[index] * ABIMO_in$provgu[index]/100 * ABIMO_in$kan_str[index]/100

  result_dbf$AU_road[index_yard] <- NA

#  result_dbf <- result_dbf[,-8] (ist glaube ich Versehen)

#runoff per year in mm (multiply with area_tot -> L/m2/yr * m2 = L/yr)

#  index <- match(result_dbf$CODE, table_ABIMO_out$CODE)
#  result_dbf$ROW <- table_ABIMO_out$ROW[index]

  index <- match(result_dbf$CODE, table_ABIMO_2025_out$code)
  result_dbf$ROW <- table_ABIMO_2025_out$runoff[index]

#infiltration (permanent) per year in mm (multiply with area_tot -> L/m2/yr * m2 = L/yr)

#  result_dbf$RI <- table_ABIMO_out$RI[index]
  result_dbf$RI <- table_ABIMO_2025_out$infiltr[index]

#evaporation/evapotranspiration per year in mm (multiply with area_tot -> L/m2/yr * m2 = L/yr)

#  result_dbf$evap <- table_ABIMO_out$VERDUNSTUN[index]
  result_dbf$evap <- table_ABIMO_2025_out$evapor[index]

#Zuordnung OgRe-Typen
  result_dbf$OGRE_TYP_KLAR <- NA
  result_dbf$OGRE_TYP_KLAR[-index_yard] <- "STR"

  index <- match(result_dbf$CODE, ABIMO_in$code)
  result_dbf$FLAECHENTYP <- ABIMO_in$typ[index]

  for(i in seq_along(Typ_to_OgReTyp$Flaechentyp)) {
      result_dbf$OGRE_TYP_KLAR[which(result_dbf$FLAECHENTYP == Typ_to_OgReTyp$Flaechentyp[i])] <- Typ_to_OgReTyp$OgRe_Typ_klar[i]
  }

  result_dbf <- result_dbf[,-which(colnames(result_dbf) == "FLAECHENTYP")]

#Gewaessernamen fuer stehende Gewasser selbst (falls eigene "Blockteilflaeche")
  index <- match(result_dbf$CODE, Gewaesser$CODE)

  result_dbf$Gew_name <- Gewaesser$GEW_NAME[index]


#Reihenfolge wie in isu-dbf
index <- match(isu_all$CODE, result_dbf$CODE)
result_dbf <- result_dbf[index,]


# Berechnung runoff-coefficient für Gebiete (ageb1)

runoff_coeff_ageb1 <- result_dbf %>%
  dplyr::mutate(ROW_m3 = ROW * area_tot / 1000, RI_m3 = RI * area_tot / 1000, evap_m3 = evap * area_tot / 1000) %>%
  dplyr::group_by(ageb1, ageb1_nr) %>%
  dplyr::summarise(runoff_coefficient = sum(ROW_m3, na.rm = TRUE) / sum(ROW_m3 + RI_m3 + evap_m3, na.rm = TRUE),
                   sum_row_m3 = sum(ROW *  area_tot, na.rm = TRUE) / 1000,
                   area_tot_m2 = sum(area_tot, na.rm = TRUE)
  )

readr::write_csv(runoff_coeff_ageb1, file = "runoff_coeff_ageb1_ABIMO_2025.csv")

# Berechnung runoff-coefficient für alle EZG Regenkanalisation differenziert (EAG)

runoff_coeff_eag_ageb1 <- result_dbf %>%
  dplyr::mutate(ROW_m3 = ROW * area_tot / 1000, RI_m3 = RI * area_tot / 1000, evap_m3 = evap * area_tot / 1000) %>%
  dplyr::group_by(ageb1, EAG, EAG_nr) %>%
  dplyr::summarise(runoff_coefficient = sum(ROW_m3, na.rm = TRUE) / sum(ROW_m3 + RI_m3 + evap_m3, na.rm = TRUE),
                   sum_row_m3 = sum(ROW *  area_tot, na.rm = TRUE) / 1000,
                   area_tot_m2 = sum(area_tot, na.rm = TRUE)
  )

readr::write_csv(runoff_coeff_eag_ageb1 , file = "runoff_coeff_eag_ageb1_ABIMO_2025.csv")

# Abspeichern der result_dbf
foreign::write.dbf(file = file.path(data.path, "/Zusammenstellung/Zusammenstellung.dbf"), dataframe = result_dbf)
