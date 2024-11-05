
rm(list = ls())


 
### Pakete
## um SPSS-Datei einzulesen
#install.packages("haven")
#install.packages("rlang")
#install.packages("dplyr")
#install.packages("tidyr")
library(haven)
library(rlang)
library(dplyr)
library(tidyr)
### Für Label-Funktionen
#install.packages("tidyverse")
#install.packages("here")
#install.packages("sjlabelled")
#install.packages("labelled")
#install.packages("car")
#install.packages("lavaan", dependencies = TRUE)
library(car)
library(tidyverse)  # general use
library(here)       # file paths
library(labelled)   # tools for labelled data
library(sjlabelled) # more tools for labelled data
library(lavaan)
library(psych)


setwd("C:\\Users\\kearu\\Desktop\\Uni gönnung\\BA\\2 Datenanalyse")
#getwd()


### Einlesen der SPSS-Daten
##  Kadur; endgültige SP von N = 413 (und nicht mehr N = 434 vollausgefüllte Fragebögen)
Kadur_413 <- haven::read_sav("C:\\Users\\kearu\\Desktop\\Uni gönnung\\BA\\2 Datenanalyse\\Daten von Frau Strobel\\05_Daten\\Rohdaten\\NFC-Pflege\\NFC_Pflege_15.10.18_Kadur.sav")
## Ziessler; endgültige SP von N = 229 (und nicht mehr N = 256 vollausgefüllte Fragebögen)
Ziessler_229 <- haven::read_sav("C:\\Users\\kearu\\Desktop\\Uni gönnung\\BA\\2 Datenanalyse\\Daten von Frau Strobel\\05_Daten\\Rohdaten\\NFC-Helfer\\BA_Ziessler\\24.09.2019_Aktueller Datensatz.sav")

 
 
### Schritt 1: Spaltennamen jedes Dataframes durchnummerieren und "K" / "Z" voranstellen
for(i in 1:ncol(Kadur_413)) {colnames(Kadur_413)[i] <- paste0("K", "_", i, "_", colnames(Kadur_413)[i])}
for(i in 1:ncol(Ziessler_229)) {colnames(Ziessler_229)[i] <- paste0("Z", "_", i, "_", colnames(Ziessler_229)[i])}


### Schritt 2: Entfernen der unerwünschten Spalten
#Kadur_413 <- Kadur_413 %>% select(-c(1:5, 8, 10, 12:30, 32, 34:36, 38:73, 96:107, 147:158, 179, 183, 184, 185, 191, 197:207))
#Ziessler_229 <- Ziessler_229 %>% select(-c(1, 2, 20:53, 113:122, 145, 146, 157, 159, 160, 172:267))

## Berufsspalten
#Kadur_413 <- Kadur_413 %>% select(-c(1:5, 8, 10, 12:21, 23:30, 32, 34, 38:73, 96:107, 147:158, 179, 183, 184, 185, 191, 197:207))
#Ziessler_229 <- Ziessler_229 %>% select(-c(1, 2, 9, 10, 11, 14:53, 113:122, 145, 146, 157, 159, 160, 172:267))


### Schritt 3: Spalte mit dem Dataframenamen hinzufügen
## bisschen unnötig, kann später wieder entfernt werden
#Kadur_413$DF_name <- "Kadur_413"
#Ziessler_229$DF_name <- "Ziessler_229"
## Entferne die Spalte mit dem Dataframename
# merged_df$DF_name <- NULL


 
### Schritt 4: Dataframes zusammenführen
ZK_merged <- bind_rows(Kadur_413, Ziessler_229)



### Schritt 5: Merge those columns that have the same name/content
## 5.1 subject_code
ZK_merged <- ZK_merged %>% unite(subject_code, K_6_tn_code, Z_3_tn_code, sep = "")
## "NA" entfernen
ZK_merged$subject_code <- gsub("NA", "", ZK_merged$subject_code)



## 5.2 sex 
# --> Checken, ob Codierung für Ziessler und Kadur übereinstimmen für männlich und weiblich
# Kadur; 1 = weiblich (n=360), 2 = männlich (n=52), 3 = intersexuell
# sum(Kadur_413$K_7_geschl == 1)
# sum(Kadur_413$K_7_geschl == 2)
# sum(Kadur_413$K_7_geschl == 3)

# Ziessler; 1 = weiblich (n=187), 2 = männlich (n=42)
# sum(Ziessler_229$Z_4_geschl == 1)
# sum(Ziessler_229$Z_4_geschl == 2)

# ZK_merged Geschlechter zählen 1 = weiblich (n=574), 2 = männlich (n=94), 3 = intersexuell (n=1)
# sum(ZK_merged$sex == 1)
# sum(ZK_merged$sex == 2)
# sum(ZK_merged$sex == 3)

ZK_merged <- ZK_merged %>% unite(sex, K_7_geschl, Z_4_geschl, sep = "")
ZK_merged$sex <- gsub("NA", "", ZK_merged$sex)

# Assign new labels for column "sex"
ZK_merged$sex <- labelled(as.numeric(factor(ZK_merged$sex)), labels = c(female=1, male=2, divers=3))

ZK_merged$female <- ifelse(ZK_merged$sex %in% c(1), 1, 0)
ZK_merged$male <- ifelse(ZK_merged$sex %in% c(2), 1, 0)
ZK_merged$divers <- ifelse(ZK_merged$sex %in% c(3), 1, 0)

ZK_merged$sex
table(ZK_merged$female)
table(ZK_merged$male)
table(ZK_merged$divers)



## 5.3 age
ZK_merged <- ZK_merged %>% unite(age, K_9_alter, Z_5_alter, sep = "")
ZK_merged$age <- gsub("NA", "", ZK_merged$age)

ZK_merged$age <- as.numeric(ZK_merged$age)

#ZK_merged$age <- as.factor(ZK_merged$age)
#ZK_merged$age
#table(ZK_merged$age)
#class(ZK_merged$age)



## 5.4 federal state
# Check if the codes for federal state match
# Kadur; 1=Baden-Württemberg, 2=Bayern, 3=Berlin, 4=Brandenburg, 5=Bremen, 6=Hamburg, 7=Hessen, 8=Mecklenburg-Vorpommern, 9=Niedersachsen, 10=Nordrhein-Westfalen, 11=Rheinland-Pfalz, 12=Saarland, 13=Sachsen, 14=Sachsen-Anhalt, 15=Schleswig-Holstein, 16=Thüringen
# Kadur_413$K_11_bula
# Ziessler; 1=Baden-Württemberg, 2=Bayern, 7=Hessen, 9=Niedersachsen, 10=Nordrhein-Westfalen, 11=Rheinland-Pfalz, 12=Saarland, 13=Sachsen, 14=Sachsen-Anhalt
# Ziessler_229$Z_6_Bundesland
# 1=51, 2=74, 3=12, 4=6, 5=4, 6=11, 7=135, 8=6, 9=47, 10=107, 11=37, 12=1, 13=126, 14=6, 15=8, 16=11

#ZK_merged$K_11_bula <- factor(ZK_merged$K_11_bula, levels = 1:16, labels = c(1:16))
ZK_merged <- ZK_merged %>% unite(federal_state, K_11_bula, Z_6_Bundesland, sep = "")
ZK_merged$federal_state <- gsub("NA", "", ZK_merged$federal_state)
ZK_merged$federal_state <- factor(ZK_merged$federal_state, levels = 1:16, labels = c(1:16))

# Neue Label für zusammen gefasste Spalte "federal_state" vergeben
ZK_merged$federal_state <- labelled(as.numeric(factor(ZK_merged$federal_state)), labels = c("Baden-Württemberg"=1, "Bayern"=2, "Berlin"=3, "Brandenburg"=4, "Bremen"=5, "Hamburg"=6, "Hessen"=7, "Mecklenburg-Vorpommern"=8, "Niedersachsen"=9, "Nordrhein-Westfalen"=10, "Rheinland-Pfalz"=11, "Saarland"=12, "Sachsen"=13, "Sachsen-Anhalt"=14, "Schleswig-Holstein"=15, "Thüringen"=16))

# NOTE: For optimal analysis, federal state is divided into "East" and "West"
ZK_merged$EastGermany <- ifelse(ZK_merged$federal_state %in% c(3, 4, 8, 13, 14, 16), 1, 0)
ZK_merged$WestGermany <- ifelse(ZK_merged$federal_state %in% c(1, 2, 5, 6, 7, 9, 10, 11, 12, 15), 1, 0)

#table(ZK_merged$EastGermany)
#table(ZK_merged$WestGermany)



## 5.5 Amount of children (in the household)
# Kadur; K_33_kinder (1-5)
# Ziessler; Z_8_Kinder (1-4)
# 1=no children (417), 2=1 (107), 3=2 (92), 4=3 (20), 5=>3 (6)

ZK_merged <- ZK_merged %>% unite(children_household, K_33_kinder, Z_8_Kinder, sep = "")
ZK_merged$children_household <- gsub("NA", "", ZK_merged$children_household)

ZK_merged$children_household <- labelled(as.numeric(factor(ZK_merged$children_household)), labels = c("no children"=1, "1"=2, "2"=3, "3"=4, "more than 3 children"=5))

# NOTE: For optimal analysis, children_household is divided into "No children" and "Children"
ZK_merged$NoChildren <- ifelse(ZK_merged$children_household %in% c(1), 1, 0)
ZK_merged$Children <- ifelse(ZK_merged$children_household %in% c(2, 3, 4, 5), 1, 0)



## 5.6 marital status
# Kadur; K_31_bstatus -> 1=single, 2=in einer Partnerschaft, 3=verheiratet, 4=geschieden, 5=verwitwet
#table(ZK_merged$K_31_bstatus)
# Ziessler; Z_7_Beziehungssstatus -> 1=ledig, 2=In einer Beziehung lebend, 3=verheiratet mit Partner zusammen lebend, 4=Verheiratet, getrennt lebend, 5=Geschieden, 6=verwitwet
#table(ZK_merged$Z_7_Beziehungssstatus)

# Bei Ziessler 3+4 aggreriren zu 3, dann alle Werte 5 zu 4 machen und alle Werte 6 zu 5 machen
ZK_merged$Z_7_Beziehungssstatus <- ifelse(ZK_merged$Z_7_Beziehungssstatus %in% c(3, 4), 3, ZK_merged$Z_7_Beziehungssstatus) # aggregiere 3 und 4 zu 3
ZK_merged$Z_7_Beziehungssstatus <- ifelse(ZK_merged$Z_7_Beziehungssstatus == 5, 4, ZK_merged$Z_7_Beziehungssstatus) # ersetze 5 durch 4
ZK_merged$Z_7_Beziehungssstatus <- ifelse(ZK_merged$Z_7_Beziehungssstatus == 6, 5, ZK_merged$Z_7_Beziehungssstatus) # ersetze 6 durch 5

# Neue Werte; 1=single(95), 2=in a partnership(245), 3=married(260), 4=divorced(30), 5=widowed(12)
# 1=95, 2=245, 3=260, 4=30, 5=12
ZK_merged <- ZK_merged %>% unite(marital_status, K_31_bstatus, Z_7_Beziehungssstatus, sep = "")
ZK_merged$marital_status <- gsub("NA", "", ZK_merged$marital_status)

ZK_merged$marital_status <- labelled(as.numeric(factor(ZK_merged$marital_status)), labels = c("single"=1, "in a partnership"=2, "married"=3, "divorced"=4, "widowed"=5))

# NOTE: For optimal analysis, marital_status is divided into "not married" and "married"
ZK_merged$NotMarried <- ifelse(ZK_merged$marital_status %in% c(1, 2, 4, 5), 1, 0)
ZK_merged$Married <- ifelse(ZK_merged$marital_status %in% c(3), 1, 0)

table(ZK_merged$NotMarried)



## 5.7 profession
# Kadur; K_37_POSITION (1=Azubi, 2=Pflegeherlfer/in oder Assistant/in,	3=Pflegefachkraft,	4= Pflegefachkraft , stellvertretende Leitungsfunktion,	5=andere Position)
# Ziessler; Z_12_berufl_pos, Z_13_anderer_Beruf
#print(ZK_merged %>% select(Z_12_berufl_pos, Z_13_anderer_Beruf), n = Inf)

#ZK_merged$K_36_pos_and_text
#ZK_merged$K_37_POSITION
#ZK_merged$K_22_AUSBILDUNG
#ZK_merged$K_13_ba_keine

#Ziessler_229$Z_9_Bildung
#Ziessler_229$Z_10_AZUBI
#Ziessler_229$Z_11_v_325
#ZK_merged$Z_12_berufl_pos
#Ziessler_229$Z_13_anderer_Beruf

#any(is.na(ZK_merged$K_37_POSITION))
#table(ZK_merged$K_22_AUSBILDUNG)


#Kontroll_dataframe_ZK <- ZK_merged %>%  select(subject_code, Z_12_berufl_pos, Z_10_AZUBI, Z_9_Bildung, Z_11_v_325, Z_13_anderer_Beruf, Z_19_v_332, K_22_AUSBILDUNG, K_36_pos_and_text, K_37_POSITION, profession)
#Kontroll_dataframe_ZK <- ZK_merged %>%  select(subject_code, Z_12_berufl_pos, K_22_AUSBILDUNG, K_37_POSITION, profession)



## 1. Spalte "profession" in ZK_merged erstellen
# Vektor mit den Kategorien und Labels erstellen
profession_labels <- c("Nurse" =1, "Physician"=2, "Psychologist, Child and Adolescent Psychotherapist / Psychological Psychotherapist" =3, 
                      "Social worker/pedagogue"=4, "Chaplain/Deacon"=5, "Management/Coordination"=6,
                      "Other therapeutic professions"=7, "Assistant professions"=8, "Caring/supporting professions"=9, "Other positions"=10,
                      "Persons in training or study"=11)

# old labels/categories (12): profession_labels <- c("Nurse" =1, "Physician"=2, "Psychologist, Child and Adolescent Psychotherapist / Psychological Psychotherapist" =3, "Social worker/pedagogue"=4, "Chaplain"=5, "Deaconess/Deacon"=6, "Management/Coordination"=7,"Other therapeutic professions"=8, "Assistant professions"=9, "Caring/supporting professions"=10, "Other positions"=11,"Persons in training or study"=12)

# Neue Spalte 'profession' im Datensatz 'ZK_merged' erstellen und mit NA-Werten initialisieren
ZK_merged$profession <- NA

# Neue Spalte 'profession' im Datensatz 'ZK_merged' erstellen und Labels zuordnen
levels(ZK_merged$profession) <- profession_labels



## 2. VPn aus Z_12_berufl_pos und K_37_POSITION zu entsprechenden Spalten in profession zuordnen
# Ziessler
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(1, 2)] <- 1
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(3)] <- 2
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(4, 5, 6)] <- 3
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(7, 8)] <- 4
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(10, 11)] <- 5
#ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(11)] <- 6
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(12)] <- 6
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(9, 13)] <- 7
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(14)] <- 8
ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(15)] <- 9
#ZK_merged$profession[ZK_merged$K_37_POSITION %in% c(x)] <- 11
#ZK_merged$profession[ZK_merged$Z_12_berufl_pos %in% c(x)] <- 12

# Kadur
ZK_merged$profession[ZK_merged$K_37_POSITION %in% c(0)] <- 11
ZK_merged$profession[ZK_merged$K_37_POSITION %in% c(1)] <- 8
ZK_merged$profession[ZK_merged$K_37_POSITION %in% c(2)] <- 1
ZK_merged$profession[ZK_merged$K_37_POSITION %in% c(3, 4)] <- 6



## 3. Uneindeutige VPn aus Datensätzen umordnen
# subject_code 'AH2611' Wert '10' in der Spalte 'ZK_merged$Z_12_berufl_pos' statt zu '5' zu '6' in 'ZK_merged$profession' zuordnen
ZK_merged$profession[ZK_merged$subject_code == 'AH2611' & ZK_merged$Z_12_berufl_pos == 10] <- '5'

# GK23 (aus K_37_POSITION 5=andere Position) zu 7 (Management/Coordination) zuordnen
ZK_merged$profession[ZK_merged$subject_code == 'GK23' & ZK_merged$K_37_POSITION == 5] <- '6'

# AL1009 (pädagogische Fachkraft) aus 5 (andere) in K_37_POSITION zu 10 (Caring/supporting professions)
ZK_merged$profession[ZK_merged$subject_code == 'AL1009' & ZK_merged$K_37_POSITION == 5] <- '9'

# BS0808 (pädagogische Fachkraft) aus 5 (andere) in K_37_POSITION zu 10 (Caring/supporting professions)
ZK_merged$profession[ZK_merged$subject_code == 'BS0808' & ZK_merged$K_37_POSITION == 5] <- '9'

# DD1002 (pädagogische Fachkraft) aus 5 (andere) in K_37_POSITION zu 10 (Caring/supporting professions)
ZK_merged$profession[ZK_merged$subject_code == 'DD1002' & ZK_merged$K_37_POSITION == 5] <- '9'

# IA2408 (pädagogische Fachkraft) aus 5 (andere) in K_37_POSITION zu 10 (Caring/supporting professions)
ZK_merged$profession[ZK_merged$subject_code == 'IA2408' & ZK_merged$K_37_POSITION == 5] <- '9'

# KH1010 Heilerziehungspflegerin) aus 5 (andere) in K_37_POSITION zu 10 (Caring/supporting professions)
ZK_merged$profession[ZK_merged$subject_code == 'KH1010' & ZK_merged$K_37_POSITION == 5] <- '9'

# KG1203 (Bundesfreiwilligendienst; aus K_37_POSITION 5=andere Position bzw. K_36_pos_and_text) zu 11 (Other positions)
ZK_merged$profession[ZK_merged$subject_code == 'KG1203' & ZK_merged$K_37_POSITION == 5] <- '10'

# AD0903 (keine Ausbildung abgeschlossen, aber auch nicht in Ausbildung; aus K_37_POSITION 5=andere Position) zu 11 (Other positions)
ZK_merged$profession[ZK_merged$subject_code == 'AD0903' & ZK_merged$K_37_POSITION == 5] <- '10'

# MB1503 (Pflegefachkraft, aber selbstständig und nicht angestellt) zu 11
ZK_merged$profession[ZK_merged$subject_code == 'MB1503' & ZK_merged$K_37_POSITION == 5] <- '10'

# ML0510 (Pflegeexperte) zu 11
ZK_merged$profession[ZK_merged$subject_code == 'ML0510' & ZK_merged$K_37_POSITION == 5] <- '10'

# mb0704 (Stabstelle) zu 11
ZK_merged$profession[ZK_merged$subject_code == 'mb0704' & ZK_merged$K_37_POSITION == 5] <- '10'

# ML1007 (Stabstelle) zu 11
ZK_merged$profession[ZK_merged$subject_code == 'ML1007' & ZK_merged$K_37_POSITION == 5] <- '10'

# BB1802 (Retnerin, nebenberufl. Tätig in Ehrenamt) zu 11
ZK_merged$profession[ZK_merged$subject_code == 'BB1802' & ZK_merged$Z_12_berufl_pos == 14] <- '10'

# AG2809 (aus Ziessler; Kinderkrankenschwester, Fachhochschulreife/Abitur -- aktuell Wert 12 in der Spalte 'ZK_merged$Z_12_berufl_pos' statt zu 12 
# (Management/Coordination) entfernen und "Persons in training or study"=12
ZK_merged$profession[ZK_merged$subject_code == 'AG2809' & ZK_merged$Z_12_berufl_pos == 12] <- '11'

# MF2705 (Azubi) zu 12 (Persons in training or study)
ZK_merged$profession[ZK_merged$subject_code == 'MF2705' & ZK_merged$Z_12_berufl_pos == 7] <- '11'

# SN1603 (Azubi) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'SN1603' & ZK_merged$Z_12_berufl_pos == 1] <- '11'

# BD3008 (Fachhochschulreife/Abitur, Altenpflege = Studium aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'BD3008' & ZK_merged$Z_12_berufl_pos == 1] <- '11'

# KM2903 (Studium aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'KM2903' & ZK_merged$Z_12_berufl_pos == 1] <- '11'

# HL1306 (Ausbildung Ergotherapie aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'HL1306' & ZK_merged$Z_12_berufl_pos == 8] <- '11'

# JC1005 (Studium aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'JC1005' & ZK_merged$Z_12_berufl_pos == 9] <- '11'

# BG0510 (Ausbildung Physiotherapie aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'BG0510' & ZK_merged$Z_12_berufl_pos == 13] <- '11'

# AD1806 (Studium aktuell) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'AD1806' & ZK_merged$Z_12_berufl_pos == 13] <- '11'

#	ZD14 (Heilpraktiker/in in Ausbildung) zu 12
ZK_merged$profession[ZK_merged$subject_code == 'ZD14' & ZK_merged$Z_12_berufl_pos == 14] <- '11'



## 4. Label für entsprechenden Werte verteieln 
ZK_merged$profession <- labelled(as.numeric(ZK_merged$profession), labels = c("Nurse" =1, "Physician"=2, "Psychologist, Child and Adolescent Psychotherapist / Psychological Psychotherapist" =3, 
                                                                              "Social worker/pedagogue"=4, "Chaplain/Deacon"=5, "Management/Coordination"=6,
                                                                              "Other therapeutic professions"=7, "Assistant professions"=8, "Caring/supporting professions"=9, "Other positions"=10,
                                                                              "Persons in training or study"=11))
ZK_merged$profession <- set_label(ZK_merged$profession, "Profession")



## 5. NOTE: For optimal analysis, profession is divided into...
#          1=medical stuff (category 1, 2, some of 7, 8), psychological, social and caring professions (category 3, 4, 5, 9), Other therapeutic/caring/assistant professions (some of 7) 
#          2=Management / Coordination (category 6),
#          3=Volunteers/Persons in education (category 10, 11)
ZK_merged$TherapeuticProfessions <- ifelse(ZK_merged$profession %in% c(1,2,3,4,5,7,8,9), 1, 0)
ZK_merged$ManagementProfessions <- ifelse(ZK_merged$profession %in% c(6), 1, 0)
ZK_merged$Volunteers_PersonsinEducation <- ifelse(ZK_merged$profession %in% c(10,11), 1, 0)

ZK_merged$ProfessionCategories <- 0
ZK_merged$ProfessionCategories[ZK_merged$TherapeuticProfessions == 1] <- 1
ZK_merged$ProfessionCategories[ZK_merged$ManagementProfessions == 1] <- 2
ZK_merged$ProfessionCategories[ZK_merged$Volunteers_PersonsinEducation == 1] <- 3



######################################### 5.8 burnout subscales [= 22 items] (D [=5], EE [=9], [r]PE [=8])
# EE1: "Ich fühle mich durch meine Arbeit ausgebrannt"
# K_74_mbi_ee01
# Z_123_mbi_ee01
#print_labels(ZK_merged$K_74_mbi_ee01)
##var_labels(ZK_merged$K_74_mbi_ee01)
##var_labels(ZK_merged$Z_123_mbi_ee01)
##var_labels(ZK_merged$mbi_ee01)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee01, K_74_mbi_ee01, Z_123_mbi_ee01, sep = "") %>%
  mutate(mbi_ee01 = gsub("NA", "", mbi_ee01)) %>%
  mutate(mbi_ee01 = labelled(as.numeric(factor(mbi_ee01)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee01 <- set_label(ZK_merged$mbi_ee01, "Ich fühle mich durch meine Arbeit ausgebrannt.")


# EE2: "Der direkte Kontakt mit Menschen bei meiner Arbeit belastet mich zu stark."
# K_75_mbi_ee02
# Z_124_mbi_ee02
var_labels(Kadur_413$K_75_mbi_ee02)
##var_labels(ZK_merged$Z_124_mbi_ee02)
##var_labels(ZK_merged$mbi_ee02)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee02, K_75_mbi_ee02, Z_124_mbi_ee02, sep = "") %>%
  mutate(mbi_ee02 = gsub("NA", "", mbi_ee02)) %>%
  mutate(mbi_ee02 = labelled(as.numeric(factor(mbi_ee02)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee02 <- set_label(ZK_merged$mbi_ee02, "Der direkte Kontakt mit Menschen bei meiner Arbeit belastet mich zu stark.")


# EE3: "Den ganzen Tag mit Menschen zu arbeiten, ist für mich wirklich anstrengend."
# K_76_mbi_ee03
# Z_125_mbi_ee03
#var_label(Kadur_413$K_76_mbi_ee03)
##var_labels(ZK_merged$K_76_mbi_ee03)
##var_labels(ZK_merged$Z_125_mbi_ee03)
#print_labels(ZK_merged$K_74_mbi_ee01)
##var_labels(ZK_merged$mbi_ee03)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee03, K_76_mbi_ee03, Z_125_mbi_ee03, sep = "") %>%
  mutate(mbi_ee03 = gsub("NA", "", mbi_ee03)) %>%
  mutate(mbi_ee03 = labelled(as.numeric(factor(mbi_ee03)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee03 <- set_label(ZK_merged$mbi_ee03, "Den ganzen Tag mit Menschen zu arbeiten, ist für mich wirklich anstrengend.")


# EE4: "Ich fühle mich durch meine Arbeit emotional erschöpft."
#Kadur_413$K_78_mbi_ee04
#Ziessler_$Z_126_mbi_ee04
##var_label(ZK_merged$K_77_mbi_de01)
#var_label(ZK_merged$K_78_mbi_ee04)
#var_label(Ziessler_229$Z_127_mbi_ee04)
##var_labels(ZK_merged$K_78_mbi_ee04)
#var_labels(Ziessler_229$Z_127_mbi_ee04)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee04,K_78_mbi_ee04, Z_127_mbi_ee04, sep = "") %>%
  mutate(mbi_ee04 = gsub("NA", "", mbi_ee04)) %>%
  mutate(mbi_ee04 = labelled(as.numeric(factor(mbi_ee04)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee04 <- set_label(ZK_merged$mbi_ee04, "Ich fühle mich durch meine Arbeit emotional erschöpft.")


# EE5: "Ich glaube, dass ich nicht mehr weiter weiß."
#Kadur_413$K_81_mbi_ee05
#Ziessler_$Z_130_mbi_ee05
#var_label(ZK_merged$K_81_mbi_ee05)
#var_label(Ziessler_229$Z_130_mbi_ee05)
###var_labels(ZK_merged$K_81_mbi_ee05)
#var_labels(Ziessler_229$Z_130_mbi_ee05)
##var_labels(ZK_merged$mbi_ee05)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee05,K_81_mbi_ee05, Z_130_mbi_ee05, sep = "") %>%
  mutate(mbi_ee05 = gsub("NA", "", mbi_ee04)) %>%
  mutate(mbi_ee05 = labelled(as.numeric(factor(mbi_ee05)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee05 <- set_label(ZK_merged$mbi_ee05, "Ich glaube, dass ich nicht mehr weiter weiß.")


# EE6: "Ich glaube, dass ich nicht mehr weiter weiß."
#Kadur_413$K_83_mbi_ee06
#Ziessler_$Z_132_mbi_ee06
var_label(ZK_merged$K_83_mbi_ee06)
var_label(Ziessler_229$Z_132_mbi_ee06)
###var_labels(ZK_merged$K_83_mbi_ee06)
#var_labels(Ziessler_229$Z_132_mbi_ee06)
##var_labels(ZK_merged$mbi_ee06)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee06, K_83_mbi_ee06, Z_132_mbi_ee06, sep = "") %>%
  mutate(mbi_ee06 = gsub("NA", "", mbi_ee06)) %>%
  mutate(mbi_ee06 = labelled(as.numeric(factor(mbi_ee06)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee06 <- set_label(ZK_merged$mbi_ee06, "Am Ende eines Arbeitstages fühle ich mich verbraucht.")


# EE7: "Ich glaube, dass ich nicht mehr weiter weiß."
#Kadur_413$K_85_mbi_ee07
#Ziessler_$Z_134_mbi_ee07
var_label(ZK_merged$K_85_mbi_ee07)
var_label(Ziessler_229$Z_134_mbi_ee07)
###var_labels(ZK_merged$K_85_mbi_ee07)
#var_labels(Ziessler_229$Z_134_mbi_ee07)
##var_labels(ZK_merged$mbi_ee07)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee07, K_85_mbi_ee07, Z_134_mbi_ee07, sep = "") %>%
  mutate(mbi_ee07 = gsub("NA", "", mbi_ee07)) %>%
  mutate(mbi_ee07 = labelled(as.numeric(factor(mbi_ee07)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee07 <- set_label(ZK_merged$mbi_ee07, "Ich fühle mich wieder müde, wenn ich morgens aufstehe und den nächsten Arbeitstag vor mir habe.")


# EE8: "Ich habe das Gefühl, dass ich an meinem Arbeitsplatz zu hart arbeite."
#Kadur_413$K_88_mbi_ee08
#Ziessler_$Z_137_mbi_ee08
var_label(ZK_merged$K_88_mbi_ee08)
var_label(Ziessler_229$Z_137_mbi_ee08)
###var_labels(ZK_merged$K_88_mbi_ee08)
#var_labels(Ziessler_229$Z_137_mbi_ee08)
##var_labels(ZK_merged$mbi_ee08)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee08, K_88_mbi_ee08, Z_137_mbi_ee08, sep = "") %>%
  mutate(mbi_ee08 = gsub("NA", "", mbi_ee08)) %>%
  mutate(mbi_ee08 = labelled(as.numeric(factor(mbi_ee08)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee08 <- set_label(ZK_merged$mbi_ee08, "Ich habe das Gefühl, dass ich an meinem Arbeitsplatz zu hart arbeite.")


# EE9: "Ich fühle mich durch meine Arbeit frustriert."
#Kadur_413$K_89_mbi_ee09
#Ziessler_$Z_138_mbi_ee09
var_label(ZK_merged$K_89_mbi_ee09)
var_label(Ziessler_229$Z_138_mbi_ee09)
###var_labels(ZK_merged$K_89_mbi_ee09)
#var_labels(Ziessler_229$Z_138_mbi_ee09)
##var_labels(ZK_merged$mbi_ee09)

ZK_merged <- ZK_merged %>%
  unite(mbi_ee09, K_89_mbi_ee09, Z_138_mbi_ee09, sep = "") %>%
  mutate(mbi_ee09 = gsub("NA", "", mbi_ee09)) %>%
  mutate(mbi_ee09 = labelled(as.numeric(factor(mbi_ee09)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_ee09 <- set_label(ZK_merged$mbi_ee09, "Ich fühle mich durch meine Arbeit frustriert.")



# D1: "Ich glaube, dass ich manche Patienten so behandle, als wären sie unpersönliche ^Objekte^."
#Kadur_413$K_77_mbi_de01
#Ziessler_$Z_126_mbi_de01
var_label(ZK_merged$K_77_mbi_de01)
var_label(Ziessler_229$Z_126_mbi_de01)
##var_labels(ZK_merged$K_77_mbi_de01)
var_labels(Ziessler_229$Z_126_mbi_de01)
##var_labels(ZK_merged$mbi_de01)

ZK_merged <- ZK_merged %>%
  unite(mbi_de01, K_77_mbi_de01, Z_126_mbi_de01, sep = "") %>%
  mutate(mbi_de01 = gsub("NA", "", mbi_de01)) %>%
  mutate(mbi_de01 = labelled(as.numeric(factor(mbi_de01)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_de01 <- set_label(ZK_merged$mbi_de01, "Ich glaube, dass ich manche Patienten so behandle, als wären sie unpersönliche ^Objekte^.")


# D2: "Ich habe das Gefühl, dass Patienten mir die Schuld für einige ihrer Probleme geben."
Kadur_413$K_90_mbi_de02
Ziessler_229$Z_139_mbi_de02
var_label(ZK_merged$K_90_mbi_de02)
var_label(Ziessler_229$Z_139_mbi_de02)
##var_labels(ZK_merged$mbi_de02)

ZK_merged <- ZK_merged %>%
  unite(mbi_de02, K_90_mbi_de02, Z_139_mbi_de02, sep = "") %>%
  mutate(mbi_de02 = gsub("NA", "", mbi_de01)) %>%
  mutate(mbi_de02 = labelled(as.numeric(factor(mbi_de02)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_de02 <- set_label(ZK_merged$mbi_de02, "Ich habe das Gefühl, dass Patienten mir die Schuld für einige ihrer Probleme geben.")


# D3: "Ich befürchte, dass diese Arbeit mich emotional verhärtet."
Kadur_413$K_92_mbi_de03
Ziessler_229$Z_141_mbi_de03
#var_label(ZK_merged$K_92_mbi_de03)
#var_label(Ziessler_229$Z_141_mbi_de03)
##var_labels(ZK_merged$mbi_de03)

ZK_merged <- ZK_merged %>%
  unite(mbi_de03, K_92_mbi_de03, Z_141_mbi_de03, sep = "") %>%
  mutate(mbi_de03 = gsub("NA", "", mbi_de03)) %>%
  mutate(mbi_de03 = labelled(as.numeric(factor(mbi_de03)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_de03 <- set_label(ZK_merged$mbi_de03, "Ich befürchte, dass diese Arbeit mich emotional verhärtet.")


# D4: "Es macht mir nicht wirklich viel aus, was mit manchen Patienten passiert."
#Kadur_413$K_94_mbi_de04
#Ziessler_229$Z_143_mbi_de04
###var_labels(ZK_merged$mbi_de04)

ZK_merged <- ZK_merged %>%
  unite(mbi_de04, K_94_mbi_de04, Z_143_mbi_de04, sep = "") %>%
  mutate(mbi_de04 = gsub("NA", "", mbi_de04)) %>%
  mutate(mbi_de04 = labelled(as.numeric(factor(mbi_de04)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_de04 <- set_label(ZK_merged$mbi_de04, "Es macht mir nicht wirklich viel aus, was mit manchen Patienten passiert.")


# D5: "Seitdem ich diese Arbeit ausübe, bin ich gefühlloser im Umgang mit anderen Menschen geworden."
#Kadur_413$K_95_mbi_de05
#Ziessler_229$Z_144_mbi_de05
##var_labels(ZK_merged$mbi_de05)

ZK_merged <- ZK_merged %>%
  unite(mbi_de05, K_95_mbi_de05, Z_144_mbi_de05, sep = "") %>%
  mutate(mbi_de05 = gsub("NA", "", mbi_de05)) %>%
  mutate(mbi_de05 = labelled(as.numeric(factor(mbi_de05)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_de05 <- set_label(ZK_merged$mbi_de05, "Seitdem ich diese Arbeit ausübe, bin ich gefühlloser im Umgang mit anderen Menschen geworden.")



# PE1 (reversed, muss in Rechnung noch umgepolt werden): "Ich habe das Gefühl, dass ich durch meine Arbeit das Leben anderer Menschen positiv beeinflusse."
#Kadur_413$K_79_mbi_pe01
#Ziessler_229$Z_128_mbi_pe01
##var_labels(ZK_merged$mbi_pe01)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe01, K_79_mbi_pe01, Z_128_mbi_pe01, sep = "") %>%
  mutate(mbi_pe01 = gsub("NA", "", mbi_pe01)) %>%
  mutate(mbi_pe01 = labelled(as.numeric(factor(mbi_pe01)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe01 <- set_label(ZK_merged$mbi_pe01, "Ich habe das Gefühl, dass ich durch meine Arbeit das Leben anderer Menschen positiv beeinflusse.")


# PE2 (reversed, muss in Rechnung noch umgepolt werden): "Ich bin guter Stimmung, wenn ich intensiv mit meinen Patienten gearbeitet habe."
#Kadur_413$K_80_mbi_pe02
#Ziessler_229$Z_129_mbi_pe02
##var_labels(ZK_merged$mbi_pe02)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe02, K_80_mbi_pe02, Z_129_mbi_pe02, sep = "") %>%
  mutate(mbi_pe02 = gsub("NA", "", mbi_pe02)) %>%
  mutate(mbi_pe02 = labelled(as.numeric(factor(mbi_pe02)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe02 <- set_label(ZK_merged$mbi_pe02, "Ich bin guter Stimmung, wenn ich intensiv mit meinen Patienten gearbeitet habe.")


# PE3 (reversed, muss in Rechnung noch umgepolt werden): "Bei der Arbeit gehe ich mit emotionalen Problemen ziemlich gelassen um."
#Kadur_413$K_82_mbi_pe03
#Ziessler_229$Z_131_mbi_pe03
##var_labels(ZK_merged$mbi_pe03)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe03, K_82_mbi_pe03, Z_131_mbi_pe03, sep = "") %>%
  mutate(mbi_pe03 = gsub("NA", "", mbi_pe03)) %>%
  mutate(mbi_pe03 = labelled(as.numeric(factor(mbi_pe03)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe03 <- set_label(ZK_merged$mbi_pe03, "Bei der Arbeit gehe ich mit emotionalen Problemen ziemlich gelassen um.")


# PE4 (reversed, muss in Rechnung noch umgepolt werden): "Es ist leicht für mich, eine entspannte Atmosphäre mit meinen Patienten herzustellen."
#Kadur_413$K_84_mbi_pe04
#Ziessler_229$Z_133_mbi_pe04
##var_labels(ZK_merged$mbi_pe04)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe04, K_84_mbi_pe04, Z_133_mbi_pe04, sep = "") %>%
  mutate(mbi_pe04 = gsub("NA", "", mbi_pe04)) %>%
  mutate(mbi_pe04 = labelled(as.numeric(factor(mbi_pe04)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe04 <- set_label(ZK_merged$mbi_pe04, "Es ist leicht für mich, eine entspannte Atmosphäre mit meinen Patienten herzustellen.")


# PE5 (reversed, muss in Rechnung noch umgepolt werden): "Ich fühle mich sehr tatkräftig."
#Kadur_413$K_86_mbi_pe05
#Ziessler_229$Z_135_mbi_pe05
##var_labels(ZK_merged$mbi_pe05)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe05, K_86_mbi_pe05, Z_135_mbi_pe05, sep = "") %>%
  mutate(mbi_pe05 = gsub("NA", "", mbi_pe05)) %>%
  mutate(mbi_pe05 = labelled(as.numeric(factor(mbi_pe05)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe05 <- set_label(ZK_merged$mbi_pe05, "Ich fühle mich sehr tatkräftig.")


# PE6 (reversed, muss in Rechnung noch umgepolt werden): "Ich gehe ziemlich erfolgreich mit den Problemen meiner Patienten um."
#Kadur_413$K_87_mbi_pe06
#Ziessler_229$Z_136_mbi_pe06
##var_labels(ZK_merged$mbi_pe06)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe06, K_87_mbi_pe06, Z_136_mbi_pe06, sep = "") %>%
  mutate(mbi_pe06 = gsub("NA", "", mbi_pe06)) %>%
  mutate(mbi_pe06 = labelled(as.numeric(factor(mbi_pe06)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe06 <- set_label(ZK_merged$mbi_pe06, "Ich gehe ziemlich erfolgreich mit den Problemen meiner Patienten um.")


# PE7 (reversed, muss in Rechnung noch umgepolt werden): "Ich habe in meiner Arbeit viele lohnenswerte Dinge erreicht."
#Kadur_413$K_91_mbi_pe07
#Ziessler_229$Z_140_mbi_pe07
##var_labels(ZK_merged$mbi_pe07)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe07, K_91_mbi_pe07, Z_140_mbi_pe07, sep = "") %>%
  mutate(mbi_pe07 = gsub("NA", "", mbi_pe07)) %>%
  mutate(mbi_pe07 = labelled(as.numeric(factor(mbi_pe07)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe07 <- set_label(ZK_merged$mbi_pe07, "Ich habe in meiner Arbeit viele lohnenswerte Dinge erreicht.")


# PE8 (reversed, muss in Rechnung noch umgepolt werden): "Es fällt mir leicht, mich in meine Patienten hineinzuversetzen."
#Kadur_413$K_93_mbi_pe08
#Ziessler_229$Z_142_mbi_pe08
##var_labels(ZK_merged$mbi_pe08)

ZK_merged <- ZK_merged %>%
  unite(mbi_pe08, K_93_mbi_pe08, Z_142_mbi_pe08, sep = "") %>%
  mutate(mbi_pe08 = gsub("NA", "", mbi_pe08)) %>%
  mutate(mbi_pe08 = labelled(as.numeric(factor(mbi_pe08)), 
                             labels = c("nie/does not occur at all" = 1, 
                                        "sehr selten" = 2, 
                                        "eher selten" = 3, 
                                        "manchmal" = 4, 
                                        "eher oft" = 5, 
                                        "sehr oft/occurs very often" = 6)))
ZK_merged$mbi_pe08 <- set_label(ZK_merged$mbi_pe08, "Es fällt mir leicht, mich in meine Patienten hineinzuversetzen.")



############################################################################################## 5.9 NFC [= 16 items] (negatively worded items; 4, 6, 7, 8, 9, 10, 11, 12, 15, 16)
# nfc 01: "Die Aufgabe, neue Lösungen für Probleme zu finden, macht mir wirklich Spaß."
#Kadur_413$K_108_nfc01
#Ziessler_229$Z_54_dupl1_nfc_01
#ZK_merged$nfc_01

ZK_merged <- ZK_merged %>%
  unite(nfc_01, K_108_nfc01, Z_54_dupl1_nfc_01, sep = "") %>%
  mutate(nfc_01 = gsub("NA", "", nfc_01)) %>%
  mutate(nfc_01 = labelled(as.numeric(nfc_01),
                           labels = c("völlig unzutreffend/completely inaccurate -3" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor 0" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate +3" = 3)))
ZK_merged$nfc_01 <- set_label(ZK_merged$nfc_01, "Die Aufgabe, neue Lösungen für Probleme zu finden, macht mir wirklich Spaß.")

ZK_merged$nfc_01

# nfc 02: "Ich würde lieber eine Aufgabe lösen, die Intelligenz erfordert, schwierig und  bedeutend ist, als eine Aufgabe, die zwar irgendwie wichtig ist, aber nicht viel  Nachdenken erfordert."
Kadur_413$K_109_nfc02
Ziessler_229$Z_55_dupl1_nfc_02
#ZK_merged$nfc_02

ZK_merged <- ZK_merged %>%
  unite(nfc_02, K_109_nfc02, Z_55_dupl1_nfc_02, sep = "") %>%
  mutate(nfc_02 = gsub("NA", "", nfc_02)) %>%
  mutate(nfc_02 = labelled(as.numeric(nfc_02),
                           labels = c("völlig unzutreffend/completely inaccurate -3" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor 0" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate +3" = 3)))
ZK_merged$nfc_02 <- set_label(ZK_merged$nfc_02, "Ich würde lieber eine Aufgabe lösen, die Intelligenz erfordert, schwierig und  bedeutend ist, als eine Aufgabe, die zwar irgendwie wichtig ist, aber nicht viel  Nachdenken erfordert.")


# nfc 03: "Ich setze mir eher solche Ziele, die nur mit erheblicher geistiger Anstrengung  erreicht werden können."
#Kadur_413$K_110_nfc03
#Ziessler_229$Z_56_dupl1_nfc_03
#ZK_merged$nfc_03

ZK_merged <- ZK_merged %>%
  unite(nfc_03, K_110_nfc03, Z_56_dupl1_nfc_03, sep = "") %>%
  mutate(nfc_03 = gsub("NA", "", nfc_03)) %>%
  mutate(nfc_03 = labelled(as.numeric(nfc_03),
                           labels = c("völlig unzutreffend/completely inaccurate -3" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor 0" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate +3" = 3)))
ZK_merged$nfc_03 <- set_label(ZK_merged$nfc_03, "Ich setze mir eher solche Ziele, die nur mit erheblicher geistiger Anstrengung  erreicht werden können.")


# nfc 04 (reversed, muss noch umgepolt werden): "Die Vorstellung, mich auf mein Denkvermögen zu verlassen, um es zu etwas zu  bringen, spricht mich nicht an."
Kadur_413$K_111_nfc04
Ziessler_229$Z_57_dupl1_nfc_04
#ZK_merged$nfc_04

ZK_merged <- ZK_merged %>%
  unite(nfc_04, K_111_nfc04, Z_57_dupl1_nfc_04, sep = "") %>%
  mutate(nfc_04 = gsub("NA", "", nfc_04)) %>%
  mutate(nfc_04 = labelled(as.numeric(nfc_04),
                           labels = c("völlig unzutreffend/completely inaccurate -3" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor 0" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate +3" = 3)))
ZK_merged$nfc_04 <- set_label(ZK_merged$nfc_04, "Ich setze mir eher solche Ziele, die nur mit erheblicher geistiger Anstrengung  erreicht werden können.")


# nfc 05: "Ich finde es besonders befriedigend, eine bedeutende Aufgabe abzuschließen,  die viel Denken und geistige Anstrengung erfordert hat."
Kadur_413$K_112_nfc05
Ziessler_229$Z_58_dupl1_nfc_05
#ZK_merged$nfc_05

ZK_merged <- ZK_merged %>%
  unite(nfc_05, K_112_nfc05, Z_58_dupl1_nfc_05, sep = "") %>%
  mutate(nfc_05 = gsub("NA", "", nfc_05)) %>%
  mutate(nfc_05 = labelled(as.numeric(nfc_05),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_05 <- set_label(ZK_merged$nfc_05, "Ich finde es besonders befriedigend, eine bedeutende Aufgabe abzuschließen,  die viel Denken und geistige Anstrengung erfordert hat.")


# nfc 06 (reversed, muss noch umgepolt werden): "Ich denke lieber über kleine, alltägliche Vorhaben nach, als über langfristige."
#Kadur_413$K_113_nfc06
#Ziessler_229$Z_59_dupl1_nfc_06
#ZK_merged$nfc_06

ZK_merged <- ZK_merged %>%
  unite(nfc_06, K_113_nfc06, Z_59_dupl1_nfc_06, sep = "") %>%
  mutate(nfc_06 = gsub("NA", "", nfc_06)) %>%
  mutate(nfc_06 = labelled(as.numeric(nfc_06),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_06 <- set_label(ZK_merged$nfc_06, "Ich denke lieber über kleine, alltägliche Vorhaben nach, als über langfristige.")


# nfc 07 (reversed, muss noch umgepolt werden): "Ich würde lieber etwas tun, das wenig Denken erfordert, als etwas, das mit  Sicherheit meine Denkfähigkeit herausfordert."
#Kadur_413$K_114_nfc07
#Ziessler_229$Z_60_dupl1_nfc_07
#ZK_merged$nfc_07

ZK_merged <- ZK_merged %>%
  unite(nfc_07, K_114_nfc07, Z_60_dupl1_nfc_07, sep = "") %>%
  mutate(nfc_07 = gsub("NA", "", nfc_07)) %>%
  mutate(nfc_07 = labelled(as.numeric(nfc_07),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_07 <- set_label(ZK_merged$nfc_07, "Ich würde lieber etwas tun, das wenig Denken erfordert, als etwas, das mit  Sicherheit meine Denkfähigkeit herausfordert.")


# nfc 08 (reversed, muss noch umgepolt werden): "Ich finde wenig Befriedigung darin, angestrengt und stundenlang nachzudenken."
#Kadur_413$K_115_nfc08
#Ziessler_229$Z_61_dupl1_nfc_08
#ZK_merged$nfc_08

ZK_merged <- ZK_merged %>%
  unite(nfc_08, K_115_nfc08, Z_61_dupl1_nfc_08, sep = "") %>%
  mutate(nfc_08 = gsub("NA", "", nfc_08)) %>%
  mutate(nfc_08 = labelled(as.numeric(nfc_08),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_08 <- set_label(ZK_merged$nfc_08, "Ich finde wenig Befriedigung darin, angestrengt und stundenlang nachzudenken.")


# nfc 09 (reversed, muss noch umgepolt werden): "In erster Linie denke ich, weil ich es muss."
Kadur_413$K_116_nfc09
Ziessler_229$Z_62_dupl1_nfc_09
#ZK_merged$nfc_09

ZK_merged <- ZK_merged %>%
  unite(nfc_09, K_116_nfc09, Z_62_dupl1_nfc_09, sep = "") %>%
  mutate(nfc_09 = gsub("NA", "", nfc_09)) %>%
  mutate(nfc_09 = labelled(as.numeric(nfc_09),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_09 <- set_label(ZK_merged$nfc_09, "In erster Linie denke ich, weil ich es muss.")


# nfc 10 (reversed, muss noch umgepolt werden): "Ich trage nicht gern die Verantwortung für eine Situation, die sehr viel Denken  erfordert."
#Kadur_413$K_117_nfc10
#Ziessler_229$Z_63_dupl1_nfc_10
#ZK_merged$nfc_10

ZK_merged <- ZK_merged %>%
  unite(nfc_10, K_117_nfc10, Z_63_dupl1_nfc_10, sep = "") %>%
  mutate(nfc_10 = gsub("NA", "", nfc_10)) %>%
  mutate(nfc_10 = labelled(as.numeric(nfc_10),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_10 <- set_label(ZK_merged$nfc_10, "Ich trage nicht gern die Verantwortung für eine Situation, die sehr viel Denken  erfordert.")


# nfc 11 (reversed, muss noch umgepolt werden): "Denken entspricht nicht dem, was ich unter Spaß verstehe."
Kadur_413$K_118_nfc11
Ziessler_229$Z_64_dupl1_nfc_11
#ZK_merged$nfc_11

ZK_merged <- ZK_merged %>%
  unite(nfc_11, K_118_nfc11, Z_64_dupl1_nfc_11, sep = "") %>%
  mutate(nfc_11 = gsub("NA", "", nfc_11)) %>%
  mutate(nfc_11 = labelled(as.numeric(nfc_11),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_11 <- set_label(ZK_merged$nfc_11, "Denken entspricht nicht dem, was ich unter Spaß verstehe.")


# nfc 12 (reversed, muss noch umgepolt werden): "Ich versuche, Situationen vorauszuahnen und zu vermeiden, in denen die  Wahrscheinlichkeit groß ist, dass ich intensiv über etwas nachdenken muss."
#Kadur_413$K_119_nfc12
#Ziessler_229$Z_65_dupl1_nfc_12
#ZK_merged$nfc_12

ZK_merged <- ZK_merged %>%
  unite(nfc_12, K_119_nfc12, Z_65_dupl1_nfc_12, sep = "") %>%
  mutate(nfc_12 = gsub("NA", "", nfc_12)) %>%
  mutate(nfc_12 = labelled(as.numeric(nfc_12),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_12 <- set_label(ZK_merged$nfc_12, "Ich versuche, Situationen vorauszuahnen und zu vermeiden, in denen die  Wahrscheinlichkeit groß ist, dass ich intensiv über etwas nachdenken muss.")


# nfc 13: "Ich habe es gern, wenn mein Leben voller kniffliger Aufgaben ist, die ich lösen  muss.
#Kadur_413$K_120_nfc13
#Ziessler_229$Z_66_dupl1_nfc_13
#ZK_merged$nfc_13

ZK_merged <- ZK_merged %>%
  unite(nfc_13, K_120_nfc13, Z_66_dupl1_nfc_13, sep = "") %>%
  mutate(nfc_13 = gsub("NA", "", nfc_13)) %>%
  mutate(nfc_13 = labelled(as.numeric(nfc_13),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_13 <- set_label(ZK_merged$nfc_13, "Ich habe es gern, wenn mein Leben voller kniffliger Aufgaben ist, die ich lösen  muss.")


# nfc 14: "Ich würde komplizierte Probleme einfachen Problemen vorziehen.
#Kadur_413$K_121_nfc14
#Ziessler_229$Z_67_dupl1_nfc_14
#ZK_merged$nfc_14

ZK_merged <- ZK_merged %>%
  unite(nfc_14, K_121_nfc14, Z_67_dupl1_nfc_14, sep = "") %>%
  mutate(nfc_14 = gsub("NA", "", nfc_14)) %>%
  mutate(nfc_14 = labelled(as.numeric(nfc_14),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_14 <- set_label(ZK_merged$nfc_14, "Ich würde komplizierte Probleme einfachen Problemen vorziehen.")


# nfc 15 (reversed, muss noch umgepolt werden): "Es genügt mir, einfach die Antwort zu kennen, ohne die Gründe für die Antwort  eines Problems zu verstehen."
#Kadur_413$K_122_nfc15
#Ziessler_229$Z_68_dupl1_nfc_15
#ZK_merged$nfc_15

ZK_merged <- ZK_merged %>%
  unite(nfc_15, K_122_nfc15, Z_68_dupl1_nfc_15, sep = "") %>%
  mutate(nfc_15 = gsub("NA", "", nfc_15)) %>%
  mutate(nfc_15 = labelled(as.numeric(nfc_15),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_15 <- set_label(ZK_merged$nfc_15, "Es genügt mir, einfach die Antwort zu kennen, ohne die Gründe für die Antwort  eines Problems zu verstehen.")


# nfc 16 (reversed, muss noch umgepolt werden): "Es genügt, dass etwas funktioniert, mir ist es egal, wie oder warum."
#Kadur_413$K_123_nfc16
#Ziessler_229$Z_69_dupl1_nfc_16
#ZK_merged$nfc_16

ZK_merged <- ZK_merged %>%
  unite(nfc_16, K_123_nfc16, Z_69_dupl1_nfc_16, sep = "") %>%
  mutate(nfc_16 = gsub("NA", "", nfc_16)) %>%
  mutate(nfc_16 = labelled(as.numeric(nfc_16),
                           labels = c("völlig unzutreffend/completely inaccurate" = -3,
                                      "-2" = -2,
                                      "-1" = -1,
                                      "weder noch/neither nor" = 0,
                                      "+1" = 1,
                                      "+2" = 2,
                                      "völlig zutreffend/very accurate" = 3)))
ZK_merged$nfc_16 <- set_label(ZK_merged$nfc_16, "Es genügt, dass etwas funktioniert, mir ist es egal, wie oder warum.")



############################################################################################## 5.10 self-control [= 13 items] (1=completely inaccurate) to 5=applies exactly)
## negatively formulated items 2, 3, 4, 5, 6, 7, 8, 10, 11
## 1 (= completely inaccurate) to 5 (= applies exactly)
# sc 01: "Ich bin gut darin, Versuchungen zu widerstehen."
Kadur_413$K_134_scs_01
Ziessler_229$Z_70_scs_01
#var_labels(ZK_merged$sc_01)

ZK_merged <- ZK_merged %>%
  unite(sc_01, K_134_scs_01, Z_70_scs_01, sep = "") %>%
  mutate(sc_01 = gsub("NA", "", sc_01)) %>%
  mutate(sc_01 = labelled(as.numeric(factor(sc_01)), 
                           labels = c("völlig unzutreffend/completely inaccurate 1" = 1, 
                                      "2" = 2, 
                                      "3" = 3, 
                                      "4" = 4, 
                                      "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_01 <- set_label(ZK_merged$sc_01, "Ich bin gut darin, Versuchungen zu widerstehen.")


# sc 02 (reversed, muss noch umgepolt werden): "Es fällt mir schwer, schlechte Gewohnheiten abzulegen."
Kadur_413$K_135_scs_02
Ziessler_229$Z_71_scs_02
#var_labels(ZK_merged$sc_02)

ZK_merged <- ZK_merged %>%
  unite(sc_02, K_135_scs_02, Z_71_scs_02, sep = "") %>%
  mutate(sc_02 = gsub("NA", "", sc_02)) %>%
  mutate(sc_02 = labelled(as.numeric(factor(sc_02)), 
                          labels = c("völlig unzutreffend 1" = 1, 
                                     "2" = 2, 
                                     "3" = 3, 
                                     "4" = 4, 
                                     "trifft ganz genau zu 5" = 5)))
ZK_merged$sc_02 <- set_label(ZK_merged$sc_02, "Es fällt mir schwer, schlechte Gewohnheiten abzulegen.")


# sc 03 (reversed, muss noch umgepolt werden): "Ich bin faul."
Kadur_413$K_136_scs_03
Ziessler_229$Z_72_scs_03
##var_labels(ZK_merged$sc_03)

ZK_merged <- ZK_merged %>%
  unite(sc_03, K_136_scs_03, Z_72_scs_03, sep = "") %>%
  mutate(sc_03 = gsub("NA", "", sc_03)) %>%
  mutate(sc_03 = labelled(as.numeric(factor(sc_03)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_03 <- set_label(ZK_merged$sc_03, "Ich bin faul.")


# sc 04 (reversed, muss noch umgepolt werden): "Ich sage unangemessene Dinge."
Kadur_413$K_137_scs_04
Ziessler_229$Z_73_scs_04
##var_labels(ZK_merged$sc_04)

ZK_merged <- ZK_merged %>%
  unite(sc_04, K_137_scs_04, Z_73_scs_04, sep = "") %>%
  mutate(sc_04 = gsub("NA", "", sc_04)) %>%
  mutate(sc_04 = labelled(as.numeric(factor(sc_04)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_04 <- set_label(ZK_merged$sc_04, "Ich sage unangemessene Dinge.")


# sc 05 (reversed, muss noch umgepolt werden): "Ich tue manchmal Dinge, die schlecht für mich sind, wenn sie mir Spaß machen."
Kadur_413$K_138_scs_05
Ziessler_229$Z_74_scs_05
##var_labels(ZK_merged$sc_05)

ZK_merged <- ZK_merged %>%
  unite(sc_05, K_138_scs_05, Z_74_scs_05, sep = "") %>%
  mutate(sc_05 = gsub("NA", "", sc_05)) %>%
  mutate(sc_05 = labelled(as.numeric(factor(sc_05)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_05 <- set_label(ZK_merged$sc_05, "Ich tue manchmal Dinge, die schlecht für mich sind, wenn sie mir Spaß machen.")


# sc 06 (reversed, muss noch umgepolt werden): "Ich wünschte, ich hätte mehr Selbstdisziplin."
Kadur_413$K_139_scs_06
Ziessler_229$Z_75_scs_06
##var_labels(ZK_merged$sc_06)

ZK_merged <- ZK_merged %>%
  unite(sc_06, K_139_scs_06, Z_75_scs_06, sep = "") %>%
  mutate(sc_06 = gsub("NA", "", sc_06)) %>%
  mutate(sc_06 = labelled(as.numeric(factor(sc_06)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_06 <- set_label(ZK_merged$sc_06, "Ich wünschte, ich hätte mehr Selbstdisziplin.")


# sc 07 (reversed, muss noch umgepolt werden): "Angenehme Aktivitäten und Vergnügen hindern mich manchmal daran, meine Arbeit zu machen."
Kadur_413$K_140_scs_07
Ziessler_229$Z_76_scs_07
##var_labels(ZK_merged$sc_07)

ZK_merged <- ZK_merged %>%
  unite(sc_07, K_140_scs_07, Z_76_scs_07, sep = "") %>%
  mutate(sc_07 = gsub("NA", "", sc_07)) %>%
  mutate(sc_07 = labelled(as.numeric(factor(sc_07)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_07 <- set_label(ZK_merged$sc_07, "Angenehme Aktivitäten und Vergnügen hindern mich manchmal daran, meine Arbeit zu machen.")


# sc 08 (reversed, muss noch umgepolt werden): "Es fällt mir schwer, mich zu konzentrieren."
Kadur_413$K_141_scs_08
Ziessler_229$Z_77_scs_08
##var_labels(ZK_merged$sc_08)

ZK_merged <- ZK_merged %>%
  unite(sc_08, K_141_scs_08, Z_77_scs_08, sep = "") %>%
  mutate(sc_08 = gsub("NA", "", sc_08)) %>%
  mutate(sc_08 = labelled(as.numeric(factor(sc_08)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_08 <- set_label(ZK_merged$sc_08, "Es fällt mir schwer, mich zu konzentrieren.")


# sc 09: "Ich kann effektiv auf langfristige Ziele hinarbeiten."
#Kadur_413$K_142_scs_09
#Ziessler_229$Z_78_scs_09
###var_labels(ZK_merged$sc_09)

ZK_merged <- ZK_merged %>%
  unite(sc_09, K_142_scs_09, Z_78_scs_09, sep = "") %>%
  mutate(sc_09 = gsub("NA", "", sc_09)) %>%
  mutate(sc_09 = labelled(as.numeric(factor(sc_09)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_09 <- set_label(ZK_merged$sc_09, "Ich kann effektiv auf langfristige Ziele hinarbeiten.")


# sc 10 (reversed, muss noch umgepolt werden): "Manchmal kann ich mich selbst nicht daran hindern, etwas zu tun, obwohl ich weiß, dass es falsch ist."
Kadur_413$K_143_scs_10
Ziessler_229$Z_79_scs_10
##var_labels(ZK_merged$sc_10)

ZK_merged <- ZK_merged %>%
  unite(sc_10, K_143_scs_10, Z_79_scs_10, sep = "") %>%
  mutate(sc_10 = gsub("NA", "", sc_10)) %>%
  mutate(sc_10 = labelled(as.numeric(factor(sc_10)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_10 <- set_label(ZK_merged$sc_10, "Manchmal kann ich mich selbst nicht daran hindern, etwas zu tun, obwohl ich weiß, dass es falsch ist.")


# sc 11 (reversed, muss noch umgepolt werden): "Ich handle oft ohne alle Alternativen durchdacht zu haben.."
Kadur_413$K_144_scs_11
Ziessler_229$Z_80_scs_11
##var_labels(ZK_merged$sc_11)

ZK_merged <- ZK_merged %>%
  unite(sc_11, K_144_scs_11, Z_80_scs_11, sep = "") %>%
  mutate(sc_11 = gsub("NA", "", sc_11)) %>%
  mutate(sc_11 = labelled(as.numeric(factor(sc_11)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_11 <- set_label(ZK_merged$sc_11, "Ich handle oft ohne alle Alternativen durchdacht zu haben.")


# sc 12: "Ich lehne Dinge ab, die schlecht für mich sind."
#Kadur_413$K_145_scs_12
#Ziessler_229$Z_81_scs_12
###var_labels(ZK_merged$sc_12)

ZK_merged <- ZK_merged %>%
  unite(sc_12, K_145_scs_12, Z_81_scs_12, sep = "") %>%
  mutate(sc_12 = gsub("NA", "", sc_12)) %>%
  mutate(sc_12 = labelled(as.numeric(factor(sc_12)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_12 <- set_label(ZK_merged$sc_12, "Ich lehne Dinge ab, die schlecht für mich sind.")


# sc 13: "Andere würden sagen, dass ich eine eiserne Selbstdisziplin habe."
Kadur_413$K_146_scs_13
Ziessler_229$Z_82_scs_13
##var_labels(ZK_merged$sc_13)

ZK_merged <- ZK_merged %>%
  unite(sc_13, K_146_scs_13, Z_82_scs_13, sep = "") %>%
  mutate(sc_13 = gsub("NA", "", sc_13)) %>%
  mutate(sc_13 = labelled(as.numeric(factor(sc_13)),
                          labels = c("völlig unzutreffend/completely inaccurate 1" = 1,
                                     "2" = 2,
                                     "3" = 3,
                                     "4" = 4,
                                     "trifft ganz genau zu/applies exactly 5" = 5)))
ZK_merged$sc_13 <- set_label(ZK_merged$sc_13, "Andere würden sagen, dass ich eine eiserne Selbstdisziplin habe.")

colnames(ZK_merged)


################################################################################################################################### 5.11 emotion regulation strategies [10 items]
## reappraisal [6 items] (items 1, 3, 5, 7, 8, and 10)
## K_188_ERQ_NB
## Z_165_EMO_NEU_MW
# ers_re 01
Kadur_413$K_124_erq_nb01
Ziessler_229$Z_83_erq_nb01
##var_labels(ZK_merged$ers_re_01)

ZK_merged <- ZK_merged %>%
  unite(ers_re_01, K_124_erq_nb01, Z_83_erq_nb01, sep = "") %>%
  mutate(ers_re_01 = gsub("NA", "", ers_re_01)) %>%
  mutate(ers_re_01 = labelled(as.numeric(factor(ers_re_01)), 
                          labels = c("stimmt überhaupt nicht 1" = 1, 
                                     "2" = 2, 
                                     "3" = 3, 
                                     "neutral 4" = 4, 
                                     "5" = 5,
                                     "6" = 6,
                                     "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_01 <- set_label(ZK_merged$ers_re_01, "Wenn ich mehr positive Gefühle (wie Freude oder Heiterkeit) empfinden möchte, ändere ich, woran ich denke.")


# ers_re 02
Kadur_413$K_126_erq_nb02
Ziessler_229$Z_85_erq_nb02
##var_labels(ZK_merged$ers_re_02)

ZK_merged <- ZK_merged %>%
  unite(ers_re_02, K_126_erq_nb02, Z_85_erq_nb02, sep = "") %>%
  mutate(ers_re_02 = gsub("NA", "", ers_re_02)) %>%
  mutate(ers_re_02 = labelled(as.numeric(factor(ers_re_02)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_02 <- set_label(ZK_merged$ers_re_02, "Wenn ich weniger negative Gefühle (wie Traurigkeit oder Ärger) empfinden möchte, ändere ich, woran ich denke.")


#ers_re 03
Kadur_413$K_128_erq_nb03
Ziessler_229$Z_87_erq_nb03
##var_labels(ZK_merged$ers_re_03)

ZK_merged <- ZK_merged %>%
  unite(ers_re_03, K_128_erq_nb03, Z_87_erq_nb03, sep = "") %>%
  mutate(ers_re_03 = gsub("NA", "", ers_re_03)) %>%
  mutate(ers_re_03 = labelled(as.numeric(factor(ers_re_03)),
                              labels = c("stimmt überhaupt nicht 1" = 1,
                                         "2" = 2,
                                         "3" = 3,
                                         "neutral 4" = 4,
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_03 <- set_label(ZK_merged$ers_re_03, "Wenn ich in eine stressige Situation gerate, ändere ich meine Gedanken über die Situation so, dass es mich beruhigt.")


#ers_re 04
Kadur_413$K_130_erq_nb04
Ziessler_229$Z_89_erq_nb04
##var_labels(ZK_merged$ers_re_04)

ZK_merged <- ZK_merged %>%
  unite(ers_re_04, K_130_erq_nb04, Z_89_erq_nb04, sep = "") %>%
  mutate(ers_re_04 = gsub("NA", "", ers_re_04)) %>%
  mutate(ers_re_04 = labelled(as.numeric(factor(ers_re_04)),
                              labels = c("stimmt überhaupt nicht 1" = 1,
                                         "2" = 2,
                                         "3" = 3,
                                         "neutral 4" = 4,
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_04 <- set_label(ZK_merged$ers_re_04, "Wenn ich mehr positive Gefühle empfinden möchte, versuche ich über die Situation anders zu denken.")


#ers_re 05
Kadur_413$K_131_erq_nb05
Ziessler_229$Z_90_erq_nb05
##var_labels(ZK_merged$ers_re_05)

ZK_merged <- ZK_merged %>%
  unite(ers_re_05, K_131_erq_nb05, Z_90_erq_nb05, sep = "") %>%
  mutate(ers_re_05 = gsub("NA", "", ers_re_05)) %>%
  mutate(ers_re_05 = labelled(as.numeric(factor(ers_re_05)),
                              labels = c("stimmt überhaupt nicht 1" = 1,
                                         "2" = 2,
                                         "3" = 3,
                                         "neutral 4" = 4,
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_05 <- set_label(ZK_merged$ers_re_05, "Ich halte meine Gefühle unter Kontrolle, indem ich über meine aktuelle Situation anders nachdenke.")


#ers_re 06
Kadur_413$K_133_erq_nb06
Ziessler_229$Z_92_erq_nb06
##var_labels(ZK_merged$ers_re_06)

ZK_merged <- ZK_merged %>%
  unite(ers_re_06, K_133_erq_nb06, Z_92_erq_nb06, sep = "") %>%
  mutate(ers_re_06 = gsub("NA", "", ers_re_06)) %>%
  mutate(ers_re_06 = labelled(as.numeric(factor(ers_re_06)),
                              labels = c("stimmt überhaupt nicht 1" = 1,
                                         "2" = 2,
                                         "3" = 3,
                                         "neutral 4" = 4,
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_re_06 <- set_label(ZK_merged$ers_re_06, "Wenn ich weniger negative Gefühle empfinden möchte, versuche ich über die Situation anders zu denken.")

#colname(ZK_merged$ers_re_01)

################################## suppression [4 items] (items 2, 4, 6, and 9)
## K_189_ERQ_UD
## Z_166_EMO_UNTER_MW
# ers_su 01
Kadur_413$K_125_erq_ud01
Ziessler_229$Z_84_erq_ud01
var_labels(ZK_merged$ers_su_01)

ZK_merged <- ZK_merged %>%
  unite(ers_su_01, K_125_erq_ud01, Z_84_erq_ud01, sep = "") %>%
  mutate(ers_su_01 = gsub("NA", "", ers_su_01)) %>%
  mutate(ers_su_01 = labelled(as.numeric(factor(ers_su_01)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_su_01 <- set_label(ZK_merged$ers_su_01, "Ich behalte meine Gefühle für mich.")


# ers_su 02
Kadur_413$K_127_erq_ud02
Ziessler_229$Z_86_erq_ud02
var_labels(ZK_merged$ers_su_02)

ZK_merged <- ZK_merged %>%
  unite(ers_su_02, K_127_erq_ud02, Z_86_erq_ud02, sep = "") %>%
  mutate(ers_su_02 = gsub("NA", "", ers_su_02)) %>%
  mutate(ers_su_02 = labelled(as.numeric(factor(ers_su_02)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_su_02 <- set_label(ZK_merged$ers_su_02, "Wenn ich positive Gefühle empfinde, bemühe ich mich, sie nicht nach außen zu zeigen.")


# ers_su 03
Kadur_413$K_129_erq_ud03
Ziessler_229$Z_88_erq_ud03
var_labels(ZK_merged$ers_su_03)

ZK_merged <- ZK_merged %>%
  unite(ers_su_03, K_129_erq_ud03, Z_88_erq_ud03, sep = "") %>%
  mutate(ers_su_03 = gsub("NA", "", ers_su_03)) %>%
  mutate(ers_su_03 = labelled(as.numeric(factor(ers_su_03)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_su_03 <- set_label(ZK_merged$ers_su_03, "Ich halte meine Gefühle unter Kontrolle, indem ich sie nicht nach außen zeige.")



# ers_su 04
Kadur_413$K_132_erq_ud04
Ziessler_229$Z_91_erq_ud04
ZK_merged$ers_su_04

ZK_merged <- ZK_merged %>%
  unite(ers_su_04, K_132_erq_ud04, Z_91_erq_ud04, sep = "") %>%
  mutate(ers_su_04 = gsub("NA", "", ers_su_04)) %>%
  mutate(ers_su_04 = labelled(as.numeric(factor(ers_su_04)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$ers_su_04 <- set_label(ZK_merged$ers_su_04, " Wenn ich negative Gefühle empfinde, sorge ich dafür, sie nicht nach außen zu zeigen.")



############################################################################################################ 5.12 coping strategies [20 items]
## 1 (= does not apply) to 4 (= applies exactly)
### adaptive coping strategies [16 items] (items 1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 14, 16, 17, 18, 19, and 20)

## positive thinkink [4 items] (items )
# K_162_sci_pos1, K_166_sci_pos2, K_173_sci_pos3, K_171_sci_pos4
# Z_93_sci_pos1, Z_97_sci_pos2, Z_108_sci_pos3, Z_98_sci_pos4

## active stress management [4 items] (items )
# K_163_sci_akt1, K_176_sci_akt2, K_169_sci_akt3, K_177_sci_akt4
# Z_104_sci_akt1, Z_95_sci_akt2, Z_99_sci_akt3, Z_109_sci_akt4

## social support [4 items] (items )
# K_160_sci_sup1, K_165_sci_sup2, K_172_sci_sup3, K_167_sci_sup4
# Z_105_sci_sup1, Z_111_sci_sup2, Z_96_sci_sup3, Z_107_sci_sup4

## holding on to faith [4 items] (items )
# K_175_sci_gla1, K_168_sci_gla2, K_178_sci_gla3, K_159_sci_gla4
# Z_100_sci_gla1, Z_101_sci_gla2, Z_102_sci_gla3, Z_110_sci_gla4

# adapt_cop_01
Kadur_413$K_162_sci_pos1
Ziessler_229$Z_93_sci_pos1
##var_labels(ZK_merged$adapt_cop_01)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_01, K_162_sci_pos1, Z_93_sci_pos1, sep = "") %>%
  mutate(adapt_cop_01 = gsub("NA", "", adapt_cop_01)) %>%
  mutate(adapt_cop_01 = labelled(as.numeric(factor(adapt_cop_01)), 
                              labels = c("trifft gar nicht zu 1" = 1, 
                                         "trifft eher nicht zu 2" = 2, 
                                         "trifft eher zu 3" = 3, 
                                         "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_01 <- set_label(ZK_merged$adapt_cop_01, "Ich sage mir, dass Stress und Druck auch ihre guten Seiten haben.")


# adapt_cop_02
Kadur_413$K_166_sci_pos2
Ziessler_229$Z_97_sci_pos2
##var_labels(ZK_merged$adapt_cop_02)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_02, K_166_sci_pos2, Z_97_sci_pos2, sep = "") %>%
  mutate(adapt_cop_02 = gsub("NA", "", adapt_cop_02)) %>%
  mutate(adapt_cop_02 = labelled(as.numeric(factor(adapt_cop_02)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_02 <- set_label(ZK_merged$adapt_cop_02, "Ich sehe Stress und Druck als positive Herausforderung an.")

# adapt_cop_03
Kadur_413$K_173_sci_pos3
Ziessler_229$Z_108_sci_pos3
##var_labels(ZK_merged$adapt_cop_03)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_03, K_173_sci_pos3, Z_108_sci_pos3, sep = "") %>%
  mutate(adapt_cop_03 = gsub("NA", "", adapt_cop_03)) %>%
  mutate(adapt_cop_03 = labelled(as.numeric(factor(adapt_cop_03)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_03 <- set_label(ZK_merged$adapt_cop_03, "Bei Stress und Druck konzentriere ich mich einfach auf das Positive.")

# adapt_cop_04
Kadur_413$K_171_sci_pos4
Ziessler_229$Z_98_sci_pos4
##var_labels(ZK_merged$adapt_cop_04)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_04, K_171_sci_pos4, Z_98_sci_pos4, sep = "") %>%
  mutate(adapt_cop_04 = gsub("NA", "", adapt_cop_04)) %>%
  mutate(adapt_cop_04 = labelled(as.numeric(factor(adapt_cop_04)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_04 <- set_label(ZK_merged$adapt_cop_04, "Auch wenn ich sehr unter Druck stehe, verliere ich meinen Humor nicht.")

# adapt_cop_05 (ab hier aktice stress management)
Kadur_413$K_163_sci_akt1
Ziessler_229$Z_104_sci_akt1
##var_labels(ZK_merged$adapt_cop_05)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_05, K_163_sci_akt1, Z_104_sci_akt1, sep = "") %>%
  mutate(adapt_cop_05 = gsub("NA", "", adapt_cop_05)) %>%
  mutate(adapt_cop_05 = labelled(as.numeric(factor(adapt_cop_05)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_05 <- set_label(ZK_merged$adapt_cop_05, "Ich tue alles, damit Stress erst gar nicht entsteht.")


# adapt_cop_06
Kadur_413$K_176_sci_akt2
Ziessler_229$Z_95_sci_akt2
##var_labels(ZK_merged$adapt_cop_06)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_06, K_176_sci_akt2, Z_95_sci_akt2, sep = "") %>%
  mutate(adapt_cop_06 = gsub("NA", "", adapt_cop_06)) %>%
  mutate(adapt_cop_06 = labelled(as.numeric(factor(adapt_cop_06)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_06 <- set_label(ZK_merged$adapt_cop_06, "Ich mache mir schon vorher Gedanken, wie ich Zeitdruck vermeiden kann.")


# adapt_cop_07
Kadur_413$K_169_sci_akt3
Ziessler_229$Z_99_sci_akt3
##var_labels(ZK_merged$adapt_cop_07)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_07, K_169_sci_akt3, Z_99_sci_akt3, sep = "") %>%
  mutate(adapt_cop_07 = gsub("NA", "", adapt_cop_07)) %>%
  mutate(adapt_cop_07 = labelled(as.numeric(factor(adapt_cop_07)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_07 <- set_label(ZK_merged$adapt_cop_07, "Ich versuche Stress schon im Vorfeld zu vermeiden.")

       
# adapt_cop_08
Kadur_413$K_177_sci_akt4
Ziessler_229$Z_109_sci_akt4
#var_labels(ZK_merged$adapt_cop_08)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_08, K_177_sci_akt4, Z_109_sci_akt4, sep = "") %>%
  mutate(adapt_cop_08 = gsub("NA", "", adapt_cop_08)) %>%
  mutate(adapt_cop_08 = labelled(as.numeric(factor(adapt_cop_08)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_08 <- set_label(ZK_merged$adapt_cop_08, "Bei Stress und Druck beseitige ich gezielt die Ursachen.")


# adapt_cop_09 (ab hier neue Dimension)
Kadur_413$K_160_sci_sup1
Ziessler_229$Z_105_sci_sup1
#var_labels(ZK_merged$adapt_cop_09)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_09, K_160_sci_sup1, Z_105_sci_sup1, sep = "") %>%
  mutate(adapt_cop_09 = gsub("NA", "", adapt_cop_09)) %>%
  mutate(adapt_cop_09 = labelled(as.numeric(factor(adapt_cop_09)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_09 <- set_label(ZK_merged$adapt_cop_09, "Wenn ich unter Druck gerate, habe ich Menschen, die mir helfen.")


# adapt_cop_10
Kadur_413$K_165_sci_sup2
Ziessler_229$Z_111_sci_sup2
#var_labels(ZK_merged$adapt_cop_10)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_10, K_165_sci_sup2, Z_111_sci_sup2, sep = "") %>%
  mutate(adapt_cop_10 = gsub("NA", "", adapt_cop_10)) %>%
  mutate(adapt_cop_10 = labelled(as.numeric(factor(adapt_cop_10)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_10 <- set_label(ZK_merged$adapt_cop_10, "Egal wie schlimm es wird, ich habe gute Freunde, auf die ich mich immer verlassen kann.")


# adapt_cop_11
Kadur_413$K_172_sci_sup3
Ziessler_229$Z_96_sci_sup3
#var_labels(ZK_merged$adapt_cop_11)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_11, K_172_sci_sup3,  Z_96_sci_sup3, sep = "") %>%
  mutate(adapt_cop_11 = gsub("NA", "", adapt_cop_11)) %>%
  mutate(adapt_cop_11 = labelled(as.numeric(factor(adapt_cop_11)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_11 <- set_label(ZK_merged$adapt_cop_11, "Wenn ich mich überfordert fühle, gibt es Menschen, die mich wieder aufbauen.")


# adapt_cop_12
Kadur_413$K_167_sci_sup4
Ziessler_229$Z_107_sci_sup4
#var_labels(ZK_merged$adapt_cop_12)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_12, K_167_sci_sup4, Z_107_sci_sup4, sep = "") %>%
  mutate(adapt_cop_12 = gsub("NA", "", adapt_cop_12)) %>%
  mutate(adapt_cop_12 = labelled(as.numeric(factor(adapt_cop_12)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_12 <- set_label(ZK_merged$adapt_cop_12, "Bei Stress und Druck finde ich Rückhalt bei meinem Partner oder einem guten Freund.")


# adapt_cop_13
Kadur_413$K_175_sci_gla1
Ziessler_229$Z_100_sci_gla1
#var_labels(ZK_merged$adapt_cop_13)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_13, K_175_sci_gla1, Z_100_sci_gla1, sep = "") %>%
  mutate(adapt_cop_13 = gsub("NA", "", adapt_cop_13)) %>%
  mutate(adapt_cop_13 = labelled(as.numeric(factor(adapt_cop_13)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_13 <- set_label(ZK_merged$adapt_cop_13, "Bei Stress und Druck finde ich Halt im Glauben.")

# adapt_cop_14
Kadur_413$K_168_sci_gla2
Ziessler_229$Z_101_sci_gla2
#var_labels(ZK_merged$adapt_cop_14)

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_14, K_168_sci_gla2, Z_101_sci_gla2, sep = "") %>%
  mutate(adapt_cop_14 = gsub("NA", "", adapt_cop_14)) %>%
  mutate(adapt_cop_14 = labelled(as.numeric(factor(adapt_cop_14)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_14 <- set_label(ZK_merged$adapt_cop_14, "Gebete helfen mir dabei, mit Stress und Bedrohungen umzugehen.")


# adapt_cop_15
Kadur_413$K_178_sci_gla3
Ziessler_229$Z_102_sci_gla3

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_15, K_178_sci_gla3, Z_102_sci_gla3, sep = "") %>%
  mutate(adapt_cop_15 = gsub("NA", "", adapt_cop_15)) %>%
  mutate(adapt_cop_15 = labelled(as.numeric(factor(adapt_cop_15)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_15 <- set_label(ZK_merged$adapt_cop_15, "Egal wie schlimm es wird, ich vertraue auf höhere Mächte.")

#var_labels(ZK_merged$adapt_cop_15)

# adapt_cop_16
Kadur_413$K_159_sci_gla4
Ziessler_229$Z_110_sci_gla4

ZK_merged <- ZK_merged %>%
  unite(adapt_cop_16, K_159_sci_gla4, Z_110_sci_gla4, sep = "") %>%
  mutate(adapt_cop_16 = gsub("NA", "", adapt_cop_16)) %>%
  mutate(adapt_cop_16 = labelled(as.numeric(factor(adapt_cop_16)), 
                                 labels = c("trifft gar nicht zu 1" = 1, 
                                            "trifft eher nicht zu 2" = 2, 
                                            "trifft eher zu 3" = 3, 
                                            "trifft genau zu 4" = 4)))
ZK_merged$adapt_cop_16 <- set_label(ZK_merged$adapt_cop_16, "Bei Stress und Druck erinnere ich mich daran, dass es höhere Werte im Leben gibt.")

#var_labels(ZK_merged$adapt_cop_16)


### maladaptive coping strategies [4 items] (items 5, 10 [reversed], 13, and 15) 
# K_174_sci_alk1, K_164_sci_alk2, K_170_sci_alk3, K_161_sci_alk4
# Z_103_sci_alk1, Z_106_sci_alk2, Z_94_sci_alk3, Z_112_sci_alk4
# maladapt_cop_01
Kadur_413$K_174_sci_alk1
Ziessler_229$Z_103_sci_alk1

ZK_merged <- ZK_merged %>%
  unite(maladapt_cop_01, K_174_sci_alk1, Z_103_sci_alk1, sep = "") %>%
  mutate(maladapt_cop_01 = gsub("NA", "", maladapt_cop_01)) %>%
  mutate(maladapt_cop_01 = labelled(as.numeric(factor(maladapt_cop_01)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$maladapt_cop_01 <- set_label(ZK_merged$maladapt_cop_01, "Wenn mir alles zu viel wird, greife ich manchmal zur Flasche.")

#var_labels(ZK_merged$maladapt_cop_01)


# maladapt_cop_02
Kadur_413$K_164_sci_alk2
Ziessler_229$Z_106_sci_alk2

ZK_merged <- ZK_merged %>%
  unite(maladapt_cop_02, K_164_sci_alk2, Z_106_sci_alk2, sep = "") %>%
  mutate(maladapt_cop_02 = gsub("NA", "", maladapt_cop_02)) %>%
  mutate(maladapt_cop_02 = labelled(as.numeric(factor(maladapt_cop_02)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$maladapt_cop_02 <- set_label(ZK_merged$maladapt_cop_02, "Bei Stress und Druck entspanne ich mich abends mit einem Glas Wein oder Bier.")

#var_labels(ZK_merged$maladapt_cop_02)


# maladapt_cop_03 (reversed, muss noch umgepolt werden)
Kadur_413$K_170_sci_alk3
Ziessler_229$Z_94_sci_alk3

ZK_merged <- ZK_merged %>%
  unite(maladapt_cop_03, K_170_sci_alk3, Z_94_sci_alk3, sep = "") %>%
  mutate(maladapt_cop_03 = gsub("NA", "", maladapt_cop_03)) %>%
  mutate(maladapt_cop_03 = labelled(as.numeric(factor(maladapt_cop_03)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$maladapt_cop_03 <- set_label(ZK_merged$maladapt_cop_03, "Egal wie groß der Stress wird, ich würde niemals wegen Stress zu Alkohol oder Zigaretten greifen.")

#var_labels(ZK_merged$maladapt_cop_03)

# maladapt_cop_04
Kadur_413$K_161_sci_alk4
Ziessler_229$Z_112_sci_alk4

ZK_merged <- ZK_merged %>%
  unite(maladapt_cop_04, K_161_sci_alk4, Z_112_sci_alk4, sep = "") %>%
  mutate(maladapt_cop_04 = gsub("NA", "", maladapt_cop_04)) %>%
  mutate(maladapt_cop_04 = labelled(as.numeric(factor(maladapt_cop_04)), 
                              labels = c("stimmt überhaupt nicht 1" = 1, 
                                         "2" = 2, 
                                         "3" = 3, 
                                         "neutral 4" = 4, 
                                         "5" = 5,
                                         "6" = 6,
                                         "stimmt vollkommen 7" = 7)))
ZK_merged$maladapt_cop_04 <- set_label(ZK_merged$maladapt_cop_04, "Wenn ich zu viel Stress habe, rauche ich eine Zigarette.")

#var_labels(ZK_merged$maladapt_cop_04)




##### Questionnaire scores #####################################################

# Create data frame "final_ZK_data" to feed the values of every subject into


### Recode all the reverse coded items
# "pe" to "reduced personal efficacy" -> thus, high scores in this burnout dimension also mean high burnout expression. 
# Recode from 1-6 to 6-1
ZK_merged <- ZK_merged %>%
  mutate(across(starts_with("mbi_pe"), ~ 7 - ., .names = "mbi_rpe{str_remove(.col, 'mbi_pe')}"))



# NFC
# Recode from -3 - +3 to 3 - (-3)
#nfc_04, nfc_06, nfc_07, nfc_08, nfc_09, nfc_10, nfc_11, nfc_12, nfc_15, nfc_16
ZK_merged$nfc_04_recoded <- recode(ZK_merged$nfc_04, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_06_recoded <- recode(ZK_merged$nfc_06, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_07_recoded <- recode(ZK_merged$nfc_07, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_08_recoded <- recode(ZK_merged$nfc_08, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_09_recoded <- recode(ZK_merged$nfc_09, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_10_recoded <- recode(ZK_merged$nfc_10, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_11_recoded <- recode(ZK_merged$nfc_11, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_12_recoded <- recode(ZK_merged$nfc_12, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_15_recoded <- recode(ZK_merged$nfc_15, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")
ZK_merged$nfc_16_recoded <- recode(ZK_merged$nfc_16, "-3=3; -2=2; -1=1; 0=0; 1=-1; 2=-2; 3=-3")


# Self-control (9 reversed items)
#sc_02, sc_03, sc_04, sc_05, sc_06, sc_07, sc_08, sc_10, sc_11
ZK_merged$sc_02_recoded <- recode(ZK_merged$sc_02, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_03_recoded <- recode(ZK_merged$sc_03, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_04_recoded <- recode(ZK_merged$sc_04, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_05_recoded <- recode(ZK_merged$sc_05, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_06_recoded <- recode(ZK_merged$sc_06, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_07_recoded <- recode(ZK_merged$sc_07, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_08_recoded <- recode(ZK_merged$sc_08, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_10_recoded <- recode(ZK_merged$sc_10, "5=1; 4=2; 3=3; 2=4; 1=5")
ZK_merged$sc_11_recoded <- recode(ZK_merged$sc_11, "5=1; 4=2; 3=3; 2=4; 1=5")


# (Mal-/adaptive) Coping strategies strategies (1 reversed item)
ZK_merged$maladapt_cop_03
ZK_merged$maladapt_cop_03_recoded <- recode(ZK_merged$maladapt_cop_03, "7=1; 6=2; 5=3; 4=4; 3=5; 2=6; 1=7")
ZK_merged$maladapt_cop_03_recoded



### Summenscores bilden (bei Ziessler keine solche Spalten, nur Mittelwerte):
# EE: "K_180_MBI_EE"
# DE: "K_181_MBI_DE"
# PE: "K_182_MBI_PE
# NFC: "K_187_NFC2"
# SC: "K_190_SCS"
# ERS... 
#  ERS_RE: "K_188_ERQ_NB"
#  ERS_SU: "K_189_ERQ_UD"              
# Adapt_Cop: K_192_SCI_POS" + "K_193_SCI_AKT" + "K_194_SCI_SUP" + "K_195_SCI_GLA"          
# Maladapt_Cop: "K_196_SCI_ALK


## MBI; 1) EE
#mbi_ee01, mbi_ee02,mbi_ee03, mbi_ee04, mbi_ee05, mbi_ee06, mbi_ee07,mbi_ee08,mbi_ee09

## MBI; 2) rPE
# mbi_rpe01, mbi_rpe02, mbi_rpe03, mbi_rpe04, mbi_rpe05, mbi_rpe06, mbi_rpe07, mbi_rpe08

## MBI; 3) D
# mbi_de01, mbi_de02, mbi_de03, mbi_de04, mbi_de05

## ERS; 1) Re: ers_re_01,ers_re_02,ers_re_03,ers_re_04,ers_re_05,ers_re_06
## ERS; 2) Su: ers_su_01, ers_su_02, ers_su_03, ers_su_04

## NFC: nfc_01,nfc_02,nfc_03,nfc_04_recoded,nfc_05,nfc_06_recoded,nfc_07_recoded,nfc_08_recoded,
# nfc_09_recoded,nfc_10_recoded,nfc_11_recoded,nfc_12_recoded,nfc_13,nfc_14,nfc_15_recoded,nfc_16_recoded

## SC:  sc_01,sc_02_recoded,sc_03_recoded,sc_04_recoded,sc_05_recoded,sc_06_recoded,sc_07_recoded,
# sc_08_recoded, sc_09,sc_10_recoded,sc_11_recoded,sc_12,sc_13

## adapt_cop: adapt_cop_01, adapt_cop_02, adapt_cop_03, adapt_cop_04, adapt_cop_05, adapt_cop_06, adapt_cop_07, 
# adapt_cop_08, adapt_cop_09, adapt_cop_10, adapt_cop_11, adapt_cop_12, adapt_cop_13, adapt_cop_14, adapt_cop_15
# adapt_cop_16

## maladapt_cop: maladapt_cop_01, maladapt_cop_02, maladapt_cop_03_recoded, maladapt_cop_04



### Dataframe "mediation_dataframe_ZK", mit dem Mediation durchgeführt wird
dataanalysis_dataframe_ZK <- ZK_merged %>%
  select(subject_code, sex, female, male, age, federal_state, EastGermany, WestGermany,
         marital_status, NotMarried, Married,
         children_household, Children, NoChildren,
         profession, ProfessionCategories, TherapeuticProfessions, ManagementProfessions, Volunteers_PersonsinEducation,
         mbi_ee01, mbi_ee02,mbi_ee03, mbi_ee04, mbi_ee05, mbi_ee06, mbi_ee07,mbi_ee08,mbi_ee09,
         mbi_rpe01, mbi_rpe02, mbi_rpe03, mbi_rpe04, mbi_rpe05, mbi_rpe06, mbi_rpe07, mbi_rpe08,
         mbi_de01, mbi_de02, mbi_de03, mbi_de04, mbi_de05,
         nfc_01,nfc_02,nfc_03,nfc_04_recoded,nfc_05,nfc_06_recoded,nfc_07_recoded,nfc_08_recoded,
         nfc_09_recoded,nfc_10_recoded,nfc_11_recoded,nfc_12_recoded,nfc_13,nfc_14,nfc_15_recoded,nfc_16_recoded,
         ers_re_01,ers_re_02,ers_re_03,ers_re_04,ers_re_05,ers_re_06,
         ers_su_01, ers_su_02, ers_su_03, ers_su_04,
         sc_01,sc_02_recoded,sc_03_recoded,sc_04_recoded,sc_05_recoded,sc_06_recoded,sc_07_recoded,sc_08_recoded,
         sc_09,sc_10_recoded,sc_11_recoded,sc_12,sc_13,
         adapt_cop_01, adapt_cop_02, adapt_cop_03, adapt_cop_04, adapt_cop_05, adapt_cop_06, adapt_cop_07,
         adapt_cop_08, adapt_cop_09, adapt_cop_10, adapt_cop_11, adapt_cop_12, adapt_cop_13, adapt_cop_14, adapt_cop_15, 
         adapt_cop_16, maladapt_cop_01, maladapt_cop_02, maladapt_cop_03_recoded, maladapt_cop_04)



# Anzeige aller Spalten im Dataframe
print(dataanalysis_dataframe_ZK, width = Inf)

# Speichern des Dataframes 'mediation_dataframe_ZK' als R-Datei
saveRDS(dataanalysis_dataframe_ZK, file = "dataanalysis_dataframe_ZK.RDS")

data_ZK <- dataanalysis_dataframe_ZK
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)




