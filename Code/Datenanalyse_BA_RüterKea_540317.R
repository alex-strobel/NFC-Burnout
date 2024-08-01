
rm(list = ls())

############## Data analysis

### Required packages
#install.packages("psych")
#install.packages("lavaan", dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("MBESS")
#install.packages("readxl")
#install.packages("GPArotation")
#install.packages("Hmisc")
library(Hmisc)
library(GPArotation)
library(readxl)
library(MBESS)
library(lavaan)
library(psych)
library(ggplot2)

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
#install.packages("GGally")
library(GGally)
library(car)
library(tidyverse)  
library(here)       
library(labelled)
library(sjlabelled)



### Load dataframe 'dataanalysis_dataframe_ZK.RDS'
setwd("C:\\Users\\kearu\\Desktop\\Uni gönnung\\BA\\2 Datenanalyse")
#getwd()
data_ZK <- readRDS("dataanalysis_dataframe_ZK.RDS")



### Seed for random number generation
#set.seed(13)



##### prepare data frame ################################################

### 1. Put questionnaire data in new dataframes (reversed Items already have been reversed)
## 1.1 With outliers
mbi <- data_ZK[,grep("mbi", colnames(data_ZK))]        # Maslach Burnout Inventory
ncs <- data_ZK[,grep("nfc", colnames(data_ZK))]        # Need for Cognition Scale
scs <- data_ZK[,grep("sc", colnames(data_ZK))]         # Self Control Scale
ind <- grep("ers", colnames(data_ZK))
ind <- ind[!colnames(data_ZK)[ind] == "Volunteers_PersonsinEducation"]
erq <- data_ZK[, ind]                                  # Emotion Regulation Questionnaire
sci <- data_ZK[,grep("adapt_cop", colnames(data_ZK))]  # Stress and Coping Inventory


### 2. Create a list with keys for the questionnaire subscales/item parcels
numbers_ZK <- data_ZK
for (i in 1:ncol(numbers_ZK)) {
  colnames(numbers_ZK)[i] <- paste0(i, "_", colnames(numbers_ZK)[i])
}

keylist <- list(mbi_ee = c(1,2,3,4,5,6,7,8,9), mbi_rpe = c(10,11,12,13,14,15,16,17), mbi_de = c(18,19,20,21,22),
                ncs = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                scs = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                erq = c(1,2,3,4,5,6,7,8,9,10),
                erq_reap = c(1,2,3,4,5,6), erq_supp = c(7,8,9,10), 
                sci = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                sci_adapt = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), sci_maladapt = c(17,18,19,20))



### 3. Create data frame to feed the values of every subject into
score_ZK <- data.frame(id = c(1:642),
                       subject_code = data_ZK$subject_code,
                       sex = data_ZK$sex, #1=female, 2=male, 3=divers
                       age = data_ZK$age,
                       WestGermanState = data_ZK$WestGermany, #1=West German Federal State, 2=East German Federal State
                       children = data_ZK$Children, #1=children live in the household, 0=no children
                       married = data_ZK$Married, #1=married, 0=not married
                       professioncategory = data_ZK$ProfessionCategories, #1=TherapeuticProfessions, 2=ManagementProfessions, 3=VolunteersPersonsinEducation
                       ManagementProfessions = data_ZK$ManagementProfessions,
                       VolunteersPersonsinEducation = data_ZK$Volunteers_PersonsinEducation,
                       mbi = rep(0,642),
                       mbi_ee = rep(0,642), mbi_rpe = rep(0,642), mbi_de = rep(0,642),
                       nfc = rep(0,642),
                       scs = rep(0,642),
                       erq = rep(0,642),
                       erq_reap = rep(0,642), erq_supp = rep(0,642),
                       sci = rep(0,642),
                       sci_adapt = rep(0,642), sci_maladapt = rep(0,642))

# Compute individual sum scores and feed in raw values
for (i in 1:nrow(data_ZK)) {
  score_ZK[i,c(11:ncol(score_ZK))] <- c(rowSums(mbi[i,]),
                                        rowSums(mbi[i,keylist$mbi_ee]), rowSums(mbi[i,keylist$mbi_rpe]), rowSums(mbi[i,keylist$mbi_de]),
                                        rowSums(ncs[i,]),
                                        rowSums(scs[i,]),
                                        rowSums(erq[i,]),
                                        rowSums(erq[i,keylist$erq_reap]), rowSums(erq[i,keylist$erq_supp]),
                                        rowSums(sci[i,]),
                                        rowSums(sci[i,keylist$sci_adapt]), rowSums(sci[i,keylist$sci_maladapt]))
}



##### Descriptive data analysis ################################################

##### 1. Check for outliers
## https://statologie.de/ausreisser-r/

## 1.1 Check outliers in intervall variables
continousOutlier_ZK <- score_ZK %>% select(age, mbi, mbi_ee, mbi_rpe, mbi_de, nfc, scs, erq, erq_reap, erq_supp, sci, sci_adapt, sci_maladapt)
#boxplot(continousOutlier_ZK, outline = TRUE, col = "lightblue", main = "boxplots of all continous variables and control variables")
# boxplot: shows some outliers -> I will check them individually with the z-score method below

## z-score method: outlier if observation has a z-score of less than -3 or more than 3
outlier_columns <- c("age", "mbi", "mbi_ee", "mbi_rpe", "mbi_de", "nfc", "scs", "erq", "erq_reap", 
                      "erq_supp", "sci", "sci_adapt", "sci_maladapt")
z_scores <- score_ZK[, c("subject_code", outlier_columns)]
z_scores[, outlier_columns] <- apply(z_scores[, outlier_columns], 2, function(x) (x - mean(x)) / sd(x))

# I checked all observations that hasn't met the criteria individually
# age: AL1801, EK1709; age = 78 -> don't want to remove them
# mbi: GM0904 (further 2 will be ignored, because they have score of 3.08 and 3.01)
# mbi_rpe_sum: KF2107, KG2701, Mw0905 (further above 3 will be ignored, because they have score of 3.092)
# mbi_de: BE1705
# scs: MK2406 (further above 3 will be ignored, because they have score of 3.074)
# erq: GB2403, Ig0901
# erq_reap: GB2403 (twice), Ig0901 (twice), Kg2902
# sci: SC0106
# sci_adapt: KF2107 (twice), HS1711, YG2106
# sci_maladapt: RQ1608, CK2209

# NOTE: Due to the fact that this control variable will be dummy-coded in the mediation analysis, not only the sample without outliers but also the sample outliers included must be calculated without
# the group "diverse" (no sufficient group size of >30). Thus, subject_code EL3108 needs to be reversed as soon as the mediation model is calculated with the control variable "sex".
# Remove row with subject_code EL3108
score_ZK_controlGender <- subset(score_ZK, subject_code != "EL3108")
dim(score_ZK_controlGender)

# Assign new labels for column "sex"
score_ZK_controlGender$sex <- labelled(as.numeric(factor(score_ZK_controlGender$sex)), labels = c(female=2, male=1))
table(score_ZK_controlGender$sex)
score_ZK_controlGender$sex

score_ZK_controlGender$female <- ifelse(score_ZK_controlGender$sex %in% c(1), 1, 0)
score_ZK_controlGender$male <- ifelse(score_ZK_controlGender$sex %in% c(2), 1, 0)



## 1.2 Check outliers in categorial variables --> categorical variables don't seem conspicuous
#ggplot(data_ZK, aes(x = WestGermany)) +
#  geom_bar(fill = "steelblue") +
#  labs(title = "Distribution of East and West German federal states") +
#  xlab("0= East Germany 1=West Germany") +
#  ylab("amount")
#table(data_ZK$WestGermany)

#ggplot(data_ZK, aes(x = Married)) +
#  geom_bar(fill = "steelblue") +
#  labs(title = "Distribution of marital status") +
#  xlab("0=not married 1=married") +
#  ylab("amount")
#table(data_ZK$Married)

#ggplot(data_ZK, aes(x = ProfessionCategories)) +
#  geom_bar(fill = "steelblue") +
#  labs(title = "Distribution of the profession categories") +
#  xlab("1=TherapeuticProfessions 2=ManagementProfessions 3=Volunteers/Persons in Education") +
#  ylab("amount")
#table(data_ZK$ProfessionCategories)





## -> For first analysis I would let in all "outliers" (16) and then try mediation analysis without them
## removed outliers for second analysis (16 outliers): EL3108, AL1801, EK1709, GM0904, BE1705, KF2107, KG2701, Mw0905, MK2406, GB2403, Ig0901, Kg2902, HS1711, YG2106, RQ1608, CK2209
outliers_to_remove <- c("EL3108", "AL1801", "EK1709", "GM0904", "BE1705", "KF2107", "KG2701", "Mw0905", "MK2406", "GB2403", "Ig0901", "Kg2902", "SC0106", "HS1711", "YG2106", "RQ1608", "CK2209")

# Create data frame "data_ZK_woOutliers"
score_ZK_woOutliers <- subset(score_ZK, !subject_code %in% outliers_to_remove)
data_ZK_woOutliers <- subset(data_ZK, !subject_code %in% outliers_to_remove)

#dim(score_ZK)
dim(score_ZK_woOutliers)

mbi_woOutliers <- data_ZK_woOutliers[,grep("mbi", colnames(data_ZK_woOutliers))]        # Maslach Burnout Inventory
ncs_woOutliers <- data_ZK_woOutliers[,grep("nfc", colnames(data_ZK_woOutliers))]        # Need for Cognition Scale
scs_woOutliers <- data_ZK_woOutliers[,grep("sc", colnames(data_ZK_woOutliers))]         # Self Control Scale
erq_woOutliers <- data_ZK_woOutliers[,grep("ers", colnames(data_ZK_woOutliers))]        # Emotion Regulation Questionnaire
sci_woOutliers <- data_ZK_woOutliers[,grep("adapt_cop", colnames(data_ZK_woOutliers))]  # Stress and Coping Inventory



##### 2. Check (non-)linear associations
## Function "my_fn" was taken from an answer on a Stackoverflow question on June 20th 2023
## https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function/35088740
# NOTE: No major changes when correlation matrix is plotted without outliers.

my_fn <- function(data, mapping, pts=list(), smt=list(), ...){
  ggplot(data = data, mapping = mapping, ...) +
    do.call(geom_point, pts) +
    do.call(geom_smooth, smt)
}

# Plot a big correlational matrix (for all interval variables) to check for non-linear associations
multicorrplot <- GGally::ggpairs(score_ZK[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc","scs","erq",
                                             "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], lower = list(continuous = my_fn))

noOutlier_multicorrplot <- GGally::ggpairs(score_ZK_woOutliers[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc","scs","erq",
                                             "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], lower = list(continuous = my_fn))
#multicorrplot
#noOutlier_multicorrplot



##### 3. Check for normal distribution 
## Mardia's test for multivariate normal distribution
# NOTE: No variable shows normal distribution, neither the data frame with nor without outliers. However, due to the sample size of N > 30, a sufficient and based on
# the central limit theorem (Bortz & Schuster, 2010) normal distribution of the characteristics can be assumed.

normaldistribution_test <- MVN::mvn(score_ZK[c("age","mbi","mbi_ee","mbi_rpe","mbi_de","nfc","scs","erq",
                                               "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")],
                        mvnTest = "mardia", covariance = TRUE, desc = TRUE, univariateTest = "SW")
noOutlier_normaldistribution_test <- MVN::mvn(score_ZK_woOutliers[c("age","mbi","mbi_ee","mbi_rpe","mbi_de","nfc","scs","erq",
                                               "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")],
                                    mvnTest = "mardia", covariance = TRUE, desc = TRUE, univariateTest = "SW")

normaldistribution_test
noOutlier_normaldistribution_test



#### 4. Categorical descriptive statistics: data frame with percentages of responses per category (sex, federal_state, marital_status, profession)
### 4.1 With outlierts
desc_cat <- data.frame(
  answer = c(0, 1, 2, 3),
  sex = c(
    nrow(data_ZK[data_ZK$male == 1, ]) / nrow(data_ZK),
    nrow(data_ZK[data_ZK$female == 1, ]) / nrow(data_ZK),
    0, 0
  ),
  federal_state = c(
    nrow(data_ZK[data_ZK$EastGermany == 1, ]) / nrow(data_ZK),
    nrow(data_ZK[data_ZK$WestGermany == 1, ]) / nrow(data_ZK),
    0, 0
  ),
  marital_status = c(
    nrow(data_ZK[data_ZK$NotMarried == 1, ]) / nrow(data_ZK),
    nrow(data_ZK[data_ZK$Married == 1, ]) / nrow(data_ZK),
    0, 0
  ),
  profession = c(
    0,
    nrow(data_ZK[data_ZK$TherapeuticProfessions == 1, ]) / nrow(data_ZK),
    nrow(data_ZK[data_ZK$ManagementProfessions == 1, ]) / nrow(data_ZK),
    nrow(data_ZK[data_ZK$Volunteers_PersonsinEducation == 1, ]) / nrow(data_ZK)
  )
)

desc_cat

### 4.2 Without outlierts
## NOTE: Distribution is less homogeneous without outliers
desc_cat_woOutliers <- data.frame(
  answer = c(0, 1, 2, 3),
  sex = c(
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$male == 1, ]) / nrow(data_ZK_woOutliers),
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$female == 1, ]) / nrow(data_ZK_woOutliers),
    0, 0
  ),
  federal_state = c(
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$EastGermany == 1, ]) / nrow(data_ZK_woOutliers),
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$WestGermany == 1, ]) / nrow(data_ZK_woOutliers),
    0, 0
  ),
  marital_status = c(
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$NotMarried == 1, ]) / nrow(data_ZK_woOutliers),
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$Married == 1, ]) / nrow(data_ZK_woOutliers),
    0, 0
  ),
  profession = c(
    0,
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$TherapeuticProfessions == 1, ]) / nrow(data_ZK_woOutliers),
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$ManagementProfessions == 1, ]) / nrow(data_ZK_woOutliers),
    nrow(data_ZK_woOutliers[data_ZK_woOutliers$Volunteers_PersonsinEducation == 1, ]) / nrow(data_ZK_woOutliers)
  )
)

#desc_cat_woOutliers



##### 4. Metric descriptive statistics: data frame with descriptive values of questionnaires
desc_met <- data.frame(Variable = c("AGE", "MBI","MBI EE","MBI RPE","MBI DE","NFC","SCS","ERQ","ERQ RE","ERQ SU", "SCI", "SCI ADAPT COP","SCI MALADAPT COP"),
                       Mean = normaldistribution_test$Descriptives$Mean,
                       SD = normaldistribution_test$Descriptives$Std.Dev,
                       Minimum = sapply(score_ZK[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc", "scs","erq","erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], min),
                       Maximum = sapply(score_ZK[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc", "scs","erq","erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], max),
                       Normality = normaldistribution_test$univariateNormality$Normality,
                       Skewness = normaldistribution_test$Descriptives$Skew,
                       Kurtosis = normaldistribution_test$Descriptives$Kurtosis)
desc_met

desc_met_woOutliers <- data.frame(Variable = c("AGE", "MBI","MBI EE","MBI RPE","MBI DE","NFC","SCS","ERQ","ERQ RE","ERQ SU", "SCI","SCI ADAPT COP","SCI MALADAPT COP"),
                       Mean = normaldistribution_test$Descriptives$Mean,
                       SD = normaldistribution_test$Descriptives$Std.Dev,
                       Minimum = sapply(score_ZK_woOutliers[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc", "scs","erq","erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], min),
                       Maximum = sapply(score_ZK_woOutliers[,c("age", "mbi","mbi_ee","mbi_rpe","mbi_de","nfc", "scs","erq","erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")], max),
                       Normality = normaldistribution_test$univariateNormality$Normality,
                       Skewness = normaldistribution_test$Descriptives$Skew,
                       Kurtosis = normaldistribution_test$Descriptives$Kurtosis)
desc_met_woOutliers

# Drop the row names
rownames(desc_met) <- NULL
rownames(desc_met_woOutliers) <- NULL

# Replace the capital YESes and NOs with small ones
desc_met$Normality[grep("NO", desc_met$Normality)] <- "No"
desc_met$Normality[grep("YES", desc_met$Normality)] <- "Yes"

desc_met$Normality[grep("NO", desc_met_woOutliers$Normality)] <- "No"
desc_met$Normality[grep("YES", desc_met_woOutliers$Normality)] <- "Yes"

# Round the values
desc_met[ ,c("Mean","SD","Skewness","Kurtosis")] <- round(desc_met[ ,c("Mean","SD","Skewness","Kurtosis")], digits = 2)
desc_met_woOutliers[ ,c("Mean","SD","Skewness","Kurtosis")] <- round(desc_met_woOutliers[ ,c("Mean","SD","Skewness","Kurtosis")], digits = 2)

desc_met
desc_met_woOutliers
dim(score_ZK)



##### 6. Sample description

### 6.1 Federal State
## W/ outliers
# Häufigkeit der Werte in der Spalte
frequency_federal_state <- table(data_ZK$federal_state)
# Berechnen der Prozentzahlen der Werte in der Spalte
percentage_federal_state <- prop.table(frequency_federal_state) * 100
# Kombinieren der Ergebnisse
descriptives_federal_state <- data.frame(federal_state = names(frequency_federal_state),
                                         count = as.vector(frequency_federal_state),
                                         percentage = as.vector(percentage_federal_state))
print(descriptives_federal_state)

## w/o outliers
frequency_federal_state_woO <- table(data_ZK_woOutliers$federal_state)
percentage_federal_state_woO <- prop.table(frequency_federal_state_woO) * 100
descriptives_federal_state_woO <- data.frame(federal_state = names(frequency_federal_state_woO),
                                             count = as.vector(frequency_federal_state_woO),
                                             percentage = as.vector(percentage_federal_state_woO))
print(descriptives_federal_state_woO)
descriptives_fs <- merge(descriptives_federal_state, descriptives_federal_state_woO, by = "federal_state")
descriptives_fs


### 6.2 Children
## W/ outliers
frequency_children <- table(data_ZK$children_household)
percentage_children <- prop.table(frequency_children) * 100
descriptives_children <- data.frame(children_household = names(frequency_children),
                                    count = as.vector(frequency_children),
                                    percentage = as.vector(percentage_children))
print(descriptives_children)

## w/o outliers
frequency_children_woO <- table(data_ZK_woOutliers$children_household)
percentage_children_woO <- prop.table(frequency_children_woO) * 100
descriptives_children_woO <- data.frame(children_household = names(frequency_children_woO),
                                        count = as.vector(frequency_children_woO),
                                        percentage = as.vector(percentage_children_woO))
print(descriptives_children_woO)

descriptives_childrenhousehold <- merge(descriptives_children, descriptives_children_woO, by = "children_household")
descriptives_childrenhousehold
data_ZK$children_household

### 6.3 Sex
table(data_ZK$sex)
table(data_ZK_woOutliers$sex)


### 6.4 Profession
data_ZK$profession
## w/ outliers
frequency_profession <- table(data_ZK$profession)
percentage_profession <- prop.table(frequency_profession) * 100
descriptives_profession <- data.frame(profession = names(frequency_profession),
                                    count = as.vector(frequency_profession),
                                    percentage = as.vector(percentage_profession))
print(descriptives_profession)

# Profession Categories w/ Outliers
frequency_ProfessionCategories <- table(data_ZK$ProfessionCategories)
percentage_ProfessionCategories <- prop.table(frequency_ProfessionCategories) * 100
descriptives_ProfessionCategories <- data.frame(profession = names(frequency_ProfessionCategories),
                                      count = as.vector(frequency_ProfessionCategories),
                                      percentage = as.vector(percentage_ProfessionCategories))
print(descriptives_ProfessionCategories)


## w/o outliers
frequency_profession_woO <- table(data_ZK_woOutliers$profession)
percentage_profession_woO <- prop.table(frequency_profession_woO) * 100
descriptives_profession_woO <- data.frame(profession = names(frequency_profession_woO),
                                      count = as.vector(frequency_profession_woO),
                                      percentage = as.vector(percentage_profession_woO))
print(descriptives_profession_woO)

# Profession Categories w/o Outliers
frequency_ProfessionCategories_woO <- table(data_ZK_woOutliers$ProfessionCategories)
percentage_ProfessionCategories_woO <- prop.table(frequency_ProfessionCategories_woO) * 100
descriptives_ProfessionCategories_woO <- data.frame(profession = names(frequency_ProfessionCategories_woO),
                                                count = as.vector(frequency_ProfessionCategories_woO),
                                                percentage = as.vector(percentage_ProfessionCategories_woO))
print(descriptives_ProfessionCategories_woO)


descriptives_profes_category <- merge(descriptives_ProfessionCategories, descriptives_ProfessionCategories_woO, by = "profession")
descriptives_profes_category


### 6.5 Marital status
data_ZK$Married
## w/ outliers
frequency_maritalStatus <- table(data_ZK$Married)
percentage_maritalStatus <- prop.table(frequency_maritalStatus) * 100
descriptives_maritalStatus <- data.frame(married = names(frequency_maritalStatus),
                                      count = as.vector(frequency_maritalStatus),
                                      percentage = as.vector(percentage_maritalStatus))
print(descriptives_maritalStatus)





##### Tests for internal consistency ###########################################

### Compute Cronbach's Alpha and MacDonald's Omega for questionnaires
# Note: All warning messages in respect of reversed items can be ignored. All relevant items already have been reversed.

## Maslach Burnout Inventory (MBI)
# NOTE: Higher Chronbach's Alpha when outliers are not removed
omega(mbi)
omega(mbi_woOutliers)

# MBI reduced personal efficacy (rpe)
# NOTE: Higher Chronbach's Alpha when outliers are not removed
mbi_rpe_columns <- data_ZK[, grepl("^mbi_rpe", colnames(data_ZK))]
mbi_rpe_columns_woOutliers <- data_ZK_woOutliers[, grepl("^mbi_rpe", colnames(data_ZK_woOutliers))]
omega(mbi_rpe_columns)
omega(mbi_rpe_columns_woOutliers)

# MBI emotional exhaustion (ee)
# NOTE: Chronbach's Alhpa does not change when outliers are removed
mbi_ee_columns <- data_ZK[, grepl("^mbi_ee", colnames(data_ZK))]
mbi_ee_columns_woOutliers <- data_ZK_woOutliers[, grepl("^mbi_ee", colnames(data_ZK_woOutliers))]
omega(mbi_ee_columns)
omega(mbi_ee_columns_woOutliers)

# MBI depersonalization (de)
# NOTE: Chronbach's Alhpa does not change when outliers are removed
mbi_de_columns <- data_ZK[, grepl("^mbi_de", colnames(data_ZK))]
mbi_de_columns_woOutliers <- data_ZK_woOutliers[, grepl("^mbi_de", colnames(data_ZK_woOutliers))]
omega(mbi_de_columns)
omega(mbi_de_columns_woOutliers)


# Need for Cognition Scale
# NOTE: Chronbach's Alpha is a little higher when outliers are removed
omega(ncs)
omega(ncs_woOutliers)


## Self Control Scale
# NOTE: Higher Chronbach's Alpha when outliers are not removed
omega(scs)
omega(scs_woOutliers)

 
## ERQ
# NOTE: Higher Chronbach's Alpha when outliers are not removed
omega(erq)
omega(erq_woOutliers)

# ERQ reappraisal
# NOTE: Higher Chronbach's Alpha when outliers are not removed
erq_re_columns <- data_ZK[, grepl("^ers_re", colnames(data_ZK))]
erq_re_columns_woOutliers <- data_ZK_woOutliers[, grepl("^ers_re", colnames(data_ZK_woOutliers))]
omega(erq_re_columns)
omega(erq_re_columns_woOutliers)
 
# ERQ suppression
# NOTE: Chronbach's Alhpa does not change when outliers are removed
erq_su_columns <- data_ZK[, grepl("^ers_su", colnames(data_ZK))]
erq_su_columns_woOutliers <- data_ZK_woOutliers[, grepl("^ers_su", colnames(data_ZK_woOutliers))]
omega(erq_su_columns)
omega(erq_su_columns_woOutliers)


# SCI
# NOTE: Higher Chronbach's Alpha when outliers are not removed
omega(sci)
omega(sci_woOutliers)
 
# SCI adaptive coping 
sci_adapt_columns <- data_ZK[, grepl("^adapt_cop", colnames(data_ZK))]
sci_adapt_columns_woOutliers <- data_ZK[, grepl("^adapt_cop", colnames(data_ZK_woOutliers))]
omega(sci_adapt_columns)
omega(sci_adapt_columns_woOutliers)

# SCI maladaptive coping 
sci_maladapt_columns <- data_ZK[, grepl("^maladapt_cop", colnames(data_ZK))]
sci_maladapt_columns_woOutliers <- data_ZK[, grepl("^maladapt_cop", colnames(data_ZK_woOutliers))]
omega(sci_maladapt_columns)
omega(sci_maladapt_columns_woOutliers)



###### Correlations #############################################################

#### Spearman's correlation coefficient (due to missing normal distribution)

# The following function for nicely formatted correlation output was (modified and) taken from
# https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
# on the 22nd of June 2023

correlation_matrix <- function(df,
                               type = "spearman",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = ""){
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = type)
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a html space before the positives to align all
  if (sum(!is.na(R) & R < 0) > 0) {
    Rformatted = ifelse(!is.na(R) & R > 0, paste0(" ", Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
    Rformatted = paste0(Rformatted, stars)
  }
  
  # make all character strings equally long
  max_length = max(nchar(Rformatted))
  Rformatted = vapply(Rformatted, function(x) {
    current_length = nchar(x)
    difference = max_length - current_length
    return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
  }, FUN.VALUE = character(1))
  
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(Rnew) <- colnames(x)
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

# correlate variables
correlations <- correlation_matrix(score_ZK[c("age","mbi","mbi_rpe","mbi_ee","mbi_de","nfc","scs","erq",
                                              "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")],
                                   type = "spearman",
                                   use = "lower",
                                   digits = 2,
                                   show_significance = TRUE,
                                   replace_diagonal = TRUE,
                                   replacement = "")
correlations_woOutliers <- correlation_matrix(score_ZK_woOutliers[c("age","mbi","mbi_rpe","mbi_ee","mbi_de","nfc","scs","erq",
                                              "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")],
                                   type = "spearman",
                                   use = "lower",
                                   digits = 2,
                                   show_significance = TRUE,
                                   replace_diagonal = TRUE,
                                   replacement = "")
correlations
correlations_woOutliers



##### Holm–Bonferroni method for alpha error adjustment
holm_columns <- score_ZK[c("age","mbi","mbi_rpe","mbi_ee","mbi_de","nfc","scs","erq",
                               "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")]

holm_correlation_result <- corr.test(holm_columns, 
                                method = "spearman",  
                                adjust = "holm",       
                                alpha = 0.05)

holm_correlation_result$p <- round(holm_correlation_result$p, digits = 3)

holm_correlation_result
holm_correlation_result$p

### Holm–Bonferroni method for alpha error adjustment outliers excluded
holm_columns_woO <- score_ZK_woOutliers[c("age","mbi","mbi_rpe","mbi_ee","mbi_de","nfc","scs","erq",
                           "erq_reap","erq_supp","sci","sci_adapt","sci_maladapt")]

holm_correlation_result_woO <- corr.test(holm_columns_woO, 
                                     method = "spearman",  
                                     adjust = "holm",       
                                     alpha = 0.05)

holm_correlation_result_woO$p <- round(holm_correlation_result_woO$p, digits = 3)

holm_correlation_result_woO
holm_correlation_result_woO$p



### Chi-Square test for correlation analysis between categorical control variables (sex, children living in the household (yes vs. no), East/West federal state, marital status, profession)
### and interval outcome variable (mbi, mbi_ee, mbi_rpe, mbi_de) as well as mediators (scs, erq_reap, erq_supp, sci_adatp, sci_maladapt)

## 1. sex
# NOTE: Due to the fact that this control variable will be dummy-coded, not only the sample without outliers but also the sample outliers included must be calculated without
# the group "divers" (no sufficient group size of >30). Thus, subject_code EL3108 needs to be reversed anyway.

## relationship found between sex~mbi, sex~mbi_de, sex~erq_supp
#chisq.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi, simulate.p.value=TRUE) # significant (p < .05) -> probably related to each other
#chisq.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_ee)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_rpe)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_de) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_controlGender$sex, score_ZK_controlGender$sci_maladapt, simulate.p.value=TRUE)

## relationship found between sex~mbi_ee, sex~mbi_de, sex~erq_supp
#chisq.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_ee) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_rpe)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_de) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$sex, score_ZK_woOutliers$sci_maladapt, simulate.p.value=TRUE)


## 2. children living
## relationship found between children~scs, children~erq_supp, children~sci_adapt (in data sets with and without outliers)
#chisq.test(score_ZK$children, score_ZK$mbi)
#fisher.test(score_ZK$children, score_ZK$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK$children, score_ZK$mbi_ee)
#fisher.test(score_ZK$children, score_ZK$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK$children, score_ZK$mbi_rpe)
#fisher.test(score_ZK$children, score_ZK$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK$children, score_ZK$mbi_de)
#fisher.test(score_ZK$children, score_ZK$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK$children, score_ZK$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK$children, score_ZK$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK$children, score_ZK$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK$children, score_ZK$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK$children, score_ZK$sci_maladapt, simulate.p.value=TRUE)

#chisq.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_ee)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_rpe)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_de)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$children, score_ZK_woOutliers$sci_maladapt, simulate.p.value=TRUE)


## 3. West/East federal state
## relationship found between WestGermanState~scs, WestGermanState~sci_maladapt
#chisq.test(score_ZK$WestGermanState, score_ZK$mbi)
#fisher.test(score_ZK$WestGermanState, score_ZK$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK$WestGermanState, score_ZK$mbi_ee)
#fisher.test(score_ZK$WestGermanState, score_ZK$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK$WestGermanState, score_ZK$mbi_rpe)
#fisher.test(score_ZK$WestGermanState, score_ZK$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK$WestGermanState, score_ZK$mbi_de)
#fisher.test(score_ZK$WestGermanState, score_ZK$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK$WestGermanState, score_ZK$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK$WestGermanState, score_ZK$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK$WestGermanState, score_ZK$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK$WestGermanState, score_ZK$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK$WestGermanState, score_ZK$sci_maladapt, simulate.p.value=TRUE)

## no relationship found
#chisq.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_ee)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_rpe)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_de)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$mbi_de, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$scs)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$WestGermanState, score_ZK_woOutliers$sci_maladapt, simulate.p.value=TRUE)


## 4. marital status
## relationship found between marital status~mbi_ee, marital status~scs, marital status~erq_supp
#chisq.test(score_ZK$married, score_ZK$mbi)
#fisher.test(score_ZK$married, score_ZK$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK$married, score_ZK$mbi_ee) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK$married, score_ZK$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK$married, score_ZK$mbi_rpe)
#fisher.test(score_ZK$married, score_ZK$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK$married, score_ZK$mbi_de)
#fisher.test(score_ZK$married, score_ZK$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK$married, score_ZK$sci_maladapt, simulate.p.value=TRUE)

## relationship found between marital status~mbi_ee, marital status~scs
#chisq.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_ee) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_rpe)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_de)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$married, score_ZK_woOutliers$sci_maladapt, simulate.p.value=TRUE)


## 5. profession
## no relationship found
#chisq.test(score_ZK$professioncategory, score_ZK$mbi)
#fisher.test(score_ZK$professioncategory, score_ZK$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK$professioncategory, score_ZK$mbi_ee) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK$professioncategory, score_ZK$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK$professioncategory, score_ZK$mbi_rpe)
#fisher.test(score_ZK$professioncategory, score_ZK$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK$professioncategory, score_ZK$mbi_de)
#fisher.test(score_ZK$professioncategory, score_ZK$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK$professioncategory, score_ZK$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK$professioncategory, score_ZK$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK$professioncategory, score_ZK$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK$professioncategory, score_ZK$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK$professioncategory, score_ZK$sci_maladapt, simulate.p.value=TRUE)

## relationship found between profession~mbi_ee, profession~sci_maladaptive
#chisq.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_ee) # significant (p < .05) -> probably related to each other
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_ee, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_rpe)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_rpe, simulate.p.value=TRUE)
#chisq.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_de)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$mbi_de, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$scs, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$erq_reap, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$erq_supp, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$sci_adapt, simulate.p.value=TRUE)
#fisher.test(score_ZK_woOutliers$professioncategory, score_ZK_woOutliers$sci_maladapt, simulate.p.value=TRUE)



###### Replication of the mediation model of Grass et al. (2018) and Zerna et al. (2022) #############################################################

##### 1. For better interpretation of the results, transfer sum scores to scale 0 - 100
score_ZK$mbi_100 <- ((score_ZK$mbi - 27)/(108-27))*100
score_ZK$mbi_ee_100 <- ((score_ZK$mbi_ee - 9)/(54-9))*100
score_ZK$mbi_rpe_100 <- ((score_ZK$mbi_rpe - 8)/(36-8))*100
score_ZK$mbi_de_100 <- ((score_ZK$mbi_de - 5)/(26-5))*100
score_ZK$nfc_100 <- ((score_ZK$nfc + 36)/(45+36))*100
score_ZK$scs_100 <- ((score_ZK$scs - 21)/(65-21))*100
score_ZK$erq_reap_100 <- ((score_ZK$erq_reap - 6)/(42-6))*100
score_ZK$erq_supp_100 <- ((score_ZK$erq_supp - 4)/(28-4))*100
score_ZK$sci_adapt_100 <- ((score_ZK$sci_adapt - 23)/(61-23))*100
score_ZK$sci_maladapt_100 <- ((score_ZK$sci_maladapt - 7)/(19-7))*100


score_ZK$mbi_100
score_ZK$mbi_ee_100
score_ZK$mbi_rpe_100
score_ZK$mbi_de_100
score_ZK$nfc_100
score_ZK$scs_100
score_ZK$erq_reap_100
score_ZK$erq_supp_100
score_ZK$sci_adapt_100
score_ZK$sci_maladapt_100


score_ZK_woOutliers$mbi_100 <- ((score_ZK_woOutliers$mbi - 27)/(101-27))*100
score_ZK_woOutliers$mbi_ee_100 <- ((score_ZK_woOutliers$mbi_ee - 9)/(53-9))*100
score_ZK_woOutliers$mbi_rpe_100 <- ((score_ZK_woOutliers$mbi_rpe - 8)/(32-8))*100
score_ZK_woOutliers$mbi_de_100 <- ((score_ZK_woOutliers$mbi_de - 5)/(24-5))*100
score_ZK_woOutliers$nfc_100 <- ((score_ZK_woOutliers$nfc + 36)/(45+36))*100
score_ZK_woOutliers$scs_100 <- ((score_ZK_woOutliers$scs - 21)/(63-21))*100
score_ZK_woOutliers$erq_reap_100 <- ((score_ZK_woOutliers$erq_reap - 9)/(42-9))*100
score_ZK_woOutliers$erq_supp_100 <- ((score_ZK_woOutliers$erq_supp - 4)/(28-4))*100
score_ZK_woOutliers$sci_adapt_100 <- ((score_ZK_woOutliers$sci_adapt - 26)/(61-26))*100
score_ZK_woOutliers$sci_maladapt_100 <- ((score_ZK_woOutliers$sci_maladapt - 7)/(16-7))*100

score_ZK_woOutliers$mbi_100
score_ZK_woOutliers$mbi_ee_100
score_ZK_woOutliers$mbi_rpe_100
score_ZK_woOutliers$mbi_de_100
score_ZK_woOutliers$nfc_100
score_ZK_woOutliers$scs_100
score_ZK_woOutliers$erq_reap_100
score_ZK_woOutliers$erq_supp_100
score_ZK_woOutliers$sci_adapt_100
score_ZK_woOutliers$sci_maladapt_100


score_ZK_controlGender$mbi_100 <- ((score_ZK_controlGender$mbi - 27)/(108-27))*100
score_ZK_controlGender$mbi_ee_100 <- ((score_ZK_controlGender$mbi_ee - 9)/(54-9))*100
score_ZK_controlGender$mbi_rpe_100 <- ((score_ZK_controlGender$mbi_rpe - 8)/(36-8))*100
score_ZK_controlGender$mbi_de_100 <- ((score_ZK_controlGender$mbi_de - 5)/(26-5))*100
score_ZK_controlGender$nfc_100 <- ((score_ZK_controlGender$nfc + 36)/(45+36))*100
score_ZK_controlGender$scs_100 <- ((score_ZK_controlGender$scs - 21)/(65-21))*100
score_ZK_controlGender$erq_reap_100 <- ((score_ZK_controlGender$erq_reap - 6)/(42-6))*100
score_ZK_controlGender$erq_supp_100 <- ((score_ZK_controlGender$erq_supp - 4)/(28-4))*100
score_ZK_controlGender$sci_adapt_100 <- ((score_ZK_controlGender$sci_adapt - 23)/(61-23))*100
score_ZK_controlGender$sci_maladapt_100 <- ((score_ZK_controlGender$sci_maladapt - 7)/(19-7))*100

score_ZK_controlGender$mbi_100
score_ZK_controlGender$mbi_ee_100
score_ZK_controlGender$mbi_rpe_100
score_ZK_controlGender$mbi_de_100
score_ZK_controlGender$nfc_100
score_ZK_controlGender$scs_100
score_ZK_controlGender$erq_reap_100
score_ZK_controlGender$erq_supp_100
score_ZK_controlGender$sci_adapt_100
score_ZK_controlGender$sci_maladapt_100



##### 2. dummy-coding for control variables "sex"
score_ZK_woOutliers$sex <- replace(score_ZK_woOutliers$sex, score_ZK_woOutliers$sex == 2, 0)
score_ZK_woOutliers$sex
score_ZK_controlGender$sex <- replace(score_ZK_controlGender$sex, score_ZK_controlGender$sex == 2, 0)
score_ZK_controlGender$sex


##### 3. Modeling the mediation models
##### Got hints from that page: https://paolotoffanin.wordpress.com/2017/06/04/multiple-mediation-example/comment-page-1/ (10th of July 2023)
require(lavaan)

### 3.1 Without Control Variables
## 3.1.1 mbi; General mediation model with outliers and the general mbi score as outcome variable
mbiMediation <- '
mbi_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100
scs_100 ~ a1*nfc_100
erq_reap_100 ~ a2*nfc_100
erq_supp_100 ~ a3*nfc_100
sci_adapt_100 ~ a4*nfc_100
sci_maladapt_100 ~ a5*nfc_100
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.1.2 mbi_ee; Mediation model with outliers and the mbi subscale "emotional exhaustion" as outcome variable
mbi_eeMediation <- '
mbi_ee_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100
scs_100 ~ a1*nfc_100
erq_reap_100 ~ a2*nfc_100
erq_supp_100 ~ a3*nfc_100
sci_adapt_100 ~ a4*nfc_100
sci_maladapt_100 ~ a5*nfc_100
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.1.3 rpe; Mediation model with mbi subscale "reduced personal accomplishment" as outcome variable
mbi_rpeMediation <- '
mbi_rpe_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100
scs_100 ~ a1*nfc_100
erq_reap_100 ~ a2*nfc_100
erq_supp_100 ~ a3*nfc_100
sci_adapt_100 ~ a4*nfc_100
sci_maladapt_100 ~ a5*nfc_100
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.1.4 de; Mediation model with mbi subscale "depersonalisation" as outcome variable
mbi_deMediation <- '
mbi_de_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100
scs_100 ~ a1*nfc_100
erq_reap_100 ~ a2*nfc_100
erq_supp_100 ~ a3*nfc_100
sci_adapt_100 ~ a4*nfc_100
sci_maladapt_100 ~ a5*nfc_100
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'


### 3.2 With control variables
# NOTE: With regard to the outliers, only the mediation models "control_mbiMediation" and "control_mbi_eeMediation" differ in their added control variables.
# In contrast to the mediation model "control_mbiMediation", the control variable "sex" is not added to the mediation model without outliers ("control_mbiMediation_woOutliers").
# Instead, the control variable "sex" is added in the mediation model "control_mbi_eeMediation_woOutliers".
# (see the correlation analyses of the chi-square test).
## 2.2.1 mbi
## 2.2.1.1 General mediation model with outliers and the general mbi score as outcome variable (sex and age as control variables)
## control variables; age~mbi, age~nfc, scs, supp, adapt
control_mbiMediation <- '
mbi_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*sex + c3*age
scs_100 ~ a1*nfc_100 + a6*sex + a7*age
erq_reap_100 ~ a2*nfc_100 + a8*sex + a9*age
erq_supp_100 ~ a3*nfc_100 + a10*sex + a11*age
sci_adapt_100 ~ a4*nfc_100 + a12*sex + a13*age
sci_maladapt_100 ~ a5*nfc_100 + a14*sex + a15*age
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a8*b2) + (a10*b3) + (a12*b4) + (a14*b5)
total3 := c3 + (a7*b1) + (a9*b2) + (a11*b3) + (a13*b4) + (a15*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.2.1.2 General mediation model without outliers and the general mbi score as outcome variable (age as control variable)
control_mbiMediation_woOutliers <- '
mbi_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*age
scs_100 ~ a1*nfc_100 + a6*age
erq_reap_100 ~ a2*nfc_100 + a7*age
erq_supp_100 ~ a3*nfc_100 + a8*age
sci_adapt_100 ~ a4*nfc_100 + a9*age
sci_maladapt_100 ~ a5*nfc_100 + a10*age
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a7*b2) + (a8*b3) + (a9*b4) + (a10*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.2.2 ee
## 3.2.2.1 Mediation model with outliers and the mbi subscale "emotional exhaustion" as outcome variable (age and marital status as control variables)
control_mbi_eeMediation <- '
mbi_ee_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*age + c3*married
scs_100 ~ a1*nfc_100 + a6*age + a7*married
erq_reap_100 ~ a2*nfc_100 + a8*age + a9*married
erq_supp_100 ~ a3*nfc_100 + a10*age + a11*married
sci_adapt_100 ~ a4*nfc_100 + a12*age + a13*married
sci_maladapt_100 ~ a5*nfc_100 + a14*age + a15*married
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a8*b2) + (a10*b3) + (a12*b4) + (a14*b5)
total3 := c3 + (a7*b1) + (a9*b2) + (a11*b3) + (a13*b4) + (a15*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.2.2.2 Mediation model without outliers and the mbi subscale "emotional exhaustion" as outcome variable (sex, age, marital status and profession as control variables)
control_mbi_eeMediation_woOutliers <- '
mbi_ee_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*sex + c3*age + c4*married + c5*ManagementProfessions + c6*VolunteersPersonsinEducation
scs_100 ~ a1*nfc_100 + a6*sex + a7*age + a8*married + a9*ManagementProfessions + a10*VolunteersPersonsinEducation
erq_reap_100 ~ a2*nfc_100 + a11*sex + a12*age + a13*married + a14*ManagementProfessions + a15*VolunteersPersonsinEducation
erq_supp_100 ~ a3*nfc_100 + a16*sex + a17*age + a18*married + a19*ManagementProfessions + a20*VolunteersPersonsinEducation
sci_adapt_100 ~ a4*nfc_100 + a21*sex + a22*age + a23*married + a24*ManagementProfessions + a25*VolunteersPersonsinEducation
sci_maladapt_100 ~ a5*nfc_100 + a26*sex + a27*age + a28*married + a29*ManagementProfessions + a30*VolunteersPersonsinEducation
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a11*b2) + (a16*b3) + (a21*b4) + (a26*b5)
total3 := c3 + (a7*b1) + (a12*b2) + (a17*b3) + (a22*b4) + (a27*b5)
total4 := c4 + (a8*b1) + (a13*b2) + (a18*b3) + (a23*b4) + (a28*b5)
total5 := c5 + (a9*b1) + (a14*b2) + (a19*b3) + (a24*b4) + (a29*b5)
total6 := c6 + (a10*b1) + (a15*b2) + (a20*b3) + (a25*b4) + (a30*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.2.3 rpe; Mediation model with mbi subscale "reduced personal accomplishment" as outcome variable (age as control variable)
## NOTE: For both, the model with and the model without outliers
control_mbi_rpeMediation <- '
mbi_rpe_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*age
scs_100 ~ a1*nfc_100 + a6*age
erq_reap_100 ~ a2*nfc_100 + a7*age
erq_supp_100 ~ a3*nfc_100 + a8*age
sci_adapt_100 ~ a4*nfc_100 + a9*age
sci_maladapt_100 ~ a5*nfc_100 + a10*age
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a7*b2) + (a8*b3) + (a9*b4) + (a10*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'

## 3.2.4 de; Mediation model with mbi subscale "depersonalisation" as outcome variable (age and sex as control variables)
## NOTE: For both, the model with and the model without outliers
control_mbi_deMediation <- '
mbi_de_100 ~ b1*scs_100 + b2*erq_reap_100 + b3*erq_supp_100 + b4*sci_adapt_100 + b5*sci_maladapt_100 + c1*nfc_100 + c2*sex + c3*age
scs_100 ~ a1*nfc_100 + a6*sex + a7*age
erq_reap_100 ~ a2*nfc_100 + a8*sex + a9*age
erq_supp_100 ~ a3*nfc_100 + a10*sex + a11*age
sci_adapt_100 ~ a4*nfc_100 + a12*sex + a13*age
sci_maladapt_100 ~ a5*nfc_100 + a14*sex + a15*age
#indirect effects
indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
indirect4 := a4*b4
indirect5 := a5*b5
#contrasts
contrast1 := indirect1 - indirect2
contrast2 := indirect1 - indirect3
contrast3 := indirect1 - indirect4
contrast4 := indirect1 - indirect5
contrast5 := indirect2 - indirect3
contrast6 := indirect2 - indirect4
contrast7 := indirect2 - indirect5
contrast8 := indirect3 - indirect4
contrast9 := indirect3 - indirect5
contrast10 := indirect4 - indirect5
#total effect
total1 := c1 + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5)
total2 := c2 + (a6*b1) + (a8*b2) + (a10*b3) + (a12*b4) + (a14*b5)
total3 := c3 + (a7*b1) + (a9*b2) + (a11*b3) + (a13*b4) + (a15*b5)
scs_100 ~~ erq_reap_100
scs_100 ~~ erq_supp_100
scs_100 ~~ sci_adapt_100
scs_100 ~~ sci_maladapt_100
erq_reap_100 ~~ erq_supp_100
erq_reap_100 ~~ sci_adapt_100
erq_reap_100 ~~ sci_maladapt_100
erq_supp_100 ~~ sci_adapt_100
erq_supp_100 ~~ sci_maladapt_100
sci_adapt_100 ~~ sci_maladapt_100
'



##### 4. Set seed, so R starts calculations at the same point
set.seed(13)



##### 5. Determine the model fit (following Grass et al. [2018] and Zerna et al. [2022] 
### a bootstrapped confidence interval with N = 2000 replicates and the seed 13 will
### be computed)
require("lavaan")

## 5.1 Model fit without control variables
# 5.1.1 mbi
fit_mbiMediation <- sem(mbiMediation,
                        data = score_ZK,
                        se = "bootstrap",
                        bootstrap = 2000,
                        verbose = TRUE)
summary(fit_mbiMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_mbiMediation_woOutliers <- sem(mbiMediation,
                                   data = score_ZK_woOutliers,
                                   se = "bootstrap",
                                   bootstrap = 2000,
                                   verbose = TRUE)
summary(fit_mbiMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.1.2 mbi_rpe
fit_mbi_rpeMediation <- sem(mbi_rpeMediation,
                            data = score_ZK,
                            se = "bootstrap",
                            bootstrap = 2000,
                            verbose = TRUE)
summary(fit_mbi_rpeMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_mbi_rpeMediation_woOutliers <- sem(mbi_rpeMediation,
                                       data = score_ZK_woOutliers,
                                       se = "bootstrap",
                                       bootstrap = 2000, verbose = TRUE)
summary(fit_mbi_rpeMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.1.3 mbi_ee (exploratory analysis)
fit_mbi_eeMediation <- sem(mbi_eeMediation,
                           data = score_ZK,
                           se = "bootstrap",
                           bootstrap = 2000, verbose = TRUE)
summary(fit_mbi_eeMediation,
                fit.measures = TRUE,
                standardize = TRUE,
                rsquare = TRUE,
                estimates = TRUE,
                ci = TRUE)

fit_mbi_eeMediation_woOutliers <- sem(mbi_eeMediation,
                                      data = score_ZK_woOutliers,
                                      se = "bootstrap",
                                      bootstrap = 2000, verbose = TRUE)
summary(fit_mbi_eeMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.1.4 mbi_de (exploratory analysis)
fit_mbi_deMediation <- sem(mbi_deMediation,
                           data = score_ZK,
                           se = "bootstrap",
                           bootstrap = 2000, verbose = TRUE)
summary(fit_mbi_deMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_mbi_deMediation_woOutliers <- sem(mbi_deMediation,
                                      data = score_ZK_woOutliers,
                                      se = "bootstrap",
                                      bootstrap = 2000, verbose = TRUE)
summary(fit_mbi_deMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)



### 5.2 Mediation model with control variables
# 5.2.1 mbi
fit_control_mbiMediation <- sem(control_mbiMediation,
                           data = score_ZK_controlGender,
                           se = "bootstrap",
                           bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbiMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_control_mbiMediation_woOutliers <- sem(control_mbiMediation_woOutliers,
                        data = score_ZK_woOutliers,
                        se = "bootstrap",
                        bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbiMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.2.2 mbi_rpe
fit_control_mbi_rpeMediation <- sem(control_mbi_rpeMediation,
                            data = score_ZK,
                            se = "bootstrap",
                            bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_rpeMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_control_mbi_rpeMediation_woOutliers <- sem(control_mbi_rpeMediation,
                                       data = score_ZK_woOutliers,
                                       se = "bootstrap",
                                       bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_rpeMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.2.3 mbi_ee (exploratory analysis)
fit_control_mbi_eeMediation <- sem(control_mbi_eeMediation,
                        data = score_ZK,
                        se = "bootstrap",
                        bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_eeMediation,
                fit.measures = TRUE,
                standardize = TRUE,
                rsquare = TRUE,
                estimates = TRUE,
                ci = TRUE)

fit_control_mbi_eeMediation_woOutliers <- sem(control_mbi_eeMediation_woOutliers,
                           data = score_ZK_woOutliers,
                           se = "bootstrap",
                           bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_eeMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

# 5.2.4 mbi_de (exploratory analysis)
fit_control_mbi_deMediation <- sem(control_mbi_deMediation,
                        data = score_ZK_controlGender,
                        se = "bootstrap",
                        bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_deMediation,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)

fit_control_mbi_deMediation_woOutliers <- sem(control_mbi_deMediation,
                           data = score_ZK_woOutliers,
                           se = "bootstrap",
                           bootstrap = 2000, verbose = TRUE)
summary(fit_control_mbi_deMediation_woOutliers,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE,
        estimates = TRUE,
        ci = TRUE)