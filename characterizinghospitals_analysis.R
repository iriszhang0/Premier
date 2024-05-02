########################################
## Characterizing hospitals
## Author: Zoe Haskell-Craig
## Date Created: May 2 2024
## Last Modified: May 2 2024
########################################

# packages ----------------------
library(haven)
library(dplyr)
library(tidyr)



# load and merge data ----------------------
setwd("/scratch/Premier/Raw_Data")

print("loading .... demo")
print(Sys.time())
demo <- read_dta("_patdemo/nyu_allyears_patdemo.dta")
print(Sys.time())


print("loading .... diagnosis")
setwd("/scratch/Premier/Raw_Data/_paticd_diag")
print(Sys.time())
load(file = "nyu_allyears_diagnosis.RData")
print(Sys.time())

print("merging")
merged_data <- left_join(demo, data, by = "pat_key")
print(Sys.time())

print("loading .... aprdrg")
setwd("/scratch/Premier/Raw_Data/_pataprdrg")
print(Sys.time())
load(file = "nyu_allyears_aprdrg.RData")
print(Sys.time())

print("merging")
merged_data <- left_join(merged_data, all_aprdrg, by = "pat_key")
print(Sys.time())

print("loading ... region")
setwd("/scratch/Premier/Raw_Data")
print(Sys.time())
region <- read.delim("nyu_providers.txt",sep = "|")
print(Sys.time())

print("merging")
#colnames(region) <- tolower(colnames(region))
merged_data <- left_join(merged_data, region, by = c("prov_id" = "PROV_ID"))
print(Sys.time())

# create covariates ---------------

#creating death or hospice transfer
print("creating death or hospice transfer variable")
merged_data$death_or_hospice <- ifelse(merged_data$disc_status %in% 
                                         c(20, 40, 41, 42, 50, 51), 1, 0)

#create in-hospital mortality
print("creating in-hospital mortality varibale")
merged_data$inhospital_death <- ifelse(merged_data$disc_status == 20, 1, 0)

#merge race and hispanicity
merged_data <- merged_data %>%
  mutate(race_ethnicity = case_when(
    hispanic_ind == "Y" ~ "Hispanic", 
    race == "B" & hispanic_ind != "Y" ~ "nonHispanic_Black",
    race == "W" & hispanic_ind != "Y" ~ "nonHispanic_White",
    race == "A" & hispanic_ind != "Y" ~ "Asian",
    race == "U" ~ "Unknown",
    .default = "Other"))
merged_data$race_ethnicity <- factor(merged_data$race_ethnicity,
                                     levels = c("nonHispanic_White", "nonHispanic_Black",
                                                "Hispanic", "Asian",  "Unknown",
                                                "Other"))

# insurance type
print("creating insurance variable")
merged_data <- merged_data %>%
  mutate(insurance = case_when(std_payor %in% c(300, 310, 320) ~ "medicare",
                               std_payor %in% c(330, 340, 350) ~ "medicaid",
                               std_payor %in% c(360, 370, 380) ~ "private",
                               .default = "other"))

merged_data$insurance <- factor(merged_data$insurance, 
                                levels = c("private", "medicaid", "medicare",
                                           "other"))

# region
print("creating region variable for hosptials")
#merge middle atlantic and new england
merged_data[merged_data$PROV_DIVISION == "MIDDLE ATLANTIC",]$PROV_DIVISION <- "NEW ENGLAND"
#merge mountain and WEST NORTH CENTRAL
merged_data[merged_data$PROV_DIVISION == "MOUNTAIN",]$PROV_DIVISION <- "WEST NORTH CENTRAL"
table(merged_data$PROV_DIVISION)


# ARDS patients
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(diagnoses_all, "J80"), 1, 0))

ARDS_data <- merged_data %>%
  filter(ARDS == 1)


# hospitals with at least one ARDS patient
#create list of hospitals with one ARDs patient
ARDS_hospitals <- unique(ARDS_data$prov_id)
Asian_ARDS_hospitals <- unique(filter(ARDS_data, race_ethnicity == "Asian")$prov_id)

#all patients in ARDS hospitals
pat_ARDS_hosp <- merged_data %>% filter(prov_id %in% ARDS_hospitals)


# all patients in hospitals with at least one Asian ARDS patient
pat_Asian_ARDS_hosp <- merged_data %>% filter(prov_id %in% Asian_ARDS_hospitals)




# descriptive statistics ----------------

## hospitals by region ---------------
# number of hospitals by region
merged_data %>% group_by(PROV_REGION) %>%
  count(prov_id) %>% select(PROV_REGION, prov_id) %>%
  unique() %>% group_by(PROV_REGION) %>% count()


# number of hospitals with at least one patient with J80 diagnosis, by region
ARDS_data %>% group_by(PROV_REGION) %>%
  count(prov_id) %>% select(PROV_REGION, prov_id) %>%
  unique() %>% group_by(PROV_REGION) %>% count()


# number of hospitals with at least one *Asian* patient with J80 diagnosis, by region
ARDS_data %>% filter(race_ethnicity == "Asian") %>% group_by(PROV_REGION) %>%
  count(prov_id) %>% select(PROV_REGION, prov_id) %>%
  unique() %>% group_by(PROV_REGION) %>% count()


# Number of (total) Asian ARDS patients by region
ARDS_data %>% filter(race_ethnicity == "Asian") %>% group_by(PROV_REGION) %>%
  count()


# insurance type by Asian identity --------------------------
table(filter(ARDS_data, race_ethnicity == "Asian")$insurance)

table(ARDS_data$insurance, ARDS_data$race_ethnicity)

# characteristics of hospitals serving Asian patients -------------------
## split by Covid period -----------------
# pre-covid
pre_covid <- c(2017101, 2016412, 2016411, 2017102, 2016308, 2016410,
               2017103, 2017204, 2017205, 2017206, 2017307, 2017308,
               2017309, 2013309, 2017410, 2017411, 2017412, 2018101,
               2018102, 2018103, 2018204, 2018205, 2018206, 2018307,
               2018308, 2018309, 2018410, 2018411, 2018412, 2019101,
               2019102, 2019103, 2019204, 2019205, 2019206, 2019307,
               2019308, 2019309, 2019410, 2019411, 2019412, 2020101,
               2020102) #up to Feb 2020

covid <- c(2020103, 2020204, 2020205, 2020206, 2020307, 2020308, 
           2020309, 2020410, 2020411, 2020412, 2021103, 2021204,
           2021102, 2021101, 2021205, 2021206, 2021307, 2021308,
           2021309)


# pre-Covid
pat_Asian_ARDS_hosp_pre <- pat_Asian_ARDS_hosp %>%
  filter(adm_mon %in% pre_covid)

pat_ARDS_hosp_pre <- pat_ARDS_hosp %>%
  filter(adm_mon %in% pre_covid)


## minority serving --------------------

pat_ARDS_hosp_pre %>% 
  group_by(prov_id) %>%
  mutate(patient_pop = n(),
    prop_white = sum(race_ethnicity == "nonHispanic_White")/patient_pop,
    decile_white = ) %>%
  
  

## medicaid serving -------------------

## urban vs rural ------------------

## teaching hospitals --------------------










