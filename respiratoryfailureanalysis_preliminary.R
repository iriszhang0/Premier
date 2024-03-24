# load and merge data ----------------------

setwd("/scratch/Premier/Raw_Data")


library(haven)
library(dplyr)
library(tidyr)
library(lme4) 

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

#create respiratory variables and covariates ------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(all_diagnoses, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(all_diagnoses, "J96"), 1, 0))


#create death (anytime) variable
print("creating death variable")
merged_data$death <- ifelse(merged_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

#create in-hospital mortality
print("creating in-hospital mortality varibale")
merged_data$inhospital_death <- ifelse(merged_data$disc_status == 20, 1, 0)

#merge race and hispanicity
merged_data <- merged_data %>%
  mutate(race_ethnicity = case_when(
    race == "B" & hispanic_ind == "Y" ~ "Hispanic_Black",
    hispanic_ind == "Y" ~ "Hispanic", 
    race == "B" & hispanic_ind != "Y" ~ "nonHispanic_Black",
    race == "W" & hispanic_ind != "Y" ~ "nonHispanic_White",
    race == "A" & hispanic_ind != "Y" ~ "Asian",
    race == "U" ~ "Unknown",
    .default = "Other"))
merged_data$race_ethnicity <- factor(merged_data$race_ethnicity,
                  levels = c("nonHispanic_White", "nonHispanic_Black",
                             "Hispanic_Black", "Hispanic", "Asian",  "Unknown",
                             "Other"))

#create outcome multinomial from disc_status
# (hospice = 40, 41, 42, 50, 51 death or not*is it okay if we are mixing expired or not), *separate death from hospice* 
# (interhospital = 2, 43, 66, 70)
# (transitional care = 61, 71 + 72 (outpatient), )
# (long term care = 3, 4 (intermediate care), 62, 63, 64)
# (home, no indication of hospice =  1, 6 (home health org), 8 (home IV provider))
# (other )
# what does "planned hospital readmission" mean? planned hospital readmission = c(81 - 95)
# * hospice, death (including died in hospice --> same as primary analysis), home other
#ARDS_data$outcome

# BMI as covariate
#merged_data$BMI

# obesity diagnosis codes BMI > 30
# Z code of str_detect "Z68.3" OR
# diagnosis of obesity "E66.0", "E66.1" "E66.2" "E66.8" "E66.9"
print("creating obesity variable")
merged_data <- merged_data %>%
  mutate(E66 = if_else(stringr::str_detect(all_diagnoses, "E66"), 1, 0),
         E66.3 = if_else(stringr::str_detect(all_diagnoses, "E66.3"), 1, 0),
         obesity = if_else((E66 == 1) & (E66.3 == 0), 1, 0)) #obesity for any E66 diagnosis except E66.3

# insurance type
print("creating insurance variable")
merged_data <- merged_data %>%
  mutate(insurance = case_when(std_payor %in% c(300, 310, 320) ~ "medicare",
                               std_payor %in% c(330, 340, 350) ~ "medicaid",
                               std_payor %in% c(360, 370, 380) ~ "private",
                               .default = "other"))






# filter to just ARDS (J80) patients ----------------
ARDS_data <- merged_data %>%
  filter(ARDS == 1)



# sample size ----------------
#sample sizes
length(unique(merged_data$pat_key)) #total sample size
length(ARDS_data$pat_key) #ARDS patients

length(unique(ARDS_data$prov_id)) #number of hospitals

#gender
sum(ARDS_data$gender == "M") #number of men
table(ARDS_data$gender)
table(ARDS_data$gender)/length(ARDS_data$pat_key) #proportion

#race
table(ARDS_data$race) #distribution by race
table(ARDS_data$race)/length(ARDS_data$pat_key) #proportion by race

#hispanicity
table(ARDS_data$hispanic_ind)
table(ARDS_data$hispanic_ind)/length(ARDS_data$pat_key) #proportion

#race and ethnicity
table(ARDS_data$race_ethnicity)
table(ARDS_data$race_ethnicity)/length(ARDS_data$pat_key)

# age
summary(ARDS_data$age) #age distribution
mean(ARDS_data$age, na.rm = TRUE)
sd(ARDS_data$age, na.rm = TRUE)
sum(is.na(ARDS_data$age))

# length of stay
summary(ARDS_data$los)


# death at discharge (expired, expired at home/medical facility/place unknown)
table(ARDS_data$death)
table(ARDS_data$death)/length(ARDS_data$pat_key) #proportion

#obesity
table(ARDS_data$obesity)
table(ARDS_data$obesity)/length(ARDS_data$pat_key) #proportion

#insurance type
table(ARDS_data$insurance)
table(ARDS_data$insurance)/length(ARDS_data$pat_key) #proportion




# Bivariate association Table 2a --------------------
chisq.test(ARDS_data$death, ARDS_data$race_ethnicity) #death
chisq.test(ARDS_data$gender, ARDS_data$race_ethnicity) #gender
chisq.test(ARDS_data$obesity, ARDS_data$race_ethnicity) #obesity

chisq.test(ARDS_data$insurance, ARDS_data$race_ethnicity) #insurance type




# Bivariate association Table 2b -----------------
chisq.test(ARDS_data$race_ethnicity, ARDS_data$death) #race
chisq.test(ARDS_data$hispanic_ind, ARDS_data$death) #ethnicity
chisq.test(ARDS_data$gender, ARDS_data$death) #gender
chisq.test(ARDS_data$obesity, ARDS_data$death) #obesity

chisq.test(ARDS_data$insurance, ARDS_data$death) #insurance type




t.test(filter(ARDS_data, death == 1)$age, filter(ARDS_data, death == 0)$age) #age


# Odds of in-hospital death -----------------------




library(lme4) 
## null --------------
m_null <- glmer(death ~ 1 + (1 | prov_id),
                data = ARDS_data, family = binomial)
summary(m_null)

# ICC
# The ICC is calculated by dividing the random effect variance, σ2i, by the total variance, i.e. the sum of the random effect variance and the residual variance, σ2ε.


## unadjusted -------------------
print(Sys.time())
m0 <- glmer(death ~ race_ethnicity + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 4 mins to run
summary(m0)

#adjusted
print(Sys.time())
m1 <- glmer(death ~ race_ethnicity + age + gender + insurance + (1 | prov_id), 
            data = ARDS_data, family = binomial)
print(Sys.time()) #approx 8 mins to run
summary(m1)





