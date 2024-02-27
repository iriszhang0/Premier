# load and merge data ----------------------

setwd("/scratch/Premier/Raw_Data")

library(haven)
library(dplyr)
library(tidyr)

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

#create respiratory variables ------------

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(all_diagnoses, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(all_diagnoses, "J96"), 1, 0))

ARDS_data <- merged_data %>%
  filter(ARDS == 1)

#create death at discharge variable
ARDS_data$death <- ifelse(ARDS_data$disc_status %in% c(20, 40, 41, 42), 1, 0)

# sample size ----------------

length(unique(merged_data$pat_key)) #total sample size
sum(merged_data$ARDS, na.rm = TRUE) #number of ARDS patients

ARDS_data <- merged_data %>%
  filter(ARDS == 1)

length(unique(ARDS_data$prov_id)) #number of hospitals

sum(ARDS_data$gender == "M") #number of men
sum(ARDS_data$gender == "M")/length(ARDS_data$pat_key) #proportion men

table(ARDS_data$race) #distribution by race
table(ARDS_data$race)/length(ARDS_data$pat_key) #proportion by race

summary(ARDS_data$age) #age distribution

# length of stay
summary(ARDS_data$los)


# death at discharge (expired, expired at home/medical facility/place unknown)
sum(ARDS_data$disc_status %in% c(20, 40, 41, 42))



# Odds of in-hospital death -----------------------

#create death at discharge variable
ARDS_data$death <- ifelse(ARDS_data$disc_status %in% c(20, 40, 41, 42), 1, 0)


death_by_race <- table(ARDS_data$race, ARDS_data$death)
death_by_race[,2]/(death_by_race[,1]+death_by_race[,2]) #proportion death by end of stay


library(lme4) #note that lme4 is not installed on SRDE
#unadjusted
m0 <- glmer(death ~ race | prov_id, data = ARDS_data, family = binomial)
summary(m0)

#adjusted
m1 <- glmer(death ~ race + age + gender | prov_id, data = ARDS_data, family = binomial)



#split by covid year
ARDS_pre <- filter(ARDS_data, )
ARDS_post <- 

m2_pre <- glmer(death ~ race + age + gender | prov_id, data = ARDS_pre, family = binomial)
m2_post <- glmer(death ~ race + age + gender | prov_id, data = ARDS_post, family = binomial)

library(nlme)
#unadjusted
m0 <- nlme(fixed = death ~ race,
           random = ~1 | prov_id,
           data = ARDS_data, family = binomial) #FIX:: no binomial family option for nlme

#adjusted
m1 <- nmle::nlme(fixed = death ~ race + age + gender,
                 random = ~1 | prov_id,
                 data = ARDS_data, family = binomial)
