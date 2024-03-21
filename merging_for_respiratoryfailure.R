# Merging for respiratory distress dataset

setwd("/scratch/Premier/Raw_Data")

library(haven)
library(dplyr)
library(tidyr)



##################################
# Diagnosis files

setwd("/scratch/Premier/Raw_Data/_paticd_diag")
print("diagnosis:")


files <- list.files()
dta_files <- files[grep("\\.csv$", files)] #only csv files have been properly widened by primary/secondary diagnosis

init <- 1

for (f in dta_files){
  if (stringr::str_detect(f, "wide_")){
    print(f)
    temp <- readr::read_csv(f)
    
    
    if (init == 1) { #first file
      data <- temp
      init = 0
    } else {
      data <- rbind(data, temp)
    }
  
  }
}  

save(data, file = "nyu_allyears_diagnosis.RData")





##################################
# CPT files

setwd("/scratch/Premier/Raw_Data/_patcpt")

library(dplyr)
library(tidyr)
library(haven)
print("cpt files..")


files <- list.files()
dta_files <- files[grep("\\.dta$", files)]

init <- 1

for (f in dta_files){
  if (stringr::str_detect(f, "wide_")){
    print(f)
    temp <- read_dta(f)
    
    
    if (init == 1) { #first file
      all_cpt <- temp
      init = 0
    } else {
      all_cpt <- rbind(all_cpt, temp)
    }
    
  }
}  

save(all_cpt, file = "nyu_allyears_cpt.RData")

#################################################
# Procedure files


setwd("/scratch/Premier/Raw_Data/_paticd_proc")

library(dplyr)
library(tidyr)
library(haven)
print("proc files..")


files <- list.files()
dta_files <- files[grep("\\.dta$", files)]

init <- 1

for (f in dta_files){
  if (stringr::str_detect(f, "wide_")){
    print(f)
    temp <- read_dta(f)
    
    
    if (init == 1) { #first file
      all_proc <- temp
      init = 0
    } else {
      all_proc <- rbind(all_proc, temp)
    }
    
  }
}  

save(all_proc, file = "nyu_allyears_proc.RData")


#################################################
# Demo files


setwd("/scratch/Premier/Raw_Data/_patdemo")

library(dplyr)
library(tidyr)
library(haven)
print("demographic files..")


files <- list.files()
dta_files <- files[grep("\\.dta$", files)]

init <- 1

for (f in dta_files){
  if (!stringr::str_detect(f, "nyu_allyears")){
    print(f)
    temp <- read_dta(f)
    
    
    if (init == 1) { #first file
      all_demo <- temp
      init = 0
    } else {
      all_demo <- rbind(all_demo, temp)
    }
    
  }
}  

save(all_demo, file = "nyu_allyears_demo.RData")

#################################################
# PATAPR files


setwd("/scratch/Premier/Raw_Data/_pataprdrg")

library(dplyr)
library(tidyr)
library(haven)
print("demographic files..")


files <- list.files()
dta_files <- files[grep("\\.dta$", files)]

init <- 1

for (f in dta_files){
  if (!stringr::str_detect(f, "nyu_allyears")){
    print(f)
    temp <- read_dta(f)
    
    
    if (init == 1) { #first file
      all_aprdrg <- temp
      init = 0
    } else {
      all_aprdrg <- rbind(all_aprdrg, temp)
    }
    
  }
}  

save(all_aprdrg, file = "nyu_allyears_aprdrg.RData")



##############################################
## Merge all files

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

#create respiratory variables

print("creating ARDS")
merged_data <- merged_data %>%
  mutate(ARDS = if_else(stringr::str_detect(all_diagnoses, "J80"), 1, 0))

print("creating J96")
merged_data <- merged_data %>%
  mutate(J96 = if_else(stringr::str_detect(all_diagnoses, "J96"), 1, 0))


#save dataset
setwd("/scratch/Premier/Merged_Data")

print("saving respiratory dataset")
respiratory_demo_aprdrg_diag <- merged_data
save(respiratory_demo_aprdrg_diag, file = "respiratory_demo_aprdrg_diag.RData")




