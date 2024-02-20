
setwd("/scratch/Premier/Raw_Data/_paticd_diag")

library(dplyr)
library(tidyr)
library(haven)

files <- list.files()
dta_files <- files[grep("\\.dta$", files)]
for (f in dta_files){
  if (stringr::str_detect(f, "nyu_allyears")){
    print("This file is the merged all years file")
  } else {
    #print("loading file:", f)
    print(f)
    temp_file <- haven::read_dta(f)
    print("selecting columns")
    temp_file2 <- select(temp_file, pat_key, icd_code)
    
    print("fixing diag stuff")
    print(Sys.time())
    new_df <- temp_file2 %>%
      group_by(pat_key) %>%
      mutate(all_diagnoses = toString(icd_code)) %>%
      select(-icd_code) %>%
      distinct()
    
    print("writing new file")
    file_name <- stringr::str_c("wide_", f)
    haven::write_dta(data = new_df, path = file_name)
    print(Sys.time())
  }
  
}

# _patcpt
# KEEP: pat_key, cpt_code
#

setwd("/scratch/Premier/Raw_Data/_patcpt")

library(dplyr)
library(tidyr)
library(haven)

files <- list.files()
dta_files <- files[grep("\\.dta$", files)]
for (f in dta_files){
  if (stringr::str_detect(f, "nyu_allyears")){
    print("This file is the merged all years file")
  } else {
    #print("loading file:", f)
    print(f)
    temp_file <- haven::read_dta(f)
    print("selecting columns")
    temp_file2 <- select(temp_file, pat_key, cpt_code)
    
    print("fixing diag stuff")
    print(Sys.time())
    new_df <- temp_file2 %>%
      group_by(pat_key) %>%
      mutate(all_cpt_codes = toString(cpt_code)) %>%
      select(-cpt_code) %>%
      distinct()
    
    print("writing new file")
    file_name <- stringr::str_c("wide_", f)
    haven::write_dta(data = new_df, path = file_name)
    print(Sys.time())
  }
  
}


#
# _paticd_proc 
# KEEP: pat_key, icd_code, proc_day, proc_phys
# [NOTE: the latter 2 codes are linked to each individual icd_code. Will this be 
# a problem given our  data transformation plan?]



