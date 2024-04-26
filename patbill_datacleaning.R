############################################
## Data cleaning for pat bill
## Author: Zoe Haskell-Craig
## Date Created: Apr 26th 2024
## Last Modified: Apr 26th 2024
############################################

#packages ------------------
library(tidyverse)
library(readr)



#load, filter, merge and save data -----------------

setwd("/scratch/Premier/Raw_Data/_patbill")

files <- list.files() #lists files in folder
init <- 1 #counter for what file we're on


for (f in dta_files){ #loop through the list of files
  print("loading file: ", f)
  print(Sys.time())
  temp <- read.table(f, sep = "|", header = TRUE) #load file
  print(Sys.time())
  
  print("filtering for pat billing code")
  
  temp2 <- temp %>% filter(STD_CHG_CODE == "")
  
  if (init == 1) { #first file
    data <- temp2
    init = 0
  } else {
    data <- rbind(data, temp2)
  }
  
  #rename to whatever billing code we've looked for
  phys_restraint <- data
  save(phys_restraint, file = "nyu_allyears_diagnosis.RData")
  
}





