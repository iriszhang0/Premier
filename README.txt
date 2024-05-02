READ ME FOR PREMIER ANALYSIS CODE


0. IN GENERAL

* In order to run the code on the SRDE, open R on the SRDE (run 'R'), copy and paste the code from your local RStudio into the terminal R session. 

* Some of the R scripts described here created a new data file on the SRDE, but in general we are working with a pipeline that loads, cleans data and runs an analysis on it each time (instead of saving a cleaned data file to the SRDE) to save space/keep the SRDE from getting cluttered


1. DATA CREATION

*Premier_widening_script.R* contains code to "widen" the raw data files. Raw data files contain rows for each patient interaction (i.e. one row per ICD diagnosis per patient). This script widens the data as follows:

* ICD diagnosis codes becomes one row per patient 'patkey', one column ('diagnoses_all') contains all ICD codes as a character string seprated by spaces, one column ('diagnoses_admit') for the admitting code, one column ('diagnoses_primary') for the primary diagnoses, one column ('diagnoses_secondary') for the secondary diagnoses. Even if more than one diagnosis per category, the codes are a single character string with spaces separating each code.

* PATCPT codes become one row per 'pat_key' with all cpt codes under the column name 'all_cpt_codes'

* PROC CODES become one row per 'pat_key' with all procedure codes under the column name 'all_proc_codes'

* PAT ICD PROC day of procedure: a dataset is also created with one row per 'pat_key' per day of hospitalization. Day of hospitalization/procudure is under the column named 'proc_day'. All the procedure codes for this day are in a string under the column name 'all_proc_codes'


*merging_for_respiratoryfailure.R* is the script to merge the previously widened files (still saved separately by year and quarter) into a single file. All these files are called 'nyu_allyears_[x]'. The following datasets have been merged: diagnosis, cpt, procedure, demo, patapr. *Note* that although there is code to join all these files together and save, this results in a file too large to be saved or opened.


*patbill_datacleaning.R* contains example code to clean the _patbill files. Since these files are so large, the specific billing code for one project needs to be done at a time (instead of widening the whole dataset). The instructions for running this code are in the comments in this .R script

2. DATA CLEANING

*example_datacleaning.R* contains example code to clean each dataset. The workflow process for analysis is to copy and paste the datacleaning code for your project into an R session on the SRDE each time you want to conduct an analysis (should take 5 - 15 mins to run all the code) instead of saving the cleaned version of your dataset as a specific file on the SRDE for space reasons (also it takes too long to open the data when it is saved as one file as opposed to many). 

The *example_datacleaning.R* is intended as a template for doing different data cleaning tasks. Each project should create their own script specifically for the covariates that they need. 

The *example_datacleaning.R* script also contains the code/template to create covariates such as 'race_ethnicity' used for many projects so that we maintain a uniform definition across multiple projects. If any new such variables are created that are likely to be used by multiple projects, that code should be added to this script.


3. ANALYSIS

Each project team should create their own script for their data analysis. All code used in the analysis should be saved in the script. In order to run the code on the SRDE, open R on the SRDE (run 'R'), copy and paste the code from your local RStudio into the terminal R session. Data will need to be loaded and cleaned each session (~7 - 15 mins). 

4. DEBUGGING TIPS

* Are you in the right directory/is your path correct? Run `getwd()` in R or `pwd` on the terminal. Note that from R we have to specifically set the working directory to /scratch/...... to be able to load a dataset saved in /scratch. Note that we cannot load or save a dataset into another user's home directories 

* is there enough space in the SRDE directory? Run `ls -l` on the terminal to see how much space files are taking up and if a file was saved/created from R but is appearing to be empty on the SRDE it may be because you ran out of space. 




