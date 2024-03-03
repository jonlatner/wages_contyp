# Top commands ----
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

raw_data = "data_files/AU_HILDA/raw_data/"
data_files = "data_files/AU/"

# PACKAGES
# install.packages("dplyr")
# install.packages("readstata13")
# install.packages("beepr")

# LIBRARY
library(tidyverse)
library(data.table) # faster merging
library(haven)
library(car) # recode

options(scipen = 999) # disable scientific notation

# PREPARE CODES FOR YEAR (LETTERS) ----

varlist <- c("hhrhid", # Household ID
             "jbmhruc", # Hours per week usually worked in main jo 
             "jbmcnt", # E25 Employment contract - current job
             "esdtl", #  labor force status detailed (FT/PT)
             "jbm682", #  ISCO-88 2-digit, Occupation current main job
             "jbmo6s", #  ANU4 occupational status scale, current main job
             "edhigh1", #  Education
             "esempdt", #  Current employment status (detailed)
             "wsfei", #  Wages,Salary from main job 
             "hhwtrps", #  Cross-sectional Weight - Enumerated person sample weight
             "hgsex", #  gender
             "hgage" #  Age last birthday at June 30 2018
             )


letters_seq <- c(letters, sapply(letters, function(x) paste0(x, letters)))

# waves 1 through 18
letters_seq1 <- letters_seq[c(1:18)]
years1 <- c(2001:2018)
rm(letters_seq)

# 1 Extract contract type from annual person data (and gap data) ----

for(i in seq_along(letters_seq1)) {
        print(years1[i])
        print(letters_seq1[i])
        varlist1 <- paste(letters_seq1[i], varlist, sep = "")
        varlist1 <- c("xwaveid",varlist1)
        # read
        df_x <- readRDS(paste0(raw_data,"Rperson_",letters_seq1[i],"180c.rds"))
        # append
        df_x <- select(df_x, all_of(varlist1))
        df_x$year <- years1[i]
        assign(paste0("df_year_",years1[i]), df_x)
}

# 2 Combine in one data set (and delete auxiliary data) ----

# Combine in one data set
varlist2 <- c("xwaveid",varlist,"year")
df_hilda = data.frame()
for(c in 2001:2018) {
        print(c)
        df_x <- get(paste0("df_year_", c))
        df_x <- zap_formats(df_x)
        df_x <- zap_label(df_x)
        df_x <- zap_labels(df_x)
        colnames(df_x) <- varlist2
        df_hilda <- rbind(df_hilda,df_x)
        rm(df_x)
}

df_hilda <- df_hilda %>%
  mutate(xwaveid = as.numeric(xwaveid),
         hhrhid = as.numeric(hhrhid))
  
# Delete auxiliary data
for(c in 2001:2018) {
  rm(list=paste0("df_year_", c))
}

# Save data sets --------------------------------------------------------------
saveRDS(df_hilda, file = paste0(data_files, "covars.rds"))

