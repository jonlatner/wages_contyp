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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/")
# setwd("C:/Users/ba1ks6/OneDrive/SECCOPA/")

raw_data = "data/JP_JHPS_KHPS/raw_data/"
data_files = "projects/wage_mobility/data_files/JP/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# Variables ----
# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight

vars_ind <- c("id", # Respondent ID  
              "year", 
              "v5", # gender
              "v6", # year of birth
              "v106", # education - school last attended
              "v170", # Labor force status - were you employed last month
              "v218", # Employment status
              "v219", # Job position
              "v224", # Contract worker
              "v263", # hours worked per week (incl. overtime)
              "v264", # hours worked overtime per week
              "v228", # occupation
              "v241" # annual income from employment
)

# Load covars from FORS files (individual) ----

df_jhps <- readRDS(file = paste0(raw_data,"JHPS_KHPS.rds"))
df_jhps <- select(df_jhps, all_of(vars_ind))

# label vars ----

df_jhps <- df_jhps %>%
        rename(pid=id,
               gender = v5,
               birth_year = v6,
               edu = v106,
               lfs = v170,
               empstat = v218,
               position = v219,
               contyp = v224,
               hours_total = v263,
               hours_overtime = v264,
               occ = v228,
               wages = v241
               )

# Save data sets ----

saveRDS(df_jhps, file = paste0(data_files, "covars.rds"))

