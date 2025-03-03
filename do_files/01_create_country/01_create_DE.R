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

raw_data = "data_files/DE/raw_data/"
data_files = "data_files/DE/"

# LIBRARY
library(dplyr)
library(data.table) # faster merging

options(scipen = 999) # disable scientific notation

# LOAD DATA --------------------------------------------------------------

# harmonized contract type
# df_pl <- readRDS(file = paste0(raw_data,"pl.rds"))
# df_contyp <- select(df_pl, pid, hid, syear, plb0037_h) %>%
#         rename(contyp = plb0037_h)
# saveRDS(df_contyp, file = paste0(raw_data, "pl_contyp.rds"))
df_contyp <- readRDS(file = paste0(raw_data,"pl_contyp.rds"))

# time-invariant control variables
df_ppfad <- readRDS(file = paste0(raw_data,"ppfad.rds"))

# time-varying control variables
df_pgen <- readRDS(file = paste0(raw_data,"pgen.rds"))
df_pequiv <- readRDS(file = paste0(raw_data,"pequiv.rds"))




# variable lists --------------------------------------------------------------
# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight
# pgen varlist
varlist_pgen <- c(
        "pid", # Person ID
        "syear", 
        "pgvebzeit", # Actual weekly working hours
        "pglfs", # labor force status
        "pgisco88", # current occupational classification (ISCO-88 Com)
        "pgsiops88", # Treiman Standard Int Occ Prestige IS88
        "pgcasmin", # CASMIN Classification (education)
        "pgemplst") # Employment status

# pequiv varlist
varlist_pequiv <- c("pid", # Person ID
                    "syear", 
                    "e11101", # Annual Work Hours of Individual 
                    "ijob1", # Wages,Salary from main job
                    "w11103", # Longitudinal Weight - Respondent Individual
                    "w11105") # Cross-sectional Weight - Respondent Individual

# ppfad varlist
varlist_ppfad <- c("pid", # "pid" Person ID
                   "sex", # "sex" gender
                   "gebjahr") # "gebjahr" birth year

# 1 Extract control variables --------------------------------------------------------------

df_ppfad <- select(df_ppfad, !! varlist_ppfad) %>%
        rename(birth_year = gebjahr)

df_pgen <- select(df_pgen, !!  varlist_pgen) %>%
        rename(lfs = pglfs,
               occ = pgisco88,
               hours_weekly = pgvebzeit,
               prestige = pgsiops88,
               edu = pgcasmin,
               empst = pgemplst)

df_pequiv <- select(df_pequiv, !! varlist_pequiv) %>%
        rename(wages = ijob1,
               hours = e11101,
               weight_long = w11103,
               weight_xs = w11105)

df_de <- merge(x = data.table(df_pgen), y = data.table(df_ppfad), by = c("pid"), all.x = TRUE)
df_de <- merge(x = data.table(df_de), y = data.table(df_pequiv), by = c("pid", "syear"), all.x = TRUE)
df_de <- merge(x = data.table(df_de), y = data.table(df_contyp), by = c("pid", "syear"), all.x = TRUE)

# Save data sets --------------------------------------------------------------
df_de <- select(df_de, syear, pid, everything())
saveRDS(df_de, file = paste0(data_files, "covars.rds"))


