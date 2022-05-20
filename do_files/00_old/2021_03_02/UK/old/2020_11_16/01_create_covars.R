# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

raw_data = "BHPS/raw_data/data/R/"
data_files = "projects/booth_etal_2002/data_files/update/"

# LIBRARY
library(dplyr)
library(data.table)
library(beepr)

options(scipen = 999) # disable scientific notation

# Waves --------------------------------------------------------------

waves_table <- read.csv(file = paste0("BHPS/raw_data/mrdoc/waves.csv"))
waves_bhps <- filter(waves_table, survey=="bhps")
waves_bhps_indresp <- factor(waves_bhps$id)

waves_ukhls <- filter(waves_table, survey=="ukhls")
waves_ukhls_indresp <- factor(waves_ukhls$id)

# Variables --------------------------------------------------------------

# BHPS
vars_bhps_indresp <- c(
        "pidp", # cross-wave person identifier
        "jbisco88_cc", # International SOC (ISCO-88 Com)
        "hiqual_dv", # Highest qualification, UKHLS & BHPS samples
        "jbft_dv", # Full or part-time employee
        "jbstat", # Employment status
        "doby", # year of birth
        "sex", # gender
        "jbhrs", # no. of hours normally worked per week
        "fimnlabgrs_dv" # total monthly labour income gross
)

# UKHLS
vars_ukhls_indresp <- c(
        "pidp", # cross-wave person identifier
        "jbisco88_cc", # International SOC (ISCO-88 Com)
        "hiqual_dv", # Highest qualification, UKHLS & BHPS samples
        "jbft_dv", # Full or part-time employee
        "jbstat", # Employment status
        "birthy", # year of birth
        "sex", # gender
        "jbhrs", # no. of hours normally worked per week
        "fimnlabgrs_dv" # total monthly labour income gross
)

# Load covars from BHPS files (indres) --------------------------------------------------------------

for (i in seq_along(waves_bhps_indresp)) {
        print(waves_bhps_indresp[i])
        x <- readRDS(file = paste0(raw_data,waves_bhps_indresp[i],"_indresp.rds"))
        colnames(x) <- gsub(paste0("^",waves_bhps_indresp[i],"_"), "", colnames(x))
        x <- select(x, all_of(vars_bhps_indresp))
        wave_yr <- waves_table[waves_table$id == paste0(waves_bhps_indresp[i]), "year"]
        x$wave_no <- waves_table[waves_table$id == paste0(waves_bhps_indresp[i]), "wave"]
        x$wave_yr <- wave_yr
        print(wave_yr)
        assign(paste0("df_indresp_wave_",waves_bhps_indresp[i]), x)
        rm(x)
}

# append
df_bhps_indresp = data.frame()
for (i in seq_along(waves_bhps_indresp)) {
        y <- get(paste0("df_indresp_wave_", waves_bhps_indresp[i]))
        df_bhps_indresp <- bind_rows(df_bhps_indresp,y)
        rm(y)
}

rm(list=ls(pattern="^df_indresp_wave")) # remove

# Load covars from UKHLS files (indres) --------------------------------------------------------------

for (i in seq_along(waves_ukhls_indresp)) {
        print(waves_ukhls_indresp[i])
        x <- readRDS(file = paste0(raw_data,waves_ukhls_indresp[i],"_indresp.rds"))
        colnames(x) <- gsub(paste0("^",waves_ukhls_indresp[i],"_"), "", colnames(x))
        x <- select(x, all_of(vars_ukhls_indresp))
        wave_yr <- waves_table[waves_table$id == paste0(waves_ukhls_indresp[i]), "year"]
        x$wave_no <- waves_table[waves_table$id == paste0(waves_ukhls_indresp[i]), "wave"]
        x$wave_yr <- wave_yr
        x <- rename(x,doby=birthy)
        print(wave_yr)
        assign(paste0("df_indresp_wave_",waves_ukhls_indresp[i]), x)
        rm(x)
}

# append
df_ukhls_indresp = data.frame()
for (i in seq_along(waves_ukhls_indresp)) {
        y <- get(paste0("df_indresp_wave_", waves_ukhls_indresp[i]))
        df_ukhls_indresp <- rbind(df_ukhls_indresp,y)
        rm(y)
}


rm(list=ls(pattern="^df_indresp_wave")) # remove

# Cleaning Global Environment --------------------------------------------------------------

rm(list=ls(pattern="^wave")) # remove
rm(list=ls(pattern="^vars")) # remove
rm(i)

# Append BHPS to UKHLS --------------------------------------------------------------

df_covars_indresp <- bind_rows(df_ukhls_indresp, df_bhps_indresp)
df_covars_indresp <- arrange(df_covars_indresp, pidp, wave_yr)

rm(df_ukhls_indresp, df_bhps_indresp)

# Save data sets --------------------------------------------------------------

saveRDS(df_covars_indresp, file = paste0(data_files, "covars.rds"))

