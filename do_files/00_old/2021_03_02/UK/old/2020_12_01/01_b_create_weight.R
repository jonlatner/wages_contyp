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
data_files = "projects/mobility/data_files/UK/"

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
waves_ukhls_indresp_1 <- filter(waves_ukhls, year<=2008)
waves_ukhls_indresp_1 <- factor(waves_ukhls_indresp_1$id)
waves_ukhls_indresp_2 <- filter(waves_ukhls, year>=2009)
waves_ukhls_indresp_2 <- factor(waves_ukhls_indresp_2$id)

# Variables --------------------------------------------------------------

# BHPS
vars_bhps_indresp <- c(
        "pidp", # cross-wave person identifier
        "xrwght" # X-sectional respondent weight
)

# UKHLS
vars_ukhls_indresp_1 <- c(
        "pidp", # cross-wave person identifier
        "indinus_xw" # X-sectional respondent weight 
)

vars_ukhls_indresp_2 <- c(
        "pidp", # cross-wave person identifier
        "indinub_xw" # X-sectional respondent weight 
)

# Load covars from BHPS files (indres) --------------------------------------------------------------

x <- readRDS(file = paste0(raw_data,"ba","_indresp.rds"))

for (i in seq_along(waves_bhps_indresp)) {
        print(waves_bhps_indresp[i])
        x <- readRDS(file = paste0(raw_data,waves_bhps_indresp[i],"_indresp.rds"))
        colnames(x) <- gsub(paste0("^",waves_bhps_indresp[i],"_"), "", colnames(x))
        x <- select(x, all_of(vars_bhps_indresp))
        wave_yr <- waves_table[waves_table$id == paste0(waves_bhps_indresp[i]), "year"]
        x$wave_no <- waves_table[waves_table$id == paste0(waves_bhps_indresp[i]), "wave"]
        x$wave_yr <- wave_yr
        x <- rename(x,weight_xc_ind=xrwght)
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

# indresp if year <= 2008
for (i in seq_along(waves_ukhls_indresp_1)) {
        print(waves_ukhls_indresp_1[i])
        x <- readRDS(file = paste0(raw_data,waves_ukhls_indresp_1[i],"_indresp.rds"))
        colnames(x) <- gsub(paste0("^",waves_ukhls_indresp_1[i],"_"), "", colnames(x))
        x <- select(x, all_of(vars_ukhls_indresp_1))
        x <- rename(x, weight_xc_ind=indinus_xw) # rename weights for harmonization across years
        wave_yr <- waves_table[waves_table$id == paste0(waves_ukhls_indresp_1[i]), "year"]
        x$wave_no <- waves_table[waves_table$id == paste0(waves_ukhls_indresp_1[i]), "wave"]
        x$wave_yr <- wave_yr
        print(wave_yr)
        assign(paste0("df_indresp_wave_",waves_ukhls_indresp_1[i]), x)
        rm(x)
}

# indresp if year >= 2009
for (i in seq_along(waves_ukhls_indresp_2)) {
        print(waves_ukhls_indresp_2[i])
        x <- readRDS(file = paste0(raw_data,waves_ukhls_indresp_2[i],"_indresp.rds"))
        colnames(x) <- gsub(paste0("^",waves_ukhls_indresp_2[i],"_"), "", colnames(x))
        x <- select(x, all_of(vars_ukhls_indresp_2))
        x <- rename(x, weight_xc_ind=indinub_xw) # rename weights for harmonization across years
        wave_yr <- waves_table[waves_table$id == paste0(waves_ukhls_indresp_2[i]), "year"]
        x$wave_no <- waves_table[waves_table$id == paste0(waves_ukhls_indresp_2[i]), "wave"]
        x$wave_yr <- wave_yr
        print(wave_yr)
        assign(paste0("df_indresp_wave_",waves_ukhls_indresp_2[i]), x)
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

saveRDS(df_covars_indresp, file = paste0(data_files, "covars_weight.rds"))

