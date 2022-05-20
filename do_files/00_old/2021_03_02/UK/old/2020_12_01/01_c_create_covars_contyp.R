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

options(scipen = 999) # disable scientific notation

# Waves ----

waves_table <- read.csv(file = paste0("BHPS/raw_data/mrdoc/waves.csv"))
waves_bhps <- filter(waves_table, survey=="bhps")
waves_bhps_indresp <- factor(waves_bhps$id)

waves_bhps_indresp_1 <- filter(waves_bhps, year<=1997)
waves_bhps_indresp_1 <- factor(waves_bhps_indresp_1$id)

waves_bhps_indresp_2 <- filter(waves_bhps, year>=1998)
waves_bhps_indresp_2 <- factor(waves_bhps_indresp_2$id)

waves_ukhls <- filter(waves_table, survey=="ukhls")
waves_ukhls_indresp <- factor(waves_ukhls$id)

# Variables ----

# BHPS
# "pidp" cross-wave person identifier

vars_bhps_indresp_1 <- c("pidp", "jbterm")
vars_bhps_indresp_2 <- c("pidp", "jbterm1", "jbterm2")

# UKHLS
# "pidp" cross-wave person identifier

vars_ukhls_indresp <- c("pidp", "jbterm1", "jbterm2")

# Load covars from indres (UKHLS) ----

for (i in seq_along(waves_ukhls_indresp)) {
          print(waves_ukhls_indresp[i])
          x <- readRDS(file = paste0(raw_data,waves_ukhls_indresp[i],"_indresp.rds"))
          colnames(x) <- gsub(paste0("^",waves_ukhls_indresp[i],"_"), "", colnames(x))
          x <- select(x, vars_ukhls_indresp)
          wave_yr <- waves_ukhls[waves_ukhls$id == paste0(waves_ukhls_indresp[i]), "year"]
          x$wave_no <- waves_ukhls[waves_ukhls$id == paste0(waves_ukhls_indresp[i]), "wave"]
          x$wave_yr <- wave_yr
          print(wave_yr)
          assign(paste0("indresp_wave_",waves_ukhls_indresp[i]), data.frame(x))
          rm(x)
}

# append
ukhls_indresp = data.frame()
for (i in seq_along(waves_ukhls_indresp)) {
          y <- get(paste0("indresp_wave_", waves_ukhls_indresp[i]))
          ukhls_indresp <- rbind(ukhls_indresp,y)
          rm(y)
}

rm(list=ls(pattern="^indresp_wave")) # remove

ukhls_indresp <- arrange(ukhls_indresp, pidp, wave_yr)

# Load covars from BHPS files (indres) ----

# indresp if year <= 1997
for (i in seq_along(waves_bhps_indresp_1)) {
          print(waves_bhps_indresp_1[i])
          x <- readRDS(file = paste0(raw_data,waves_bhps_indresp_1[i],"_indresp.rds"))
          colnames(x) <- gsub(paste0("^",waves_bhps_indresp_1[i],"_"), "", colnames(x))
          x <- select(x, vars_bhps_indresp_1)
          wave_yr <- waves_bhps[waves_bhps$id == paste0(waves_bhps_indresp_1[i]), "year"]
          x$wave_no <- waves_bhps[waves_bhps$id == paste0(waves_bhps_indresp_1[i]), "wave"]
          x$wave_yr <- wave_yr
          print(wave_yr)
          assign(paste0("indresp_wave_",waves_bhps_indresp_1[i]), data.frame(x))
          rm(x)
}

# indresp if year >= 1998
for (i in seq_along(waves_bhps_indresp_2)) {
          print(waves_bhps_indresp_2[i])
          x <- readRDS(file = paste0(raw_data,waves_bhps_indresp_2[i],"_indresp.rds"))
          colnames(x) <- gsub(paste0("^",waves_bhps_indresp_2[i],"_"), "", colnames(x))
          x <- select(x, vars_bhps_indresp_2)
          wave_yr <- waves_bhps[waves_bhps$id == paste0(waves_bhps_indresp_2[i]), "year"]
          x$wave_no <- waves_bhps[waves_bhps$id == paste0(waves_bhps_indresp_2[i]), "wave"]
          x$wave_yr <- wave_yr
          print(wave_yr)
          assign(paste0("indresp_wave_",waves_bhps_indresp_2[i]), data.frame(x))
          rm(x)
}

# append
bhps_indresp = data.frame()
for (i in seq_along(waves_bhps_indresp)) {
          y <- get(paste0("indresp_wave_", waves_bhps_indresp[i]))
          bhps_indresp <- bind_rows(bhps_indresp,y)
          rm(y)
}

rm(list=ls(pattern="^indresp_wave")) # remove

# Cleaning Global Environment ----

rm(list=ls(pattern="^wave")) # remove
rm(list=ls(pattern="^vars")) # remove
rm(i)

# Save data sets ----

covars_indresp <- bind_rows(ukhls_indresp, bhps_indresp)
covars_indresp <- arrange(covars_indresp, pidp, wave_yr)
saveRDS(covars_indresp, file = paste0(data_files, "covars_contyp.rds"))

