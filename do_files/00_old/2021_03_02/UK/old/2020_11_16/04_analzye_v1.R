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

data_files = "projects/booth_etal_2002/data_files/update/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 
df_uk <- df_uk %>%
        filter(year >= 2000)

# clean data -----

df_m <- df_uk %>%
        filter(male == 1)

df_f <- df_uk %>%
        filter(male == 0)

df_unique <- df_uk %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

# plm model (salary/wages) -----

iv <- "unmp + ftc*poly(age,2,raw=TRUE) + tmp*poly(age,2,raw=TRUE) + factor(year)"

plm_model_m_ann <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                       data = df_m,
                       index = c("pid","year"))
plm_model_f_ann <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                       data = df_f,
                       index = c("pid","year"))

plm_model_m_hrly <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
                        data = df_m,
                        index = c("pid","year"))
plm_model_f_hrly <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
                        data = df_f,
                        index = c("pid","year"))

stargazer(plm_model_m_ann, plm_model_f_ann, plm_model_m_hrly, plm_model_f_hrly, type = "text", omit = ("year"))

# plm model (prestige) -----

plm_model_m_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_m,
                        index = c("pid","year"))
plm_model_f_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_f,
                        index = c("pid","year"))
stargazer(plm_model_m_status, plm_model_f_status, type = "text", omit = ("year"))

