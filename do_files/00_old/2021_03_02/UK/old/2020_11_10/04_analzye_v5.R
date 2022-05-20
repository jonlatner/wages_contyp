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
library(broom)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 

# clean data -----

df_uk <- df_uk %>%
        mutate(wages = ifelse(wages<1, yes = 1, no = wages),
               ln_wages = log(wages)) %>%
        mutate(ftc = ifelse(is.na(ftc) & unmp == 1, yes = 0, no = ftc),
               tmp = ifelse(is.na(tmp) & unmp == 1, yes = 0, no = tmp),
               contyp = ifelse(is.na(contyp) & unmp == 1, yes = 0, no = contyp),
               prestige = ifelse(is.na(prestige) & unmp == 1, yes = 0, no = prestige))

df_m <- df_uk %>%
        filter(male == 1) 

df_f <- df_uk %>%
        filter(male == 0)

df_unique <- df_uk %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

# independent variables -----

iv <-   "unmp + ftc + as.factor(post_ftc) + tmp + age + age_2 + factor(year)"
iv_2 <- "unmp + ftc + as.factor(post_ftc_first) + tmp + age + age_2 + factor(year)"

# plm model (male) -----
df_male <- data.frame()

plm_model <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                       data = df_m,
                       index = c("pid","year"))

df_results <- tidy(plm_model)
df_results$gender = "Male"
df_results$post = "any"
df_results$age = "All"
df_results$edu = "All"
df_results <- df_results %>%
        filter(row_number()>=2 & row_number()<=7) %>%
        mutate(term = row_number()-1)
df_male <- rbind(df_male,df_results)

plm_model <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                 data = subset(df_m,age_cat == 1),
                 index = c("pid","year"))

df_results <- tidy(plm_model)
df_results$gender = "Male"
df_results$post = "any"
df_results$age = "25-34"
df_results$edu = "All"
df_results <- df_results %>%
        filter(row_number()>=2 & row_number()<=7) %>%
        mutate(term = row_number()-1)
df_male <- rbind(df_male,df_results)

plm_model <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                 data = subset(df_m,age_cat == 2),
                 index = c("pid","year"))

df_results <- tidy(plm_model)
df_results$gender = "Male"
df_results$post = "any"
df_results$age = "25-34"
df_results$edu = "All"
df_results <- df_results %>%
        filter(row_number()>=2 & row_number()<=7) %>%
        mutate(term = row_number()-1)

plm_model_m_first <- plm(as.formula(paste0("ln_wages ~ ",iv_2)),
                         data = df_m,
                         index = c("pid","year"))
plm_model_f_first <- plm(as.formula(paste0("ln_wages ~ ",iv_2)),
                         data = df_f,
                         index = c("pid","year"))
stargazer(plm_model_m_any, plm_model_m_first, plm_model_f_any, plm_model_f_first, type = "text", omit = ("year"))


plm_model_m_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_m,
                        index = c("pid","year"))
plm_model_f_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_f,
                        index = c("pid","year"))
plm_model_m_status_first <- plm(as.formula(paste0("prestige ~ ",iv_2)),
                          data = df_m,
                          index = c("pid","year"))
plm_model_f_status_first <- plm(as.formula(paste0("prestige ~ ",iv_2)),
                          data = df_f,
                          index = c("pid","year"))
stargazer(plm_model_m_status, plm_model_m_status_first, plm_model_f_status, plm_model_f_status_first, type = "text", omit = ("year"))

