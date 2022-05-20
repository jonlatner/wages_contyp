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

data_files = "projects/mobility/data_files/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)
library(broom)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        select(country,study_period,pid,year,unmp,temp,ln_hourly_wage,period)

# prepare for output ----
df_table_output = data.frame() # output

iv_plm <-   "unmp + temp + factor(period)"

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

country <- c("AU","CH","DE","JP","KO","NE-LISS")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # plm model
                plm_model_0 <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_1 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,1) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_2 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,2) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_3 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,3) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_4 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,4) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_5 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,5) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                # summary table
                output_table <- tidy(plm_model_0)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 0
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_1)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 1
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_2)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 2
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_3)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 3
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_4)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 4
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_5)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 5
                df_table_output <- rbind(df_table_output,output_table)
        }
}


# model data (UK) ----
# with(subset(df_sample_1,country=="UK"),table(year,study_period))
# with(subset(df_sample_1,country=="DE"),table(year,study_period))

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(seq(2003,2008,1))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # plm model
                plm_model_0 <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_1 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,1) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_2 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,2) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_3 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,3) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_4 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,4) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                # summary table
                output_table <- tidy(plm_model_0)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 0
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_1)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 1
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_2)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 2
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_3)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 3
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_4)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 4
                df_table_output <- rbind(df_table_output,output_table)
                
        }
}

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year_1 <- c(2000,2001,2002)
        year_2 <- c(seq(2009,2012,1))
        year <- c(year_1,year_2)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                plm_model_0 <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_1 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,1) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_2 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,2) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_3 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,3) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_4 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,4) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_5 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,5) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                # summary table
                output_table <- tidy(plm_model_0)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 0
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_1)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 1
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_2)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 2
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_3)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 3
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_4)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 4
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_5)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 5
                df_table_output <- rbind(df_table_output,output_table)
        }
}

# model data (IT, NE-LSP) countries with biannual surveys ----

country <- c("IT","NE-LSP")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                plm_model_0 <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                plm_model_2 <- plm(as.formula(paste0("plm::lead(ln_hourly_wage,2) ~ ",iv_plm)),
                                   data = df_period,
                                   index = c("pid","year"))
                
                # summary table
                output_table <- tidy(plm_model_0)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 0
                df_table_output <- rbind(df_table_output,output_table)
                
                output_table <- tidy(plm_model_2)
                output_table$country <- c
                output_table$study_period <- y
                output_table$post <- 4
                df_table_output <- rbind(df_table_output,output_table)
        }
}

# Save data sets ----

write.csv(df_table_output, file = paste0(results, "results_output_base.csv"))
