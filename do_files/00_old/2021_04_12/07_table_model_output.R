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
tables = "projects/mobility/tables/"

# LIBRARY
library(tidyverse)
library(texreg)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds"))

# Switzerland -----

country <- c("CH")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_full_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

texreg(list(CH_2000,CH_2001,CH_2002,CH_2003,CH_2004,CH_2005,CH_2006,
            CH_2007,CH_2008,CH_2009,CH_2010,CH_2011,CH_2012), 
       file = paste0(tables,"CH_full.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$",
                             "Younger age (25-34) $\\times$ Temp contract",
                             "Younger age (25-34) $\\times$ Years after",
                             "Younger age (25-34) $\\times$ Years after$^{2}$",
                             "Older age (45-54) $\\times$ Temp contract",
                             "Older age (45-54) $\\times$ Years after",
                             "Older age (45-54) $\\times$ Years after$^{2}$",
                             "Higher education ($>$ Secondary) $\\times$ Temp contract",
                             "Higher education ($>$ Secondary) $\\times$ Years after",
                             "Higher education ($>$ Secondary) $\\times$ Years after$^{2}$",
                             "Male $\\times$ Temp contract",
                             "Male $\\times$ Years after",
                             "Male $\\times$ Years after$^{2}$"
       ),
       custom.model.names = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                              "2007", "2008", "2009", "2010", "2011", "2012"),
       # stars = c(0.05, 0.01, 0.001),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

country <- c("CH")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_base_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

screenreg(list(CH_2000)) 

texreg(list(CH_2000,CH_2001,CH_2002,CH_2003,CH_2004,CH_2005,CH_2006,
            CH_2007,CH_2008,CH_2009,CH_2010,CH_2011,CH_2012), 
       file = paste0(tables,"CH_base.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$"
       ),
       custom.model.names = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                              "2007", "2008", "2009", "2010", "2011", "2012"),
       # stars = c(0.05, 0.01, 0.001),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

# Germany -----

country <- c("DE")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_full_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

texreg(list(DE_2000,DE_2001,DE_2002,DE_2003,DE_2004,DE_2005,DE_2006,
            DE_2007,DE_2008,DE_2009,DE_2010,DE_2011,DE_2012), 
       file = paste0(tables,"DE_full.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$",
                             "Younger age (25-34) $\\times$ Temp contract",
                             "Younger age (25-34) $\\times$ Years after",
                             "Younger age (25-34) $\\times$ Years after$^{2}$",
                             "Older age (45-54) $\\times$ Temp contract",
                             "Older age (45-54) $\\times$ Years after",
                             "Older age (45-54) $\\times$ Years after$^{2}$",
                             "Lower education ($<$ Secondary) $\\times$ Temp contract",
                             "Lower education ($<$ Secondary) $\\times$ Years after",
                             "Lower education ($<$ Secondary) $\\times$ Years after$^{2}$",
                             "Higher education ($>$ Secondary) $\\times$ Temp contract",
                             "Higher education ($>$ Secondary) $\\times$ Years after",
                             "Higher education ($>$ Secondary) $\\times$ Years after$^{2}$",
                             "Male $\\times$ Temp contract",
                             "Male $\\times$ Years after",
                             "Male $\\times$ Years after$^{2}$"
       ),
       custom.model.names = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                              "2007", "2008", "2009", "2010", "2011", "2012"),
       # stars = c(0.05, 0.01, 0.001),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

country <- c("DE")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_base_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

screenreg(list(DE_2000)) 
       
texreg(list(DE_2000,DE_2001,DE_2002,DE_2003,DE_2004,DE_2005,DE_2006,
            DE_2007,DE_2008,DE_2009,DE_2010,DE_2011,DE_2012), 
       file = paste0(tables,"DE_base.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$"
       ),
       custom.model.names = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                              "2007", "2008", "2009", "2010", "2011", "2012"),
       # stars = c(0.05, 0.01, 0.001),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

# Italy -----

country <- c("IT")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_full_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

texreg(list(IT_2000,IT_2002,IT_2004,IT_2006,IT_2008,IT_2010), 
       file = paste0(tables,"Italy_full.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$",
                             "Younger age (25-34) $\\times$ Temp contract",
                             "Younger age (25-34) $\\times$ Years after",
                             "Younger age (25-34) $\\times$ Years after$^{2}$",
                             "Older age (45-54) $\\times$ Temp contract",
                             "Older age (45-54) $\\times$ Years after",
                             "Older age (45-54) $\\times$ Years after$^{2}$",
                             "Lower education ($<$ Secondary) $\\times$ Temp contract",
                             "Lower education ($<$ Secondary) $\\times$ Years after",
                             "Lower education ($<$ Secondary) $\\times$ Years after$^{2}$",
                             "Higher education ($>$ Secondary) $\\times$ Temp contract",
                             "Higher education ($>$ Secondary) $\\times$ Years after",
                             "Higher education ($>$ Secondary) $\\times$ Years after$^{2}$",
                             "Male $\\times$ Temp contract",
                             "Male $\\times$ Years after",
                             "Male $\\times$ Years after$^{2}$"
                             ),
       custom.model.names = c("2000", "2002", "2004", "2006", "2008", "2010"),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)


# United Kingdom -----

country <- c("UK")
country_vector <- list()
for(c in country) {
        print(c)
        df_country <- df_sample_0
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                load(paste0(results,"ols_model_",c,"_",y,".rda"))
                assign(paste(c,y,sep = "_"),ols_model)
                country_vector <- append(country_vector,list(paste(c,y,sep = "_")))
        }
}

texreg(list(UK_2000,UK_2001,UK_2002,UK_2003,UK_2004,UK_2005,UK_2006,
            UK_2007,UK_2008,UK_2009,UK_2010,UK_2011,UK_2012), 
       file = paste0(tables,"UK.tex"), 
       table = FALSE, center = FALSE,
       omit.coef = c("(period)|(Inter)"),
       custom.coef.names = c("Unemployment",
                             "Temporary contract",
                             "Years after temp contract",
                             "Years after temp contract$^{2}$",
                             "Younger age (25-34) $\\times$ Temp contract",
                             "Younger age (25-34) $\\times$ Years after",
                             "Younger age (25-34) $\\times$ Years after$^{2}$",
                             "Older age (45-54) $\\times$ Temp contract",
                             "Older age (45-54) $\\times$ Years after",
                             "Older age (45-54) $\\times$ Years after$^{2}$",
                             "Lower education ($<$ Secondary) $\\times$ Temp contract",
                             "Lower education ($<$ Secondary) $\\times$ Years after",
                             "Lower education ($<$ Secondary) $\\times$ Years after$^{2}$",
                             "Higher education ($>$ Secondary) $\\times$ Temp contract",
                             "Higher education ($>$ Secondary) $\\times$ Years after",
                             "Higher education ($>$ Secondary) $\\times$ Years after$^{2}$",
                             "Male $\\times$ Temp contract",
                             "Male $\\times$ Years after",
                             "Male $\\times$ Years after$^{2}$"
       ),
       custom.model.names = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                              "2007", "2008", "2009", "2010", "2011", "2012"),
       custom.note = c("%stars. Period effects not shown"),
       dcolumn = TRUE,
       include.adjrs = FALSE,
       booktabs = TRUE, 
       use.packages = FALSE, 
       single.row = FALSE)

