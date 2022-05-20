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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"
results = "results/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "03c_df_sample_cleaned_prepared_multiple_events_data.rds")) 

df_sample_0 <- df_sample_0 %>%
  filter(country!="NE-LISS")
df_sample_0$country <- recode(df_sample_0$country, "'NE-LSP'='NE'")
df_sample_0$country_name <- recode(df_sample_0$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Country year sample

df_table <- data.frame(with(df_sample_0,table(year,country)))
df_table <- pivot_wider(df_table,names_from = country, values_from = Freq)
df_table <- df_table %>%
        mutate()

t <- xtable(df_table, digits = c(0))
print(t,
      file = paste0(tables,"table_country_year.tex"),
      format.args = list(big.mark = ",", decimal.mark = "."),
      include.rownames = FALSE,
      include.colnames = TRUE,
      floating="FALSE",
      comment = FALSE
)

t