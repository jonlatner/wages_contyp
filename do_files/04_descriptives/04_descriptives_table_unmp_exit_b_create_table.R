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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)

options(scipen = 999) # disable scientific notation

# load data -----

df_table <- read.csv(paste0(data_files, "04_df_descriptives_table_unmp_exit.csv"))
df_table <- df_table %>%
  select(!matches("total"))
df_table

df_table %>% pivot_longer(!country) 
