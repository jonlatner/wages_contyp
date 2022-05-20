# Top commands -----------------------------------------
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
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)

options(scipen = 999) # disable scientific notation

# Do files -----------------------------------------

# prepare
source("projects/mobility/do_files/02_AU_analyze_wages.R", echo = TRUE)

source("projects/mobility/do_files/02_CH_analyze_wages.R", echo = TRUE)
source("projects/mobility/do_files/02_DE_analyze_wages.R", echo = TRUE)
source("projects/mobility/do_files/02_UK_analyze_wages.R", echo = TRUE)
