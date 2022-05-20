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
graphs = "graphs/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(texreg)

options(scipen = 999) # disable scientific notation

# load data -----

event <- c("contyp","t_p", "p_t")
country_all <- c("AU","CH","DE","JP","KO","UK","NE-LSP","IT")

for (e in event) {
        for(c in country_all) {
                load(paste0(results,"model_event_",e,"_country_",c,".Rdata"))
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

# Tables ----

# Event by countries
country <- "UK"
for(c in country) {
        for (e in event) {
                model <- get(paste0("model_event_",e,"_country_",c))
                assign(paste0("model_",e),model)
                
        }
        
        print(screenreg(list(model_contyp,model_p_t,model_t_p),
               table = FALSE,include.ci = FALSE, digits = 3,
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept")))

}
