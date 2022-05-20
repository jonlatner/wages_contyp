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
graphs = "projects/mobility/graphs/"
tables = "projects/mobility/tables/"

# LIBRARY
library(tidyverse)
library(texreg)

options(scipen = 999) # disable scientific notation

# load data -----

event <- c("t_p","p_t","u_p","u_t")
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
country_all <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")

for (e in event) {
        for(c in country_all) {
                load(paste0(results,"model_2_multiple_event_",e,"_country_",c,".Rdata"))
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

# Tables ----

# Event by countries
for (e in event) {
        for(c in country_all) {
                
                model_event_country <- get(paste0("model_event_",e,"_country_",c))
                assign(paste0("model_",c),model_event_country)
                
        }
        
        # print(screenreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
        #        custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
        #        custom.coef.names = c("Age","Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
        #        table = FALSE,include.ci = FALSE,
        #        custom.note = c("%stars. Note: Reference event is Pre event (-1).  SE are cluster robust."),
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
        #        omit.coef = c("year|Intercept")))

        texreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
               custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
               custom.coef.names = c("Age","Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
               file = paste0(tables,"table_model_2_multiple_output_event_",e,".tex"),
               table = FALSE,include.ci = FALSE,
               custom.note = c("%stars. Note: Reference period is 1 period before event.  SE are cluster robust."),
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}

# Individual countries - annual
for(c in country_all) {
        
        model_event_t_p_country <- get(paste0("model_event_t_p_country_",c))
        model_event_u_p_country <- get(paste0("model_event_u_p_country_",c))
        model_event_u_t_country <- get(paste0("model_event_u_t_country_",c))
        model_event_p_t_country <- get(paste0("model_event_p_t_country_",c))
        
        # print(screenreg(list(model_event_t_p_country,model_event_t_p_country,model_event_u_p_country,model_event_u_t_country),
        #        custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
        #        custom.coef.names = c("Age","Pre event (-2)", "Pre event (-1)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
        #        table = FALSE,include.ci = FALSE,
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
        #        omit.coef = c("year|Intercept")))
        
        texreg(list(model_event_t_p_country,model_event_p_t_country,model_event_u_p_country,model_event_u_t_country),
               custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
               custom.coef.names = c("Age","Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
               file = paste0(tables,"table_model_2_multiple_output_country_",c,".tex"),
               table = FALSE,include.ci = FALSE,
               custom.note = c("%stars. Note: Reference period is 1 period before event.  SE are cluster robust."),
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}

# Individual countries - biannual
for(c in country_bi) {
        
        model_event_t_p_country <- get(paste0("model_event_t_p_country_",c))
        model_event_u_p_country <- get(paste0("model_event_u_p_country_",c))
        model_event_u_t_country <- get(paste0("model_event_u_t_country_",c))
        model_event_p_t_country <- get(paste0("model_event_p_t_country_",c))
        
        texreg(list(model_event_t_p_country,model_event_p_t_country,model_event_u_p_country,model_event_u_t_country),
               custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
               custom.coef.names = c("Age", "Event", "Post event (+2)", "Post event (+4)"),
               file = paste0(tables,"table_model_2_multiple_output_country_",c,".tex"),
               table = FALSE,include.ci = FALSE,
               custom.note = c("%stars. Note: Reference period is 1 period before event.  SE are cluster robust."),
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}
