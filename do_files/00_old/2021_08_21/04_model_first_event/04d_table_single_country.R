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
country <- c("DE")

for (e in event) {
        for(c in country) {
                load(paste0(results,"model_1_first_event_",e,"_country_",c,".Rdata"))
                assign(paste0("model_1_event_",e),model)
                load(paste0(results,"model_2_first_event_",e,"_country_",c,".Rdata"))
                assign(paste0("model_2_event_",e),model)
                load(paste0(results,"model_3_first_event_",e,"_country_",c,".Rdata"))
                assign(paste0("model_3_event_",e),model)
        }
}

# Tables ----

# Event by countries
for (e in event) {

                model_1 <- get(paste0("model_1_event_",e))
                model_2 <- get(paste0("model_2_event_",e))
                model_3 <- get(paste0("model_3_event_",e))
                
        # print(screenreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
        #        custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
        #        custom.coef.names = c("Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
        #        table = FALSE,include.ci = FALSE,
        #        custom.note = c("%stars. Note: Reference event is Pre event (-1).  SE are cluster robust."),
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
        #        omit.coef = c("year|Intercept")))

        texreg(list(model_1,model_2,model_3),
               custom.model.names = c("Model \\ref{model_1}", "Model \\ref{model_2}", "Model \\ref{model_3}"),
               custom.coef.names = c("Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4),", "Age", "Unmp rate"),
               file = paste0(tables,"table_DE_event_",e,".tex"),
               table = FALSE,include.ci = FALSE,
               custom.note = c("%stars. Note: Reference period is 1 period before event."),
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}
