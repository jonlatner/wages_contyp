# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package,character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
tables = "projects/mobility/tables/"
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(broom)
library(broomExtra)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"df_sample_first_clean.rds"))

# df_original <- readRDS(paste0(data_files,"df_sample_first_clean_sample.rds"))

# clean data -----

df_event_t_p <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_t_p")) %>%
        rename(event_time_pos = event_t_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_p <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_u_p")) %>%
        rename(event_time_pos = event_u_p_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_u_t <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_u_t")) %>%
        rename(event_time_pos = event_u_t_time_pos) %>%
        filter(!is.na(event_time_pos))

df_event_p_t <- df_original %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,matches("event_p_t")) %>%
        rename(event_time_pos = event_p_t_time_pos) %>%
        filter(!is.na(event_time_pos))

# examine some data ----

# df_country <- df_event_u_p %>%
#         filter(event_u_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,matches("event"))
# with(df_country,table(event_u_p_time,event_time_pos))

# df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)

# with(df_event_t_p,table(country,event_t_p_yes))
# df_country <- df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)
# with(df_country,table(event_t_p_time,event_time_pos))

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("t_p","p_t","u_p","u_t")

# Countries
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
country_all <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")

# Models ---- 

# Biannual countries
for(c in country_bi) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model - baseline is 1 period before event.  in biannual data this is event_time_pos==1
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "1")
                df_country <- data.frame(df_country)
                model <- feis(ln_hourly_wage ~ age + event_time_pos + as.factor(year) | 1,
                                   data = df_country, 
                                   robust = TRUE,
                                   id = "pid")
                
                assign(paste0("model_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_2_first_event_",e,"_country_",c,".Rdata"))
                
                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}

# Annual countries) 
for(c in country_ann) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
                df_country$event_time_pos <- relevel(factor(df_country$event_time_pos), ref = "2")
                df_country <- data.frame(df_country)
                model <- feis(ln_hourly_wage ~ age + event_time_pos + as.factor(year) | 1,
                                   data = df_country, 
                                   robust = TRUE,
                                   id = "pid")
                
                assign(paste0("model_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_2_first_event_",e,"_country_",c,".Rdata"))
                
                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
}


# Save output ----

saveRDS(df_yhat, file = paste0(data_files, "df_yhat_first_base_age.rds"))

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
        #        table = FALSE,
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
        #        omit.coef = c("year|Intercept")))

        texreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
               custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
               custom.coef.names = c("Age", "Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
               file = paste0(tables,"table_model_2_first_output_event_",e,".tex"),
               custom.note = c("%stars. Note: Reference period is 1 period before event.  SE are cluster robust."),
               table = FALSE,include.ci = FALSE,
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}

# Individual countries - annual
for(c in country_ann) {
        
        model_event_t_p_country <- get(paste0("model_event_t_p_country_",c))
        model_event_u_p_country <- get(paste0("model_event_u_p_country_",c))
        model_event_u_t_country <- get(paste0("model_event_u_t_country_",c))
        model_event_p_t_country <- get(paste0("model_event_p_t_country_",c))
        
        # screenreg(list(model_event_t_p_country,model_event_t_p_country,model_event_u_p_country,model_event_u_t_country),
        #        custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
        #        custom.coef.names = c("Age", "Pre event (-2)", "Pre event (-1)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
        #        table = FALSE,include.ci = FALSE,
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
        #        omit.coef = c("year|Intercept"))
        
        texreg(list(model_event_t_p_country,model_event_p_t_country,model_event_u_p_country,model_event_u_t_country),
               custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
               custom.coef.names = c("Age", "Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
               file = paste0(tables,"table_model_2_first_output_country_",c,".tex"),
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
               file = paste0(tables,"table_model_2_first_output_country_",c,".tex"),
               table = FALSE,include.ci = FALSE,
               custom.note = c("%stars. Note: Reference period is 1 period before event.  SE are cluster robust."),
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=TRUE,
               omit.coef = c("year|Intercept"))
}

beep()

