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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"
tables = "tables/"
graphs = "graphs/"
results = "results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(broom)
library(broomExtra)

options(scipen = 999) # disable scientific notation

# load data -----

df_original <- readRDS(paste0(data_files,"df_sample_clean_contyp.rds"))

# df_original <- df_original %>%
#         filter(country == "UK")

# clean data -----

df_event_t_p <- df_original %>%
        rename(event = event_t_p)

df_event_p_t <- df_original %>%
        rename(event = event_p_t)

df_event_contyp <- df_original %>%
        mutate(event = temp)

# Prepare for models ----

df_yhat <- data.frame()
event <- c("contyp","t_p", "p_t")
country <- unique(df_event_contyp$country)

# Models ---- 

# Annual countries
for(c in country) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model - baseline is 1 period before event.  in annual data this is event_time_pos==2
                df_country <- data.frame(df_country)
                model <- feis(ln_hourly_wage ~ event + age + unemployment_rate + as.factor(year) | 1,
                              data = df_country,
                              robust = TRUE,
                              id = "pid")
                
                assign(paste0("model_event_",e,"_country_",c),model)
                save(model, file=paste0(results,"model_event_",e,"_country_",c,".Rdata"))
                
                df_output <- tidy_parameters(model)
                df_output$event <- e
                df_output$country <- c
                df_yhat <- rbind(df_yhat,df_output)
        }
        beep()
}

beep()
beep()

# Save output ----

saveRDS(df_yhat, file = paste0(data_files, "df_yhat_contyp.rds"))


screenreg(list(model_event_contyp_UK,
               model_event_p_t_UK,
               model_event_t_p_UK), digits = 3,
          omit = c("year"))

