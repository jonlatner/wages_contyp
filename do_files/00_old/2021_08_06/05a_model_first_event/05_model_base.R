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
library(dummies)
library(robumeta) #group.center
library(beepr)

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

# df_test <- df_event_u_p %>%
#         filter(event_u_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,matches("event"))
# with(df_test,table(event_u_p_time,event_time_pos))

# df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)

# df_test <- df_event_t_p %>%
#         filter(event_t_p_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_t_p,event_t_p_time,event_time_pos)
# with(df_test,table(event_t_p_time,event_time_pos))

with(df_event_t_p,table(country,event_t_p_yes))

# Prepare for models ----
# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

event <- c("t_p","p_t","u_p","u_t")

# Variables 

iv_vars_annual =   c("event_time_pos_1 + event_time_pos_3 + event_time_pos_4 + event_time_pos_5 + event_time_pos_6 + event_time_pos_7")
iv_vars_biannual = c("event_time_pos_3 + event_time_pos_5 + event_time_pos_7")

vars_annual = c("ln_hourly_wage", "event_time_pos_1", "event_time_pos_3", "event_time_pos_4", "event_time_pos_5", "event_time_pos_6", "event_time_pos_7")
vars_biannual = c("ln_hourly_wage", "event_time_pos_3", "event_time_pos_5", "event_time_pos_7")

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
                
                # create year variables
                df_dummy <- dummy(x = df_country$year,sep = "")
                df_country <- cbind(df_country,df_dummy)
                
                df_dummy <- dummy(x = df_country$event_time_pos,sep = "_")
                df_country <- cbind(df_country,df_dummy)
                
                year <- sort(unique(df_country$year))
                year_vars = c()
                for(y in year) {
                        year_vars <- c(year_vars,paste0("year",as.character(y)))
                }
                remove_base_year_vars <- c(year_vars[1]) # drop baseline
                year_vars <- year_vars [! year_vars %in% remove_base_year_vars]
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_biannual,year_vars)
                
                for (v in vars) {
                        df_country$test <- group.center(df_country[[v]], df_country$pid)
                        df_country$test <- as.numeric(df_country$test)
                        df_country[[v]] <- NULL
                        names(df_country)[names(df_country) == "test"] <- paste0(v)
                }
                
                # create independent variables
                vars_all = c()
                for (v in vars) {
                        vars_all <- paste(vars_all,v,"+")
                }
                vars_all <- sub("..$", "", vars_all)
                
                # model
                df_country <- data.frame(df_country)
                model <- lm(as.formula(paste0("ln_hourly_wage ~ ",vars_all)),
                            data = df_country)
                
                assign(paste0("model_event_",e,"_country_",c),model)
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
                
                # create year variables
                df_dummy <- dummy(x = df_country$year,sep = "")
                df_country <- cbind(df_country,df_dummy)
                
                df_dummy <- dummy(x = df_country$event_time_pos,sep = "_")
                df_country <- cbind(df_country,df_dummy)
                
                year <- sort(unique(df_country$year))
                year_vars = c()
                for(y in year) {
                        year_vars <- c(year_vars,paste0("year",as.character(y)))
                }
                remove_base_year_vars <- c(year_vars[1]) # drop baseline
                year_vars <- year_vars [! year_vars %in% remove_base_year_vars]
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_annual,year_vars)
                
                for (v in vars) {
                        df_country$test <- group.center(df_country[[v]], df_country$pid)
                        df_country$test <- as.numeric(df_country$test)
                        df_country[[v]] <- NULL
                        names(df_country)[names(df_country) == "test"] <- paste0(v)
                }
                
                # create independent variables
                vars_all = c()
                for (v in vars) {
                        vars_all <- paste(vars_all,v,"+")
                }
                vars_all <- sub("..$", "", vars_all)
                
                # model
                df_country <- data.frame(df_country)
                model <- lm(as.formula(paste0("ln_hourly_wage ~ ",vars_all)),
                            data = df_country)
                
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

rm(list=ls(pattern="vars"))
rm(c,e,v,y,year,df_country,df_event,df_dummy,model)

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
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        #        omit.coef = c("year|Intercept")))

        texreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
                        custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
                        custom.coef.names = c("Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
                        file = paste0(tables,"table_model_1_first_output_event_",e,".tex"),
                        table = FALSE,
                        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
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
        #        table = FALSE,
        #        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        #        omit.coef = c("year|Intercept"))
        
        texreg(list(model_event_t_p_country,model_event_p_t_country,model_event_u_p_country,model_event_u_t_country),
               custom.model.names = c("Event 1 (T-P)", "Event 4 (P-T)", "Event 2 (U-P)", "Event 3 (U-T)"),
               custom.coef.names = c("Pre event (-2)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)"),
               file = paste0(tables,"table_model_1_first_output_country_",c,".tex"),
               table = FALSE,
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
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
               custom.coef.names = c("Event", "Post event (+2)", "Post event (+4)"),
               file = paste0(tables,"table_model_1_first_output_country_",c,".tex"),
               table = FALSE,
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
               omit.coef = c("year|Intercept"))
}

# Predict models t_p, p_t ----

df_yhat <- data.frame()

# New data for annual countries

newdata_neg_2 = data.frame(age=0,event_time_pos_1=1,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

# baseline
newdata_neg_1 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_0 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                       event_time_pos_3=1, # event
                       event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0, # post event
                       year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                       year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_1 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=1,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_2 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=1,event_time_pos_6=0,event_time_pos_7=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_3 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=1,event_time_pos_7=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_4 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=1, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

# Predict annual countries
for(c in country_ann) {
        for (e in event) {
                model_event <- get(paste0("model_event_",e,"_country_",c))
                print(c)
                print(e)
                
                yhat_neg_2 <- data.frame(predict(object = model_event,
                                                 se.fit = TRUE,
                                                 newdata = newdata_neg_2
                )) %>%
                        select(fit,se.fit)
                yhat_neg_2$post <- -2
                
                yhat_neg_1 <- data.frame(predict(object = model_event,
                                                 se.fit = TRUE,
                                                 newdata = newdata_neg_1
                )) %>%
                        select(fit,se.fit)
                yhat_neg_1$post <- -1
                
                yhat_0 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_0
                )) %>%
                        select(fit,se.fit)
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_1
                )) %>%
                        select(fit,se.fit)
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_2
                )) %>%
                        select(fit,se.fit)
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_3
                )) %>%
                        select(fit,se.fit)
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_4
                )) %>%
                        select(fit,se.fit)
                yhat_4$post <- 4
                
                yhat <- rbind(yhat_neg_2,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
                yhat$event <- e
                yhat$country <- c
                df_yhat <- rbind(df_yhat,yhat)
        }
}

# New data for biannual countries

# baseline
newdata_neg_2 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_0 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                       event_time_pos_3=1, # event
                       event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                       year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                       year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_2 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=1,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_4 = data.frame(age=0,event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=1,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

# Predict biannual countries

for(c in country_bi) {
        for (e in event) {
                model_event <- get(paste0("model_event_",e,"_country_",c))
                print(c)
                print(e)
                
                yhat_neg_2 <- data.frame(predict(object = model_event,
                                                 se.fit = TRUE,
                                                 newdata = newdata_neg_2
                )) %>%
                        select(fit,se.fit)
                yhat_neg_2$post <- -2
                
                yhat_0 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_0
                )) %>%
                        select(fit,se.fit)
                yhat_0$post <- 0
                
                yhat_2 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_2
                )) %>%
                        select(fit,se.fit)
                yhat_2$post <- 2
                
                
                yhat_4 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_4
                )) %>%
                        select(fit,se.fit)
                yhat_4$post <- 4
                
                yhat <- rbind(yhat_neg_2,yhat_0,yhat_2,yhat_4)        
                yhat$event <- e
                yhat$country <- c
                df_yhat <- rbind(df_yhat,yhat)
        }
}

rm(list=ls(pattern="yhat_"))
rm(list=ls(pattern="model"))
rm(list=ls(pattern="newdata"))
rm(list=ls(pattern="country"))
rm(e,c,event,yhat)

# Save output ----

saveRDS(df_yhat, file = paste0(data_files, "df_yhat_first_base.rds"))
beep()

