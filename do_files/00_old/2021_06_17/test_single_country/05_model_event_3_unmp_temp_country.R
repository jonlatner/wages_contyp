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
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(feisr)
library(texreg)
library(dummies)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        # filter(country == "DE" | country == "AU" | country == "IT")
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_3")) %>%
        mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3))

df_dummy <- dummy(x = df_sample_1$post_event_3,sep = "_")
df_sample_1 <- cbind(df_sample_1,df_dummy)

# prepare for output ----

country <- c("DE")
df_output = data.frame() # output
df_model_yhat <- data.frame()

# model 1  ----

iv_vars = c("pre_event_3",
         "event_3", 
         "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)

# annual data
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
        filter(country == c)

        # create year variables
        df_dummy <- dummy(x = df_country$year,sep = "")
        df_country <- cbind(df_country,df_dummy)

        year <- sort(unique(df_country$year))
        year_vars = c()
        for(y in year) {
                year_vars <- c(year_vars,paste0("year",as.character(y)))
        }
        remove <- c(year_vars[1]) # drop baseline
        year_vars <- year_vars [! year_vars %in% remove]
        year_vars

        # create independent variables
        vars <- c(iv_vars,year_vars)

        iv_all = c()
        for (v in vars) {
                iv_all <- paste(iv_all,v,"+")
        }
        iv_all <- sub("..$", "", iv_all)
        iv_all

        # model
        df_country <- data.frame(df_country)
        model <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_all,"| 1")),
                        data = df_country, 
                        id = "pid")

        # predict
        yhat_neg_1 <- data.frame(predict(object = model,
                                         se.fit = TRUE,
                                         newdata = data.frame(pre_event_3=1,
                                                              event_3=0,
                                                              post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                              year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                              year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                         ))) %>%
                select(fit,se.fit)
        yhat_neg_1$post <- -1
        
        yhat_0 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=1,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_0$post <- 0
        
        yhat_1 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=1,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_1$post <- 1
        
        yhat_2 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=1,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_2$post <- 2
        
        
        yhat_3 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=1,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_3$post <- 3
        
        yhat_4 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=1,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_4$post <- 4
        
        yhat <- rbind(yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
        yhat$country <- c
        yhat$model <- 1
        df_model_yhat <- rbind(df_model_yhat,yhat)
        rm(yhat,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,v,y,year,year_vars,remove)
}

model_1 <- model

# model 2  ----

iv_vars = c("unmp",
        "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)

# annual data
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_dummy <- dummy(x = df_country$year,sep = "")
        df_country <- cbind(df_country,df_dummy)
        
        year <- sort(unique(df_country$year))
        year_vars = c()
        for(y in year) {
                year_vars <- c(year_vars,paste0("year",as.character(y)))
        }
        remove <- c(year_vars[1]) # drop baseline
        year_vars <- year_vars [! year_vars %in% remove]
        year_vars
        
        # create independent variables
        vars <- c(iv_vars,year_vars)
        
        iv_all = c()
        for (v in vars) {
                iv_all <- paste(iv_all,v,"+")
        }
        iv_all <- sub("..$", "", iv_all)
        iv_all

        # model
        df_country <- data.frame(df_country)
        model <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_all,"| 1")),
                        data = df_country, 
                        id = "pid")

        # predict
        yhat_neg_1 <- data.frame(predict(object = model,
                                         se.fit = TRUE,
                                         newdata = data.frame(unmp=1,
                                                              pre_event_3=1,
                                                              event_3=0,
                                                              post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                              year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                              year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                         ))) %>%
                select(fit,se.fit)
        yhat_neg_1$post <- -1
        
        yhat_0 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,
                                                          pre_event_3=0,
                                                          event_3=1,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_0$post <- 0
        
        yhat_1 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=1,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_1$post <- 1
        
        yhat_2 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=1,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_2$post <- 2
        
        
        yhat_3 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=1,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_3$post <- 3
        
        yhat_4 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=1,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_4$post <- 4
        
        yhat <- rbind(yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
        yhat$country <- c
        yhat$model <- 2
        df_model_yhat <- rbind(df_model_yhat,yhat)
        rm(yhat,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,v,y,year,year_vars,remove)
}
        
model_2 <- model

# model 3  ----

iv_vars = c("unmp","temp",
            "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)

# annual data
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_dummy <- dummy(x = df_country$year,sep = "")
        df_country <- cbind(df_country,df_dummy)
        
        year <- sort(unique(df_country$year))
        year_vars = c()
        for(y in year) {
                year_vars <- c(year_vars,paste0("year",as.character(y)))
        }
        remove <- c(year_vars[1]) # drop baseline
        year_vars <- year_vars [! year_vars %in% remove]
        year_vars
        
        # create independent variables
        vars <- c(iv_vars,year_vars)
        
        iv_all = c()
        for (v in vars) {
                iv_all <- paste(iv_all,v,"+")
        }
        iv_all <- sub("..$", "", iv_all)
        iv_all

        # model
        df_country <- data.frame(df_country)
        model <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_all,"| 1")),
                        data = df_country, 
                        id = "pid")

        # predict
        yhat_neg_1 <- data.frame(predict(object = model,
                                         se.fit = TRUE,
                                         newdata = data.frame(unmp=1,temp=0,
                                                              pre_event_3=1,
                                                              event_3=0,
                                                              post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                              year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                              year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                         ))) %>%
                select(fit,se.fit)
        yhat_neg_1$post <- -1
        
        yhat_0 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,temp=1,
                                                          pre_event_3=0,
                                                          event_3=1,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_0$post <- 0
        
        yhat_1 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,temp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=1,post_event_3_2=0,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_1$post <- 1
        
        yhat_2 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,temp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=1,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_2$post <- 2
        
        
        yhat_3 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,temp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=1,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_3$post <- 3
        
        yhat_4 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(unmp=0,temp=0,
                                                          pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=0,post_event_3_3=0,post_event_3_4=1,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_4$post <- 4
        
        yhat <- rbind(yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
        yhat$country <- c
        yhat$model <- 3
        df_model_yhat <- rbind(df_model_yhat,yhat)
        rm(yhat,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,v,y,year,year_vars,remove)
}

model_3 <- model

# Save data sets ----

df_model_yhat$event <- 3

# write.csv(df_output,file = paste0(results,"output_event_3.csv"))
write.csv(df_model_yhat,file = paste0(results,"yhat_event_3_country.csv"))

# table ----

screenreg(
        list(model_1,model_2,model_3),
        omit.coef = c("year"),
        # omit.coef = c("year|post|pre")
)

texreg(
        list(model_1,model_2,model_3),
        custom.model.names = c("Model \\ref{model_1}", "Model  \\ref{model_2}", "Model  \\ref{model_3}"),
        file = paste0(tables,"table_event_3_country.tex"),
        table = FALSE,
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = FALSE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year"))

beep()
