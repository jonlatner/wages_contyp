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
results = "projects/mobility/results/balanced/"

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
        select(country,pid,year,ln_hourly_wage,unmp,temp,matches("event_3"))

# prepare for output ----

df_output = data.frame() # output
df_model_yhat <- data.frame()

# model 1  ----

# annual data
iv_vars = c("pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)
country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
        filter(country == c)

        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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

        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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

# biannual data
iv_vars = c("pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3"
)
country <- c("IT","NE-LSP")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 3, yes = 3, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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
        
        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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
        yhat_neg_1$post <- -2

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
        yhat_1$post <- 2
        
        yhat_2 <- data.frame(predict(object = model,
                                     se.fit = TRUE,
                                     newdata = data.frame(pre_event_3=0,
                                                          event_3=0,
                                                          post_event_3_1=0,post_event_3_2=1,post_event_3_3=0,post_event_3_4=0,post_event_3_5=0,
                                                          year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                                                          year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
                                     ))) %>%
                select(fit,se.fit)
        yhat_2$post <- 4

        yhat <- rbind(yhat_neg_1,yhat_0,yhat_1,yhat_2)        
        yhat$country <- c
        yhat$model <- 1
        df_model_yhat <- rbind(df_model_yhat,yhat)
        rm(yhat,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,v,y,year,year_vars,remove)
}

# model 2  ----

# annual data
iv_vars = c("unmp",
            "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)
country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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
        
        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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

# biannual data
iv_vars = c("unmp",
            "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3"
)
country <- c("IT","NE-LSP")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 3, yes = 3, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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
        
        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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

# model 3  ----

# annual data
iv_vars = c("unmp","temp",
            "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3", "post_event_3_4", "post_event_3_5"
)
country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 5, yes = 5, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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
        
        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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
                                     newdata = data.frame(unmp=0,temp=0,
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

# biannual data
iv_vars = c("unmp","temp",
            "pre_event_3",
            "event_3", 
            "post_event_3_1", "post_event_3_2", "post_event_3_3"
)
country <- c("IT","NE-LSP")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        
        df_country <- df_country %>%
                filter(country == c)
        
        # create year variables
        df_country <- df_country %>%
                mutate(post_event_3 = ifelse(post_event_3 > 3, yes = 3, no = post_event_3))
        
        df_dummy <- dummy(x = df_country$post_event_3,sep = "_")
        df_country <- cbind(df_country,df_dummy)
        
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
        
        # output <- data.frame(model$coefficients)
        # colnames(output) <- "estimate"
        # output$std.error <- coef(summary(model))[,"Std. Error"]
        # output$country <- c
        # output$study_period <- y
        # output$model <- "event1_feif_post_pre"
        # output$term <- row.names(output)
        # output$r2 <- summary(model)$r.squared[1]
        # rownames(df_output) <- c()
        # output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
        # df_output <- rbind(df_output,output)
        
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
        yhat_neg_1$post <- -2
        
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
        yhat_1$post <- 2
        
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
        yhat_2$post <- 4
        
        yhat <- rbind(yhat_neg_1,yhat_0,yhat_1,yhat_2)        
        yhat$country <- c
        yhat$model <- 3
        df_model_yhat <- rbind(df_model_yhat,yhat)
        rm(yhat,yhat_neg_1,yhat_0,yhat_1,yhat_2,v,y,year,year_vars,remove)
}

# Save data sets ----

df_model_yhat$event <- 3

# write.csv(df_output,file = paste0(results,"output_event_3.csv"))
write.csv(df_model_yhat,file = paste0(results,"yhat_event_3.csv"))

beep()
        