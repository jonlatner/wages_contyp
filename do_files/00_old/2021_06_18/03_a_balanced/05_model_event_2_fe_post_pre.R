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

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        select(country,study_period,pid,year,period,ln_hourly_wage,unmp,temp,event_2,post_event_2,pre_event_2)

df_dummy <- dummy(x = df_sample_1$period,sep = "_")
df_sample_1 <- cbind(df_sample_1,df_dummy)

df_dummy <- dummy(x = df_sample_1$post_event_2,sep = "_")
df_sample_1 <- cbind(df_sample_1,df_dummy)

# prepare for output ----

df_output = data.frame() # output
df_model_yhat <- data.frame()

# model United Kingdom ----

# with(subset(df_sample_1,country == "UK"),table(study_period,period))
# with(subset(df_sample_1,country == "UK"),table(study_period,post_event_2))

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(seq(2003,2008,1))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      unmp + temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + post_event_2_3 + post_event_2_4 + 
                                      period_2 + period_3 + period_4 + period_5 + period_6 | 1,
                              data = df_period,
                              id = "pid")
                
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=1,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0))) %>%
                        select(fit,se.fit)
                yhat_3$study_period = y
                yhat_3$country = c
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=1,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0))) %>%
                        select(fit,se.fit)
                yhat_4$study_period = y
                yhat_4$country = c
                yhat_4$post <- 4
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)
        }
}

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year_1 <- c(2000,2001,2002)
        year_2 <- c(seq(2009,2012,1))
        year <- c(year_1,year_2)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      unmp + temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + post_event_2_3 + post_event_2_4 + post_event_2_5 + 
                                      period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1,
                              data = df_period,
                              id = "pid")
                
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=1,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_3$study_period = y
                yhat_3$country = c
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=1,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_4$study_period = y
                yhat_4$country = c
                yhat_4$post <- 4
                
                yhat_5 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=1,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_5$study_period = y
                yhat_5$country = c
                yhat_5$post <- 5
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)
        }
}


# model (Netherlands LISS) ----

# In LISS data,in study period 2011,no one has post_event_2 == 5
# with(subset(df_sample_1,country == "NE-LISS"),table(study_period,post_event_2))
# also few caes of unemployed is unemployed
# with(subset(df_sample_1,country == "NE-LISS"),table(study_period,unmp))

country <- c("NE-LISS")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(2008,2009,2010,2012)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + post_event_2_3 + post_event_2_4 + post_event_2_5 + 
                                      period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1,
                              data = df_period,
                              id = "pid")
                
                # Save model output
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=1,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_3$study_period = y
                yhat_3$country = c
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=1,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_4$study_period = y
                yhat_4$country = c
                yhat_4$post <- 4
                
                yhat_5 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=1,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_5$study_period = y
                yhat_5$country = c
                yhat_5$post <- 5
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)
        }
}

country <- c("NE-LISS")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(2011)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + post_event_2_3 + post_event_2_4 + 
                                      period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1,
                              data = df_period,
                              id = "pid")
                
                # Save model output
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=1,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_3$study_period = y
                yhat_3$country = c
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=1,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_4$study_period = y
                yhat_4$country = c
                yhat_4$post <- 4
                
                yhat_5 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_5$study_period = y
                yhat_5$country = c
                yhat_5$post <- 5
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)
        }
}

# model (annual countries) ----

country <- c("AU","CH","DE","JP","KO")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      unmp + temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + post_event_2_3 + post_event_2_4 + post_event_2_5 + 
                                      period_2 + period_3 + period_4 + period_5 + period_6 + period_7 | 1,
                              data = df_period,
                              id = "pid")
                
                
                # Save model output
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0
                
                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 1
                
                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,post_event_2_3=0,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 2
                
                
                yhat_3 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=1,post_event_2_4=0,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_3$study_period = y
                yhat_3$country = c
                yhat_3$post <- 3
                
                yhat_4 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=1,post_event_2_5=0,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_4$study_period = y
                yhat_4$country = c
                yhat_4$post <- 4
                
                yhat_5 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=0,post_event_2_3=0,post_event_2_4=0,post_event_2_5=1,
                                                                  period_2=0,period_3=0,period_4=0,period_5=0,period_6=0,period_7=0))) %>%
                        select(fit,se.fit)
                yhat_5$study_period = y
                yhat_5$country = c
                yhat_5$post <- 5
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)
        }
}


# model (biannual countries) ----

country <- c("NE-LSP","IT")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                model <- feis(ln_hourly_wage ~ 
                                      unmp + temp + pre_event_2 + event_2 + 
                                      post_event_2_1 + post_event_2_2 + 
                                      period_2 + period_3 + period_4 | 1,
                              data = df_period,
                              id = "pid")
                
                # Save model output
                output <- data.frame(model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(model))[,"Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "event2_feif_post_pre"
                output$term <- row.names(output)
                output$r2 <- summary(model)$r.squared[1]
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(period)",replacement = "period_",output$term,fixed = TRUE)
                df_output <- rbind(df_output,output)
                
                # predict
                yhat_0 <- data.frame(predict(object = model,
                                           se.fit = TRUE,
                                           newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                event_2=1,
                                                                post_event_2_1=0,post_event_2_2=0,
                                                                period_2=0,period_3=0,period_4=0))) %>%
                        select(fit,se.fit)
                yhat_0$study_period = y
                yhat_0$country = c
                yhat_0$post <- 0

                yhat_1 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=1,post_event_2_2=0,
                                                                  period_2=0,period_3=0,period_4=0))) %>%
                        select(fit,se.fit)
                yhat_1$study_period = y
                yhat_1$country = c
                yhat_1$post <- 2

                yhat_2 <- data.frame(predict(object = model,
                                             se.fit = TRUE,
                                             newdata = data.frame(unmp=0,temp=1,pre_event_2=0,
                                                                  event_2=1,
                                                                  post_event_2_1=0,post_event_2_2=1,
                                                                  period_2=0,period_3=0,period_4=0))) %>%
                        select(fit,se.fit)
                yhat_2$study_period = y
                yhat_2$country = c
                yhat_2$post <- 4
                
                yhat <- rbind(yhat_0,yhat_1,yhat_2)        
                df_model_yhat <- rbind(df_model_yhat,yhat)
                rm(yhat,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4,yhat_5)
        }
}


# Save data sets ----

df_model_yhat$model <- "event2_feif_post_pre"
df_model_yhat$event <- 2

write.csv(df_output,file = paste0(results,"output_event2_feif_post_pre.csv"))
write.csv(df_model_yhat,file = paste0(results,"yhat_event2_feif_post_pre.csv"))

