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

# LIBRARY
library(tidyverse)
library(plm)
library(dummies)
library(robumeta)
library(ggplot2)
library(broom)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(sample_perm == 1 | sample_temp == 1) %>%
        mutate(post_temp_2 = post_temp*post_temp,
               edu_cat = as.numeric(edu_cat)) %>%
        mutate(edu_cat = ifelse((country == "JP" & edu_cat == 1) | (country == "CH" & edu_cat == 1), yes = 2, no = edu_cat)) %>%
        mutate(age_cat = ifelse((country == "CH" & age_cat == 3), yes = 2, no = age_cat)) %>%
        filter(study_period>=2000 & study_period < 2013)

# with(df_sample_0,table(country))
# with(df_sample_1,table(country))

df_dummy <- dummy(x = df_sample_1$age_cat)
df_sample_1 <- cbind(df_sample_1, df_dummy)

df_dummy <- dummy(x = df_sample_1$edu_cat)
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

# prepare for output ----
df_table_output = data.frame() # output
df_model_yhat <- data.frame()

# model data ----

# model data (Italy) ----
country <- c("IT")
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
                
                df_period$post_temp <- recode(df_period$post_temp,"3=2;5=3")
                df_period$post_temp_2 <- recode(df_period$post_temp_2,"9=4;25=9")

                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4"
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3), age_cat1_post_temp_2 = c(0,1,4,9),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3), age_cat3_post_temp_2 = c(0,1,4,9),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_edu_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 1, edu_cat1_temp = 1, edu_cat1_post_temp = c(0,1,2,3), edu_cat1_post_temp_2 = c(0,1,4,9),
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3), edu_cat3_post_temp_2 = c(0,1,4,9),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3),
                                                             post_temp_2 = c(0,1,4,9),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3), male_post_temp_2 = c(0,1,4,9),
                                                             period2 = 0, period3 = 0, period4 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat1 <- data.frame(as.vector(yhat_edu_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat1$variable <- "edu_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,2,4,6)
                yhat_age_cat1$period <- c(0,2,4,6)
                yhat_age_cat3$period <- c(0,2,4,6)
                yhat_edu_cat1$period <- c(0,2,4,6)
                yhat_edu_cat3$period <- c(0,2,4,6)
                yhat_male$period <- c(0,2,4,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data (Netherlands - LSP) ----
country <- c("NE-LSP")
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
                
                df_period$post_temp <- recode(df_period$post_temp,"3=2;5=3")
                df_period$post_temp_2 <- recode(df_period$post_temp_2,"9=4;25=9")
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4"
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3), age_cat1_post_temp_2 = c(0,1,4,9),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3), age_cat3_post_temp_2 = c(0,1,4,9),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_edu_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 1, edu_cat1_temp = 1, edu_cat1_post_temp = c(0,1,2,3), edu_cat1_post_temp_2 = c(0,1,4,9),
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3),
                                                                 post_temp_2 = c(0,1,4,9),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3), edu_cat3_post_temp_2 = c(0,1,4,9),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3),
                                                             post_temp_2 = c(0,1,4,9),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3), male_post_temp_2 = c(0,1,4,9),
                                                             period2 = 0, period3 = 0, period4 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat1 <- data.frame(as.vector(yhat_edu_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat1$variable <- "edu_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,2,4,6)
                yhat_age_cat1$period <- c(0,2,4,6)
                yhat_age_cat3$period <- c(0,2,4,6)
                yhat_edu_cat1$period <- c(0,2,4,6)
                yhat_edu_cat3$period <- c(0,2,4,6)
                yhat_male$period <- c(0,2,4,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data - United Kingdom (2003 - 2008) ----

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
                
                # create categorical dummy variables
                df_period <- df_period %>%
                        mutate(period1 = ifelse(year==y, yes = 1, no = 0),
                               period2 = ifelse(year==y+1, yes = 1, no = 0),
                               period3 = ifelse(year==y+2, yes = 1, no = 0),
                               period4 = ifelse(year==y+3, yes = 1, no = 0),
                               period5 = ifelse(year==y+4, yes = 1, no = 0),
                               period6 = ifelse(year==y+5, yes = 1, no = 0)
                        )

                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)

                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6"

                # ols model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                          newdata = data.frame(unmp = 0, 
                                                               temp = 1,
                                                               post_temp = c(0,1,2,3,4,5,6),
                                                               post_temp_2 = c(0,1,4,9,16,25,36),
                                                               age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                               age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                               edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                               edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                               male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                               period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                          ))

                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3,4,5,6), age_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                            ))

                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3,4,5,6), age_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                            ))
                
                yhat_edu_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 1, edu_cat1_temp = 1, edu_cat1_post_temp = c(0,1,2,3,4,5,6), edu_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                            ))

                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3,4,5,6), edu_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 1, male_temp = 1, male_post_temp = c(0,1,2,3,4,5,6), male_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0
                                            ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat1 <- data.frame(as.vector(yhat_edu_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))

                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat1$variable <- "edu_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_male$period <- c(0,1,2,3,4,5,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data - United Kingdom (2000 - 2002 & 2009 - 2012) ----

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
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)

                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"


                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")

                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3,4,5,6), age_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3,4,5,6), age_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_edu_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 1, edu_cat1_temp = 1, edu_cat1_post_temp = c(0,1,2,3,4,5,6), edu_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3,4,5,6), edu_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3,4,5,6),
                                                             post_temp_2 = c(0,1,4,9,16,25,36),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3,4,5,6), male_post_temp_2 = c(0,1,4,9,16,25,36),
                                                             period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat1 <- data.frame(as.vector(yhat_edu_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat1$variable <- "edu_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_male$period <- c(0,1,2,3,4,5,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data - Australia, Germany, Korea ----
country <- c("AU","DE","KO")
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
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3,4,5,6), age_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3,4,5,6), age_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_edu_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 1, edu_cat1_temp = 1, edu_cat1_post_temp = c(0,1,2,3,4,5,6), edu_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3,4,5,6), edu_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3,4,5,6),
                                                             post_temp_2 = c(0,1,4,9,16,25,36),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3,4,5,6), male_post_temp_2 = c(0,1,4,9,16,25,36),
                                                             period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat1 <- data.frame(as.vector(yhat_edu_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat1$variable <- "edu_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_male$period <- c(0,1,2,3,4,5,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat1,yhat_edu_cat3,yhat_male,yhat)
                
        }
}


# model data - Japan ----
country <- c("JP")
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
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3,4,5,6), age_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 1, age_cat3_temp = 1, age_cat3_post_temp = c(0,1,2,3,4,5,6), age_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))

                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3,4,5,6), edu_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3,4,5,6),
                                                             post_temp_2 = c(0,1,4,9,16,25,36),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3,4,5,6), male_post_temp_2 = c(0,1,4,9,16,25,36),
                                                             period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_age_cat3 <- data.frame(as.vector(yhat_age_cat3))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_age_cat3$variable <- "age_cat_3"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_male$period <- c(0,1,2,3,4,5,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data - Switzerland ----
country <- c("CH")
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
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat3", 
                        "age_cat1", 
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_age_cat1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 1, age_cat1_temp = 1, age_cat1_post_temp = c(0,1,2,3,4,5,6), age_cat1_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                

                yhat_edu_cat3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 1, edu_cat3_temp = 1, edu_cat3_post_temp = c(0,1,2,3,4,5,6), edu_cat3_post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                
                yhat_male <- predict.lm(object = ols_model, se.fit = TRUE, 
                                        newdata = data.frame(unmp = 0, 
                                                             temp = 1,
                                                             post_temp = c(0,1,2,3,4,5,6),
                                                             post_temp_2 = c(0,1,4,9,16,25,36),
                                                             age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                             age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                             edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                             edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                             male = 1, male_temp = 1, male_post_temp = c(0,1,2,3,4,5,6), male_post_temp_2 = c(0,1,4,9,16,25,36),
                                                             period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                        ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))
                yhat_age_cat1 <- data.frame(as.vector(yhat_age_cat1))
                yhat_edu_cat3 <- data.frame(as.vector(yhat_edu_cat3))
                yhat_male <- data.frame(as.vector(yhat_male))
                
                yhat_baseline$variable <- "base"
                yhat_age_cat1$variable <- "age_cat_1"
                yhat_edu_cat3$variable <- "edu_cat_3"
                yhat_male$variable <- "male"
                
                yhat_baseline$period <- c(0,1,2,3,4,5,6)
                yhat_age_cat1$period <- c(0,1,2,3,4,5,6)
                yhat_edu_cat3$period <- c(0,1,2,3,4,5,6)
                yhat_male$period <- c(0,1,2,3,4,5,6)
                
                yhat <- data.frame(rbind(yhat_baseline,yhat_age_cat1,yhat_edu_cat3,yhat_male))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)
                
                rm(yhat_baseline,yhat_age_cat1,yhat_age_cat3,yhat_edu_cat3,yhat_male,yhat)
                
        }
}

# model data - Netherlands (LISS) ----
country <- c("NE-LISS")
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
                
                # create categorical dummy variables
                df_dummy <- dummy(x = df_period$period)
                df_period <- cbind(df_period, df_dummy)
                rm(df_dummy)
                
                # create interaction variables
                iv = c(
                        "edu_cat1", "edu_cat3", 
                        "age_cat1", "age_cat3",
                        "male"
                )
                temp <- c(
                        "temp","post_temp","post_temp_2"
                )
                for(i in iv){
                        for(f in temp){
                                df_period$test <- df_period[[i]]*df_period[[f]]
                                df_period$test <- as.numeric(df_period$test)
                                names(df_period)[names(df_period) == 'test'] <- paste(i,f,sep = "_")
                        }
                }
                rm(i,f,iv,temp)
                
                # center variables
                vars = c(
                        "unmp", 
                        "temp", "post_temp", "post_temp_2",
                        # "age_cat1_temp", "age_cat1_post_temp", "age_cat1_post_temp_2",
                        # "age_cat3_temp", "age_cat3_post_temp", "age_cat3_post_temp_2",
                        # "edu_cat1_temp", "edu_cat1_post_temp", "edu_cat1_post_temp_2",
                        # "edu_cat3_temp", "edu_cat3_post_temp", "edu_cat3_post_temp_2",
                        # "male_temp", "male_post_temp", "male_post_temp_2", 
                        "period2","period3","period4","period5","period6","period7",
                        "ln_hourly_wage"
                )
                
                for (v in vars) {
                        df_period$test <- group.center(df_period[[v]], df_period$pid) # create new variable (group centered)
                        df_period$test <- as.numeric(df_period$test) # make it numeric (probably not necessary)
                        df_period[[v]] <- NULL # drop old variable
                        names(df_period)[names(df_period) == "test"] <- paste0(v) # rename new variable with old variable
                        
                }
                
                # independent variables
                iv_ols <-   "unmp + temp + 
                post_temp + post_temp_2 + 
                # age_cat1_temp + age_cat1_post_temp + age_cat1_post_temp_2 +
                # age_cat3_temp + age_cat3_post_temp + age_cat3_post_temp_2 +
                # edu_cat1_temp + edu_cat1_post_temp + edu_cat1_post_temp_2 +
                # edu_cat3_temp + edu_cat3_post_temp + edu_cat3_post_temp_2 +
                # male_temp + male_post_temp + male_post_temp_2 +
                period2 + period3 + period4 + period5 + period6 + period7"
                
                # model
                ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),
                                data = df_period,model=FALSE,x=FALSE,y=FALSE)
                save(ols_model, file = paste0(results,"ols_model_",c,"_",y,".rda"))
                
                # plm model
                # iv_plm <-   "unmp + temp + post_temp + post_temp_2 +
                # temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + post_temp_2*as.factor(age_cat) +
                # temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + post_temp_2*as.factor(edu_cat) +
                # temp*male + post_temp*male + post_temp_2*male +
                # factor(period)"
                # 
                # df_period_2 <- df_country %>%
                #         filter(study_period == y)
                # 
                # plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
                #                  data = df_period_2,
                #                  index = c("pid","year"))
                # 
                # stargazer(ols_model, plm_model, type = "text")
                
                # summary table
                output_table <- tidy(ols_model)
                output_table$country <- c
                output_table$study_period <- y
                df_table_output <- rbind(df_table_output,output_table)
                
                # predict -----
                
                yhat_baseline <- predict.lm(object = ols_model, se.fit = TRUE, 
                                            newdata = data.frame(unmp = 0, 
                                                                 temp = 1,
                                                                 post_temp = c(0,1,2,3,4,5,6),
                                                                 post_temp_2 = c(0,1,4,9,16,25,36),
                                                                 age_cat1 = 0, age_cat1_temp = 0, age_cat1_post_temp = 0, age_cat1_post_temp_2 = 0,
                                                                 age_cat3 = 0, age_cat3_temp = 0, age_cat3_post_temp = 0, age_cat3_post_temp_2 = 0,
                                                                 edu_cat1 = 0, edu_cat1_temp = 0, edu_cat1_post_temp = 0, edu_cat1_post_temp_2 = 0,
                                                                 edu_cat3 = 0, edu_cat3_temp = 0, edu_cat3_post_temp = 0, edu_cat3_post_temp_2 = 0,
                                                                 male = 0, male_temp = 0, male_post_temp = 0, male_post_temp_2 = 0,
                                                                 period2 = 0, period3 = 0, period4 = 0, period5 = 0, period6 = 0, period7 = 0
                                            ))
                
                yhat_baseline <- data.frame(as.vector(yhat_baseline))

                yhat_baseline$variable <- "base"

                yhat_baseline$period <- c(0,1,2,3,4,5,6)

                yhat <- data.frame(rbind(yhat_baseline))
                yhat$study_period = y
                yhat$country = c
                
                df_model_yhat <- rbind(df_model_yhat,yhat)

                rm(yhat_baseline)
        }
}

rm(df_period,df_country,ols_model,output_table,v,vars,y,year,year_1,year_2,c,country,iv_ols)

# Save data sets ----

saveRDS(df_model_yhat, file = paste0(results, "results_b_predicted.rds"))
write.csv(df_table_output, file = paste0(results, "results_b_output.csv"))

table(df_model_yhat$country)
