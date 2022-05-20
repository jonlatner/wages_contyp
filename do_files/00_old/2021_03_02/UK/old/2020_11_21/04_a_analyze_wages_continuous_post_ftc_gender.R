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

data_files = "projects/mobility/data_files/UK/"
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 

# clean data -----

df_uk <- df_uk %>%
        mutate(age_cat = as.factor(age_cat),
               edu_cat = as.factor(edu_cat),
               male = ifelse(male==0, yes = "f", no = "m"))
df_uk$edu_cat <- relevel(df_uk$edu_cat,ref = 2)
df_uk$age_cat <- relevel(df_uk$age_cat,ref = 2)

df_uk <- df_uk %>%
        mutate(ln_hourly_wage = ifelse(is.na(ln_hourly_wage), yes = 1, no = ln_hourly_wage))

df_uk_m <- filter(df_uk, male == "m")
df_uk_f <- filter(df_uk, male == "f")

# Prepare data for individual fixed effects ----

gender <- c("m","f")
for(m in gender) {
        df_uk_cntr <- filter(df_uk, male == m)
        
        # create categorical dummy variables
        df_dummy <- dummy(x = df_uk_cntr$year)
        df_uk_cntr <- cbind(df_uk_cntr, df_dummy)
        
        df_dummy <- dummy(x = df_uk_cntr$age_cat)
        df_uk_cntr <- cbind(df_uk_cntr, df_dummy)
        
        df_dummy <- dummy(x = df_uk_cntr$edu_cat)
        df_uk_cntr <- cbind(df_uk_cntr, df_dummy)
        rm(df_dummy)
        
        # create interaction variables
        iv = c(
                "edu_cat1", "edu_cat3", 
                "age_cat1", "age_cat3"
        )
        ftc <- c(
                "ftc","post_ftc"
        )
        for(i in iv){
                for(f in ftc){
                        df_uk_cntr$test <- df_uk_cntr[[i]]*df_uk_cntr[[f]]
                        df_uk_cntr$test <- as.numeric(df_uk_cntr$test)
                        names(df_uk_cntr)[names(df_uk_cntr) == 'test'] <- paste(i,f,sep = "_")
                }
        }
        rm(i,f,iv,ftc)
        
        colnames(df_uk_cntr)
        
        # center variables
        vars = c(
                "unmp", "tmp", "ftc", "post_ftc",
                "age_cat1_ftc", "age_cat1_post_ftc",
                "age_cat3_ftc", "age_cat3_post_ftc",
                "edu_cat1_ftc", "edu_cat1_post_ftc",
                "edu_cat3_ftc", "edu_cat3_post_ftc", 
                "year2001","year2002","year2003","year2004","year2005", "year2006", "year2007", "year2008", "year2009", "year2010", "year2011", "year2012", "year2013", "year2014", "year2015", "year2016",
                "prestige","ln_hourly_wage"
        )
        
        for (v in vars) {
                df_uk_cntr$test <- group.center(df_uk_cntr[[v]], df_uk_cntr$pid) # create new variable (group centered)
                df_uk_cntr$test <- as.numeric(df_uk_cntr$test) # make it numeric (probably not necessary)
                df_uk_cntr[[v]] <- NULL # drop old variable
                names(df_uk_cntr)[names(df_uk_cntr) == "test"] <- paste0(v) # rename new variable with old variable
                
        }
        
        # assign output
        assign(paste("df_uk_cntr",m,sep = "_"), df_uk_cntr)
}

# independent variables -----

iv_plm <-   "unmp + tmp + 
ftc*as.factor(age_cat) + post_ftc*as.factor(age_cat) + 
ftc*as.factor(edu_cat) + post_ftc*as.factor(edu_cat) + 
factor(year)"

iv_ols <-   "unmp + tmp + ftc + post_ftc + 
age_cat1_ftc + age_cat1_post_ftc +
age_cat3_ftc + age_cat3_post_ftc +
edu_cat1_ftc + edu_cat1_post_ftc +
edu_cat3_ftc + edu_cat3_post_ftc +
year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016"

# plm model -----

# ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),data = df_uk_cntr_m)
# plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),data = df_uk_m,index = c("pid","year"))
# stargazer(ols_model, plm_model, type = "text",
#           omit = c("year"))

gender <- c("m","f")
for(m in gender) {
        df_data <- get(paste0("df_uk_cntr_",m))
        ols_model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_ols)),data = df_data)
        assign(paste0("ols_model_",m), ols_model)
}

# stargazer(ols_model_m, ols_model_f, type = "text",
#           keep = c("ftc", "post"),
#           omit = c("year"))

# predict (baseline) -----

gender <- c("m","f")
for(m in gender) {
        ols_model <- get(paste0("ols_model_",m))
        
        yhat_base_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 0,  
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0,
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        
        yhat_base_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 1, 
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        
        yhat_base_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 2, 
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        
        yhat_base_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 3,  
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        
        yhat_base_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 4,  
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        
        yhat_base_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                  newdata = data.frame(
                                          unmp = 0, tmp = 0, 
                                          ftc = 1, post_ftc = 5, 
                                          age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                          age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                          edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                          edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                          male_ftc = 0, male_post_ftc = 0, 
                                          year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                  ))
        yhat_base_0 <- data.frame(as.vector(yhat_base_0))
        yhat_base_1 <- data.frame(as.vector(yhat_base_1))
        yhat_base_2 <- data.frame(as.vector(yhat_base_2))
        yhat_base_3 <- data.frame(as.vector(yhat_base_3))
        yhat_base_4 <- data.frame(as.vector(yhat_base_4))
        yhat_base_5 <- data.frame(as.vector(yhat_base_5))
        
        # edu_cat1 -----
        
        yhat_edu_cat_1_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 0,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 1, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 1, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 2, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 2, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 3,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 3, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 4,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 4, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 5, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 1, edu_cat1_post_ftc = 5, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_1_0 <- data.frame(as.vector(yhat_edu_cat_1_0))
        yhat_edu_cat_1_1 <- data.frame(as.vector(yhat_edu_cat_1_1))
        yhat_edu_cat_1_2 <- data.frame(as.vector(yhat_edu_cat_1_2))
        yhat_edu_cat_1_3 <- data.frame(as.vector(yhat_edu_cat_1_3))
        yhat_edu_cat_1_4 <- data.frame(as.vector(yhat_edu_cat_1_4))
        yhat_edu_cat_1_5 <- data.frame(as.vector(yhat_edu_cat_1_5))
        
        # edu_cat3 -----
        
        yhat_edu_cat_3_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 0,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 1, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 1, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 2, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 2, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 3,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 3, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 4,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 4, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 5, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 1, edu_cat3_post_ftc = 5, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_edu_cat_3_0 <- data.frame(as.vector(yhat_edu_cat_3_0))
        yhat_edu_cat_3_1 <- data.frame(as.vector(yhat_edu_cat_3_1))
        yhat_edu_cat_3_2 <- data.frame(as.vector(yhat_edu_cat_3_2))
        yhat_edu_cat_3_3 <- data.frame(as.vector(yhat_edu_cat_3_3))
        yhat_edu_cat_3_4 <- data.frame(as.vector(yhat_edu_cat_3_4))
        yhat_edu_cat_3_5 <- data.frame(as.vector(yhat_edu_cat_3_5))
        
        # age_cat1 -----
        
        yhat_age_cat_1_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 0,  
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 0, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 1, 
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 1, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 2, 
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 2, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 3,  
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 3, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 4,  
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 4, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 5, 
                                               age_cat1_ftc = 1, age_cat1_post_ftc = 5, 
                                               age_cat3_ftc = 0, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_1_0 <- data.frame(as.vector(yhat_age_cat_1_0))
        yhat_age_cat_1_1 <- data.frame(as.vector(yhat_age_cat_1_1))
        yhat_age_cat_1_2 <- data.frame(as.vector(yhat_age_cat_1_2))
        yhat_age_cat_1_3 <- data.frame(as.vector(yhat_age_cat_1_3))
        yhat_age_cat_1_4 <- data.frame(as.vector(yhat_age_cat_1_4))
        yhat_age_cat_1_5 <- data.frame(as.vector(yhat_age_cat_1_5))
        
        # age_cat3 -----
        
        yhat_age_cat_3_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 0,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 0, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 1, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 1, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 2, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 2, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 3,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 3, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 4,  
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 4, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                                       newdata = data.frame(
                                               unmp = 0, tmp = 0, 
                                               ftc = 1, post_ftc = 5, 
                                               age_cat1_ftc = 0, age_cat1_post_ftc = 0,
                                               age_cat3_ftc = 1, age_cat3_post_ftc = 5, 
                                               edu_cat1_ftc = 0, edu_cat1_post_ftc = 0, 
                                               edu_cat3_ftc = 0, edu_cat3_post_ftc = 0, 
                                               male_ftc = 0, male_post_ftc = 0, 
                                               year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0
                                       ))
        
        yhat_age_cat_3_0 <- data.frame(as.vector(yhat_age_cat_3_0))
        yhat_age_cat_3_1 <- data.frame(as.vector(yhat_age_cat_3_1))
        yhat_age_cat_3_2 <- data.frame(as.vector(yhat_age_cat_3_2))
        yhat_age_cat_3_3 <- data.frame(as.vector(yhat_age_cat_3_3))
        yhat_age_cat_3_4 <- data.frame(as.vector(yhat_age_cat_3_4))
        yhat_age_cat_3_5 <- data.frame(as.vector(yhat_age_cat_3_5))
        
        # combine predicted fit and se.fit into a single data frame -----
        
        df_yhat <- data.frame(rbind(
                yhat_base_0,yhat_base_1,yhat_base_2,yhat_base_3,yhat_base_4,yhat_base_5,
                yhat_edu_cat_1_0,yhat_edu_cat_1_1,yhat_edu_cat_1_2,yhat_edu_cat_1_3,yhat_edu_cat_1_4,yhat_edu_cat_1_5,
                yhat_edu_cat_3_0,yhat_edu_cat_3_1,yhat_edu_cat_3_2,yhat_edu_cat_3_3,yhat_edu_cat_3_4,yhat_edu_cat_3_5,
                yhat_age_cat_1_0,yhat_age_cat_1_1,yhat_age_cat_1_2,yhat_age_cat_1_3,yhat_age_cat_1_4,yhat_age_cat_1_5,
                yhat_age_cat_3_0,yhat_age_cat_3_1,yhat_age_cat_3_2,yhat_age_cat_3_3,yhat_age_cat_3_4,yhat_age_cat_3_5
        ))
        
        
        rownames(df_yhat) <- seq.int(nrow(df_yhat))
        df_yhat$category <- c("base", "base", "base", "base", "base", "base",
                              "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1",
                              "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3",
                              "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1",
                              "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3"
        )
        df_yhat$period <- c(0,1,2,3,4,5,
                            0,1,2,3,4,5,
                            0,1,2,3,4,5,
                            0,1,2,3,4,5,
                            0,1,2,3,4,5
        )
        
        df_yhat <- df_yhat %>%
                select(-residual.scale,-df)
        
        rm(yhat_base_0,yhat_base_1,yhat_base_2,yhat_base_3,yhat_base_4,yhat_base_5,
           yhat_edu_cat_1_0,yhat_edu_cat_1_1,yhat_edu_cat_1_2,yhat_edu_cat_1_3,yhat_edu_cat_1_4,yhat_edu_cat_1_5,
           yhat_edu_cat_3_0,yhat_edu_cat_3_1,yhat_edu_cat_3_2,yhat_edu_cat_3_3,yhat_edu_cat_3_4,yhat_edu_cat_3_5,
           yhat_age_cat_1_0,yhat_age_cat_1_1,yhat_age_cat_1_2,yhat_age_cat_1_3,yhat_age_cat_1_4,yhat_age_cat_1_5,
           yhat_age_cat_3_0,yhat_age_cat_3_1,yhat_age_cat_3_2,yhat_age_cat_3_3,yhat_age_cat_3_4,yhat_age_cat_3_5
        )
        
        df_yhat$country = "UK"
        df_yhat$gender = m
        assign(paste0("df_yhat_",m), df_yhat)
        
}

df_yhat <- rbind(df_yhat_m,df_yhat_f)
saveRDS(df_yhat, file = paste0(results, "uk_wages.rds"))

# Graph ----

ggplot(data = df_yhat, aes(x = period, y = fit, color = gender)) +
        facet_grid(~category) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2)

ggsave(filename = paste0(graphs,"graph_uk_wages.pdf"), plot = last_plot(), height = 8, width = 10, units = "in")



