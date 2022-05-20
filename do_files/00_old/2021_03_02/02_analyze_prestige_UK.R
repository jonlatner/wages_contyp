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
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)
library(broom)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "df_sample_clean.rds")) 
df_uk <- df_uk %>%
        filter(country == "UK")
summary(df_uk)

# clean data -----
df_uk <- df_uk %>%
        mutate(age_cat = as.factor(age_cat),
               edu_cat = as.factor(edu_cat),
               female = ifelse(male == 1, yes = 0, no = 1))
df_uk$edu_cat <- relevel(df_uk$edu_cat,ref = 2)
df_uk$age_cat <- relevel(df_uk$age_cat,ref = 2)

iv_plm <-   "unmp + tmp + temp + post_temp + factor(year)"
iv_plm <-   "unmp + tmp +
temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + 
temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + 
temp*female + post_temp*female + 
factor(year)"

# plm_model <- plm(as.formula(paste0("prestige ~ ",iv_plm)),
#                  data = df_uk,
#                  index = c("pid","year"))
# summary(plm_model)

# Prepare data for individual fixed effects ----
df_uk_cntr <- df_uk

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
        "age_cat1", "age_cat3",
        "female"
)
temp <- c(
        "temp","post_temp"
)
for(i in iv){
        for(f in temp){
                df_uk_cntr$test <- df_uk_cntr[[i]]*df_uk_cntr[[f]]
                df_uk_cntr$test <- as.numeric(df_uk_cntr$test)
                names(df_uk_cntr)[names(df_uk_cntr) == 'test'] <- paste(i,f,sep = "_")
        }
}
rm(i,f,iv,temp)

colnames(df_uk_cntr)

# center variables
vars = c(
        "unmp", "temp", "post_temp",
        "age_cat1_temp", "age_cat1_post_temp",
        "age_cat3_temp", "age_cat3_post_temp",
        "edu_cat1_temp", "edu_cat1_post_temp",
        "edu_cat3_temp", "edu_cat3_post_temp", 
        "female_temp", "female_post_temp", 
        "year2001","year2002","year2003","year2004","year2005", "year2006", "year2007", "year2008", "year2009", "year2010", "year2011", "year2012", "year2013", "year2014", "year2015", "year2016", "year2017", "year2018",
        "prestige","prestige"
)

for (v in vars) {
        df_uk_cntr$test <- group.center(df_uk_cntr[[v]], df_uk_cntr$pid) # create new variable (group centered)
        df_uk_cntr$test <- as.numeric(df_uk_cntr$test) # make it numeric (probably not necessary)
        df_uk_cntr[[v]] <- NULL # drop old variable
        names(df_uk_cntr)[names(df_uk_cntr) == "test"] <- paste0(v) # rename new variable with old variable
        
}

# independent variables -----

iv_plm <-   "unmp + 
temp*as.factor(age_cat) + post_temp*as.factor(age_cat) + 
temp*as.factor(edu_cat) + post_temp*as.factor(edu_cat) + 
factor(year)"

iv_ols <-   "unmp + temp + post_temp + 
age_cat1_temp + age_cat1_post_temp +
age_cat3_temp + age_cat3_post_temp +
edu_cat1_temp + edu_cat1_post_temp +
edu_cat3_temp + edu_cat3_post_temp +
female_temp + female_post_temp +
year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017 + year2018"

# plm model -----

ols_model <- lm(as.formula(paste0("prestige ~ ",iv_ols)),
                data = df_uk_cntr)
summary(ols_model)
df_model <- tidy(ols_model)
df_model$rsq <- summary(ols_model)$r.squared[2]

# predict (baseline) -----

yhat_base_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 0,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0,
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_base_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 1, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_base_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 2, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_base_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 3,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_base_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 4,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_base_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 5, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 0, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))
yhat_base_0 <- data.frame(as.vector(yhat_base_0))
yhat_base_1 <- data.frame(as.vector(yhat_base_1))
yhat_base_2 <- data.frame(as.vector(yhat_base_2))
yhat_base_3 <- data.frame(as.vector(yhat_base_3))
yhat_base_4 <- data.frame(as.vector(yhat_base_4))
yhat_base_5 <- data.frame(as.vector(yhat_base_5))

# female -----

yhat_female_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 0,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 1, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 1, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 2, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 2,
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 3,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 3, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 4,  
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 4, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  temp = 1, post_temp = 5, 
                                  age_cat1_temp = 0, age_cat1_post_temp = 0,
                                  age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                  edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                  edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                  female_temp = 1, female_post_temp = 5, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                          ))

yhat_female_0 <- data.frame(as.vector(yhat_female_0))
yhat_female_1 <- data.frame(as.vector(yhat_female_1))
yhat_female_2 <- data.frame(as.vector(yhat_female_2))
yhat_female_3 <- data.frame(as.vector(yhat_female_3))
yhat_female_4 <- data.frame(as.vector(yhat_female_4))
yhat_female_5 <- data.frame(as.vector(yhat_female_5))

# edu_cat1 -----

yhat_edu_cat_1_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 0,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 1, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 1, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 2, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 2, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 3,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 3, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 4,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 4, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 5, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 1, edu_cat1_post_temp = 5, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
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
                                       temp = 1, post_temp = 0,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 1, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 1, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 2, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 2, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 3,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 3, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 4,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 4, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_edu_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 5, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 1, edu_cat3_post_temp = 5, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
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
                                       temp = 1, post_temp = 0,  
                                       age_cat1_temp = 1, age_cat1_post_temp = 0, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 1, 
                                       age_cat1_temp = 1, age_cat1_post_temp = 1, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 2, 
                                       age_cat1_temp = 1, age_cat1_post_temp = 2, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 3,  
                                       age_cat1_temp = 1, age_cat1_post_temp = 3, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 4,  
                                       age_cat1_temp = 1, age_cat1_post_temp = 4, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 5, 
                                       age_cat1_temp = 1, age_cat1_post_temp = 5, 
                                       age_cat3_temp = 0, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
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
                                       temp = 1, post_temp = 0,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 0, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 1, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 1, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 2, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 2, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 3,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 3, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 4,  
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 4, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
                               ))

yhat_age_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                               newdata = data.frame(
                                       unmp = 0, tmp = 0, 
                                       temp = 1, post_temp = 5, 
                                       age_cat1_temp = 0, age_cat1_post_temp = 0,
                                       age_cat3_temp = 1, age_cat3_post_temp = 5, 
                                       edu_cat1_temp = 0, edu_cat1_post_temp = 0, 
                                       edu_cat3_temp = 0, edu_cat3_post_temp = 0, 
                                       female_temp = 0, female_post_temp = 0, 
                                       year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0, year2018 = 0
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
        yhat_female_0,yhat_female_1,yhat_female_2,yhat_female_3,yhat_female_4,yhat_female_5,
        yhat_edu_cat_1_0,yhat_edu_cat_1_1,yhat_edu_cat_1_2,yhat_edu_cat_1_3,yhat_edu_cat_1_4,yhat_edu_cat_1_5,
        yhat_edu_cat_3_0,yhat_edu_cat_3_1,yhat_edu_cat_3_2,yhat_edu_cat_3_3,yhat_edu_cat_3_4,yhat_edu_cat_3_5,
        yhat_age_cat_1_0,yhat_age_cat_1_1,yhat_age_cat_1_2,yhat_age_cat_1_3,yhat_age_cat_1_4,yhat_age_cat_1_5,
        yhat_age_cat_3_0,yhat_age_cat_3_1,yhat_age_cat_3_2,yhat_age_cat_3_3,yhat_age_cat_3_4,yhat_age_cat_3_5
))


# df_yhat$names <- c("yhat_base_0","yhat_base_1","yhat_base_2","yhat_base_3","yhat_base_4","yhat_base_5","yhat_female_0","yhat_female_1","yhat_female_2","yhat_female_3","yhat_female_4","yhat_female_5","yhat_edu_cat_1_0","yhat_edu_cat_1_1","yhat_edu_cat_1_2","yhat_edu_cat_1_3","yhat_edu_cat_1_4","yhat_edu_cat_1_5","yhat_edu_cat_3_0","yhat_edu_cat_3_1","yhat_edu_cat_3_2","yhat_edu_cat_3_3","yhat_edu_cat_3_4","yhat_edu_cat_3_5","yhat_age_cat_1_0","yhat_age_cat_1_1","yhat_age_cat_1_2","yhat_age_cat_1_3","yhat_age_cat_1_4","yhat_age_cat_1_5","yhat_age_cat_3_0","yhat_age_cat_3_1","yhat_age_cat_3_2","yhat_age_cat_3_3","yhat_age_cat_3_4","yhat_age_cat_3_5")
rownames(df_yhat) <- seq.int(nrow(df_yhat))
df_yhat$category <- c("base", "base", "base", "base", "base", "base",
                      "female", "female", "female", "female", "female", "female",
                      "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1", "edu_cat_1",
                      "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3", "edu_cat_3",
                      "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1", "age_cat_1",
                      "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3", "age_cat_3"
)
df_yhat$period <- c(0,1,2,3,4,5,
                    0,1,2,3,4,5,
                    0,1,2,3,4,5,
                    0,1,2,3,4,5,
                    0,1,2,3,4,5,
                    0,1,2,3,4,5
)

df_yhat <- df_yhat %>%
        select(-residual.scale,-df)

rm(yhat_base_0,yhat_base_1,yhat_base_2,yhat_base_3,yhat_base_4,yhat_base_5,
   yhat_female_0,yhat_female_1,yhat_female_2,yhat_female_3,yhat_female_4,yhat_female_5,
   yhat_edu_cat_1_0,yhat_edu_cat_1_1,yhat_edu_cat_1_2,yhat_edu_cat_1_3,yhat_edu_cat_1_4,yhat_edu_cat_1_5,
   yhat_edu_cat_3_0,yhat_edu_cat_3_1,yhat_edu_cat_3_2,yhat_edu_cat_3_3,yhat_edu_cat_3_4,yhat_edu_cat_3_5,
   yhat_age_cat_1_0,yhat_age_cat_1_1,yhat_age_cat_1_2,yhat_age_cat_1_3,yhat_age_cat_1_4,yhat_age_cat_1_5,
   yhat_age_cat_3_0,yhat_age_cat_3_1,yhat_age_cat_3_2,yhat_age_cat_3_3,yhat_age_cat_3_4,yhat_age_cat_3_5
)

df_yhat$country = "UK"
df_yhat$post = "any"

write.csv(df_yhat, file = paste0(results, "prestige_mfx_uk.csv"))
write.csv(df_model, file = paste0(results, "prestige_results_uk.csv"))

# Graph ----

ggplot(data = df_yhat, aes(x = period, y = fit)) +
        facet_grid(~category) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        # scale_y_continuous(breaks = c(seq(-.5, .5, by = .2)), limits = c(-.5, .5)) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2)

