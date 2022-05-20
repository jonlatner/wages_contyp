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

data_files = "projects/mobility/data_files/CH/"
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

df_ch <- readRDS(paste0(data_files, "ch_fors.rds")) 
summary(df_ch)

# clean data -----
df_ch <- df_ch %>%
        mutate(age_cat = as.factor(age_cat),
               edu_cat = as.factor(edu_cat),
               female = ifelse(male == 0, yes = 1, no = 0))
df_ch$edu_cat <- relevel(df_ch$edu_cat,ref = 2)
df_ch$age_cat <- relevel(df_ch$age_cat,ref = 2)

df_ch <- df_ch %>%
        mutate(prestige = ifelse(is.na(prestige), yes = 1, no = prestige))

iv_plm <-   "unmp + ftc + post_ftc_first + factor(year)"
iv_plm <-   "unmp + 
ftc*as.factor(age_cat) + post_ftc_first*as.factor(age_cat) + 
ftc*as.factor(edu_cat) + post_ftc_first*as.factor(edu_cat) + 
ftc*female + post_ftc_first*female + 
factor(year)"

# plm_model <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv_plm)),
#                  data = df_ch,
#                  index = c("pid","year"))
# summary(plm_model)

# Prepare data for individual fixed effects ----
df_ch_cntr <- df_ch

# create categorical dummy variables
df_dummy <- dummy(x = df_ch_cntr$year)
df_ch_cntr <- cbind(df_ch_cntr, df_dummy)

df_dummy <- dummy(x = df_ch_cntr$age_cat)
df_ch_cntr <- cbind(df_ch_cntr, df_dummy)

df_dummy <- dummy(x = df_ch_cntr$edu_cat)
df_ch_cntr <- cbind(df_ch_cntr, df_dummy)
rm(df_dummy)

# create interaction variables
iv = c(
        "edu_cat1", "edu_cat3", 
        "age_cat1", "age_cat3",
        "female"
)
ftc <- c(
        "ftc","post_ftc_first"
)
for(i in iv){
        for(f in ftc){
                df_ch_cntr$test <- df_ch_cntr[[i]]*df_ch_cntr[[f]]
                df_ch_cntr$test <- as.numeric(df_ch_cntr$test)
                names(df_ch_cntr)[names(df_ch_cntr) == 'test'] <- paste(i,f,sep = "_")
        }
}
rm(i,f,iv,ftc)

colnames(df_ch_cntr)

# center variables
vars = c(
        "unmp", 
        "ftc", "post_ftc_first",
        "age_cat1_ftc", "age_cat1_post_ftc_first",
        "age_cat3_ftc", "age_cat3_post_ftc_first",
        "edu_cat1_ftc", "edu_cat1_post_ftc_first",
        "edu_cat3_ftc", "edu_cat3_post_ftc_first", 
        "female_ftc", "female_post_ftc_first", 
        "year2001","year2002","year2003","year2004","year2005", "year2006", "year2007", "year2008", "year2009", "year2010", "year2011", "year2012", "year2013", "year2014", "year2015", "year2016", "year2017",
        "prestige","ln_hourly_wage"
)

for (v in vars) {
        df_ch_cntr$test <- group.center(df_ch_cntr[[v]], df_ch_cntr$pid) # create new variable (group centered)
        df_ch_cntr$test <- as.numeric(df_ch_cntr$test) # make it numeric (probably not necessary)
        df_ch_cntr[[v]] <- NULL # drop old variable
        names(df_ch_cntr)[names(df_ch_cntr) == "test"] <- paste0(v) # rename new variable with old variable
        
}

# View(filter(df_ch_cntr,age_cat1==1&post_ftc_first==1) %>% select(pid,year,ftc,post_ftc_first,matches("age_cat1")))
# View(filter(df_ch_cntr,pid==1242) %>% select(pid,year,ftc,post_ftc_first,matches("age_cat1")))

# independent variables -----

iv_plm <-   "unmp + 
ftc*as.factor(age_cat) + post_ftc_first*as.factor(age_cat) + 
ftc*as.factor(edu_cat) + post_ftc_first*as.factor(edu_cat) + 
factor(year)"

iv_ols <-   "unmp + 
ftc + post_ftc_first + 
age_cat1_ftc + age_cat1_post_ftc_first +
age_cat3_ftc + age_cat3_post_ftc_first +
edu_cat1_ftc + edu_cat1_post_ftc_first +
edu_cat3_ftc + edu_cat3_post_ftc_first +
female_ftc + female_post_ftc_first +
year2001 + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017"

# plm model -----

ols_model <- lm(as.formula(paste0("prestige ~ ",iv_ols)),
                 data = df_ch_cntr)
summary(ols_model)

# predict (baseline) -----

yhat_base_0 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 0,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0,
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                                      ))

yhat_base_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 1, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 0, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_base_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 2, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 0, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_base_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 3,  
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 0, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_base_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 4,  
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 0, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_base_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 5, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 0, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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
                                  ftc = 1, post_ftc_first = 0,  
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 0, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_female_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 1, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 1, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_female_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 2, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 2,
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_female_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 3,  
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 3, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_female_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 4,  
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 4, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                          ))

yhat_female_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                          newdata = data.frame(
                                  unmp = 0, tmp = 0, 
                                  ftc = 1, post_ftc_first = 5, 
                                  age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                  age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                  edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                  edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                  female_ftc = 1, female_post_ftc_first = 5, 
                                  year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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
                                      ftc = 1, post_ftc_first = 0,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 1, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 1, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 2, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 2, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 3,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 3, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 4,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 4, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 5, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 1, edu_cat1_post_ftc_first = 5, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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
                                      ftc = 1, post_ftc_first = 0,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 1, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 1, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 2, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 2, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 3,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 3, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 4,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 4, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_edu_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 5, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 1, edu_cat3_post_ftc_first = 5, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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
                                      ftc = 1, post_ftc_first = 0,  
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 0, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_1_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 1, 
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 1, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_1_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 2, 
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 2, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_1_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 3,  
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 3, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_1_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 4,  
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 4, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_1_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 5, 
                                      age_cat1_ftc = 1, age_cat1_post_ftc_first = 5, 
                                      age_cat3_ftc = 0, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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
                                      ftc = 1, post_ftc_first = 0,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 0, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_3_1 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 1, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 1, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_3_2 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 2, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 2, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_3_3 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 3,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 3, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_3_4 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 4,  
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 4, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
                              ))

yhat_age_cat_3_5 <- predict.lm(object = ols_model, se.fit = TRUE, 
                              newdata = data.frame(
                                      unmp = 0, tmp = 0, 
                                      ftc = 1, post_ftc_first = 5, 
                                      age_cat1_ftc = 0, age_cat1_post_ftc_first = 0,
                                      age_cat3_ftc = 1, age_cat3_post_ftc_first = 5, 
                                      edu_cat1_ftc = 0, edu_cat1_post_ftc_first = 0, 
                                      edu_cat3_ftc = 0, edu_cat3_post_ftc_first = 0, 
                                      female_ftc = 0, female_post_ftc_first = 0, 
                                      year2001 = 0, year2002 = 0, year2003 = 0, year2004 = 0, year2005 = 0, year2006 = 0, year2007 = 0, year2008 = 0, year2009 = 0, year2010 = 0, year2011 = 0, year2012 = 0, year2013 = 0, year2014 = 0, year2015 = 0, year2016 = 0, year2017 = 0
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

df_yhat$country = "CH"
df_yhat$post = "first"

saveRDS(df_yhat, file = paste0(results, "ch_prestige_post_ftc_first.rds"))

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

# ggsave(filename = paste0(graphs,"graph_ch_prestige.pdf"), plot = last_plot(), height = 8, width = 10, units = "in")
