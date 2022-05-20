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
library(feisr)
library(texreg)
library(dummies)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds")) %>%
        filter(country == "DE")

# make sample -----

set.seed(1234)

df_sample_10_unique <- df_sample_0 %>%
        select(country,pid) %>%
        group_by(country,pid) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        sample_frac(.1) %>%
        mutate(keep=1)

df_sample_10 <- merge(df_sample_0,df_sample_10_unique) %>%
        filter(keep==1)

df_sample_0 <- df_sample_10

rm(df_sample_10_unique,df_sample_10)

# clean data -----

df_event_1 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_1")) %>%
        rename(event_time_pos = event_1_time_pos) %>%
        mutate(event_time_pos = as.factor(event_time_pos))

df_event_2 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_2")) %>%
        rename(event_time_pos = event_2_time_pos)

df_event_3 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_3")) %>%
        rename(event_time_pos = event_3_time_pos)

df_event_4 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,matches("event_4")) %>%
        rename(event_time_pos = event_4_time_pos)

# prepare for output ----

df_model_yhat <- data.frame()

# Variables  ----

iv_vars = c("factor(event_time_pos) + factor(year)")
iv_vars = c("event_time_pos")

# Model (Annual countries) ----

country <- c("DE")
event <- c(1)
for(c in country) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # model
                df_country <- data.frame(df_country)
                model <- feis(as.formula(paste0("ln_hourly_wage ~ ",iv_vars,"| 1")),
                              data = df_country, 
                              id = "pid")
                
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

# Predict (Annual countries) ----

# iv_vars = c("factor(event_time_pos) + factor(year)")

country <- c("DE")
event <- c(1)
for(c in country) {
        for (e in event) {
        model_event <- get(paste0("model_event_",e,"_country_",c))
        print(c)
        print(e)
        }
}


newdata <- data.frame(Species=factor("setosa", levels=c("setosa", "versicolor", "virginica")))
foo.new <- data.frame(event_time_pos=factor("1",levels=c("0","1","2","3","4","5","6","7","8")))

yhat_neg_2 <- data.frame(predict(object = model_event,
                                 se.fit = TRUE,
                                 newdata = foo.new
)) %>%
        select(fit,se.fit)

foo.new <- data.frame(event_time_pos=factor("1",levels=c("1","2","3","4","5","6","7","8")),
                      year=factor("2001",levels=c("2001","2002","2003","2004","2005","2006","2007","2008","2009",
                                                  "2010","2011","2012","2013","2014","2015","2016","2017","2018")))

        
        yhat_neg_2 <- data.frame(predict(object = model_event,
                                         se.fit = TRUE,
                                         newdata = data.frame(event_time_pos=1,year=0)
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


# graph predict ----

df_graph <- df_yhat

df_graph$event <- factor(df_graph$event,
                         levels = c(1,4,2,3),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 4 (perm to temp)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("factor(year)s before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )
