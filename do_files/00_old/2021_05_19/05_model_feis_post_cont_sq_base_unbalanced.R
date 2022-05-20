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
library(feisr)
library(texreg)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean_unbalanced.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(unmp==0) %>%
        select(country,study_period,pid,year,temp,ln_hourly_wage,period,post_temp) %>%
        group_by(country,study_period,pid) %>%
        mutate(count = row_number(),
               max = max(count)) %>%
        ungroup() %>%
        filter(max>2) %>%
        mutate(post_temp_sq = post_temp * post_temp)

# prepare for output ----
df_output = data.frame() # output
df_yhat <- data.frame()

# post model ----

country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
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
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp + post_temp_sq | year, 
                                   data = df_period, 
                                   id = "pid")

                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post_cont_sq"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)

                # predict
                yhat <- data.frame(predict(object = feis_model,
                                                    newdata = data.frame(temp = 1,
                                                                         post_temp = c(0,1,2,3,4,5),
                                                                         post_temp_sq = c(0,1,4,9,16,25)),
                                                    se.fit = TRUE)) %>%
                        select(fit,se.fit) %>%
                        mutate(period = row_number()-1)
                yhat$variable <- "base"
                yhat$type <- "cont_sq"
                yhat$period <- c(0,1,2,3,4,5)
                yhat$study_period = y
                yhat$country = c

                df_yhat <- rbind(df_yhat,yhat)

                
        }
}

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
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp + post_temp_sq | year, 
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post_cont_sq"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)
                
                # predict
                yhat <- data.frame(predict(object = feis_model,
                                           newdata = data.frame(temp = 1,
                                                                post_temp = c(0,2,4),
                                                                post_temp_sq = c(0,2,4)),
                                           se.fit = TRUE)) %>%
                        select(fit,se.fit)
                yhat$variable <- "base"
                yhat$type <- "cont_sq"
                yhat$period <- c(0,2,4)
                yhat$study_period = y
                yhat$country = c
                
                df_yhat <- rbind(df_yhat,yhat)
                
                
        }
}

# Save data sets ----

write.csv(df_output, file = paste0(results, "results_output_feis_post_cont_sq_base_unbalanced.csv"))
write.csv(df_yhat, file = paste0(results, "results_yhat_feis_post_cont_sq_base_unbalanced.csv"))
