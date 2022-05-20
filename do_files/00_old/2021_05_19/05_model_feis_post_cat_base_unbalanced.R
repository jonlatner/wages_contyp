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
library(dummies)

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
        filter(max>2)

# create categorical dummy variables
df_dummy <- dummy(x = df_sample_1$post_temp)
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

# prepare for output ----
df_output = data.frame() # output
df_yhat = data.frame() # output

# annual countries ----

country <- c("AU","CH","DE","JP","KO","NE-LISS")
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
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp1 + post_temp2 + post_temp3 + post_temp4 + post_temp5 | year,
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)

                t_0 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_1 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_2 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 1, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_3 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 1, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_4 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 1, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_5 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 1),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                yhat <- rbind(t_0, t_1, t_2, t_3, t_4, t_5)
                yhat <- yhat %>%
                        mutate(year = row_number()-1)
                
                yhat$country <- c
                yhat$study_period <- y
                df_yhat <- rbind(df_yhat,yhat)
        }
}

# biannual countries ----

# df_output <- df_output %>%
#         filter(country != "IT") %>%
#         filter(country != "NE-LSP")
# df_yhat <- df_yhat %>%
#         filter(country != "IT") %>%
#         filter(country != "NE-LSP")

country <- c("IT", "NE-LSP")
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
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp2 + post_temp4 | year,
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)
                
                t_0 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_2 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp2 = 1, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_4 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp4 = 1),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                yhat <- rbind(t_0, t_2, t_4)
                yhat <- yhat %>%
                        mutate(year = row_number()-1)
                
                yhat$country <- c
                yhat$study_period <- y
                df_yhat <- rbind(df_yhat,yhat)
        }
}

# United Kingdom ----

with(subset(df_sample_1,country == "UK"),table(study_period,post_temp))

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(2000,2001,2002,2004,2005,2006,2007,2009,2010,2011,2012)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp1 + post_temp2 + post_temp3 + post_temp4 + post_temp5 | year,
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)
                
                t_0 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_1 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_2 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 1, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_3 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 1, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_4 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 1, 
                                                               post_temp5 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_5 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 1),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                yhat <- rbind(t_0, t_1, t_2, t_3, t_4, t_5)
                yhat <- yhat %>%
                        mutate(year = row_number()-1)
                
                yhat$country <- c
                yhat$study_period <- y
                df_yhat <- rbind(df_yhat,yhat)
        }
}

country <- c("UK")
for(c in country) {
        print(c)
        df_country <- df_sample_1
        df_country <- df_country %>%
                filter(country == c)
        year <- c(2003,2008)
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp1 + post_temp2 + post_temp3 + post_temp4 | year,
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                df_output <- rbind(df_output,output)
                
                t_0 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_1 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_2 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 1, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_3 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 1, 
                                                               post_temp4 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_4 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 1),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                yhat <- rbind(t_0, t_1, t_2, t_3, t_4)
                yhat <- yhat %>%
                        mutate(year = row_number()-1)
                
                yhat$country <- c
                yhat$study_period <- y
                df_yhat <- rbind(df_yhat,yhat)
        }
}

# Save data sets ----

write.csv(df_output, file = paste0(results, "results_output_feis_post_base_unbalanced.csv"))
write.csv(df_yhat, file = paste0(results, "results_yhat_feis_post_base_unbalanced.csv"))
