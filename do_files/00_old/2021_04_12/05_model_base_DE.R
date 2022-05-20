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
library(texreg)
library(feisr)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        select(country,study_period,pid,year,emp_status,unmp,temp,ln_hourly_wage,period,post_temp)

# prepare for output ----
df_table_output = data.frame() # output

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

country <- c("DE")
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
                feis_model <- feis(ln_hourly_wage ~ unmp + temp + factor(year) | 1, 
                                   data = df_period, 
                                   id = "pid")
                
                output_table <- data.frame(feis_model$coefficients)
                colnames(output_table) <- "coef"
                output_table$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output_table$country <- c
                output_table$study_period <- y
                output_table$model <- "fe"
                output_table$term <- row.names(output_table)
                rownames(df_table_output) <- c()
                df_table_output <- rbind(df_table_output,output_table)

        }
}

country <- c("DE")
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
                feis_model <- feis(ln_hourly_wage ~ unmp + temp | year, 
                                   data = df_period, 
                                   id = "pid")
                
                output_table <- data.frame(feis_model$coefficients)
                colnames(output_table) <- "coef"
                output_table$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output_table$country <- c
                output_table$study_period <- y
                output_table$model <- "feis"
                output_table$term <- row.names(output_table)
                rownames(df_table_output) <- c()
                df_table_output <- rbind(df_table_output,output_table)
                
        }
}

country <- c("DE")
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
                feis_model <- feis(ln_hourly_wage ~ unmp + temp + factor(post_temp) | year, 
                                   data = df_period, 
                                   id = "pid")
                
                output_table <- data.frame(feis_model$coefficients)
                colnames(output_table) <- "coef"
                output_table$std.error <- coef(summary(feis_model))[, "Std. Error"]
                output_table$country <- c
                output_table$study_period <- y
                output_table$model <- "post"
                output_table$term <- row.names(output_table)
                rownames(df_table_output) <- c()
                df_table_output <- rbind(df_table_output,output_table)
                
        }
}

# graph data ----


df_graph <- df_table_output %>%
        select(term,coef,std.error,country,study_period,model) %>%
        filter(term == "temp") %>%
        arrange(country, study_period)

ggplot(data = df_graph, aes(x = study_period, y = coef)) +
        facet_wrap(. ~ model) +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = coef-1.96*std.error,
                          ymax = coef+1.96*std.error
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

df_graph <- df_table_output %>%
        select(term,coef,std.error,country,study_period,model) %>%
        filter(term == "factor(post_temp)6") %>%
        arrange(country, study_period)

ggplot(data = df_graph, aes(x = study_period, y = coef)) +
        facet_wrap(. ~ model) +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = coef-1.96*std.error,
                          ymax = coef+1.96*std.error
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )
