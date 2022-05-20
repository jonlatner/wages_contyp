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

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean_unbalanced.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
        filter(unmp==0) %>%
        filter(country == "DE") %>%
        select(country,study_period,pid,year,unmp,temp,ln_hourly_wage,period,post_temp) %>%
        group_by(country,study_period,pid) %>%
        mutate(count = row_number(),
               max = max(count)) %>%
        ungroup() %>%
        filter(max>2)

df_id <- df_sample_1 %>%
        select(study_period,pid) %>%
        group_by(study_period,pid) %>%
        slice(1) %>%
        group_by(study_period) %>%
        mutate(id = row_number()) %>%
        ungroup()
df_sample_1 <- merge(df_sample_1,df_id) %>%
        arrange(pid,year)
df_sample_small <- df_sample_1 %>%
        filter(id < 500)

df_sample_small <- df_sample_1



# prepare for output ----
df_output = data.frame() # output

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

country <- unique(sort(df_sample_small$country))
for(c in country) {
        print(c)
        df_country <- df_sample_small
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp + factor(year) | 1, 
                                   data = df_period, 
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "fe"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(year)", replacement = "year_", output$term, fixed = TRUE)
                df_output <- rbind(df_output,output)
                
        }
}

for(c in country) {
        print(c)
        df_country <- df_sample_small
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp | year,
                                   data = df_period,
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "feis"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(year)", replacement = "year_", output$term, fixed = TRUE)
                df_output <- rbind(df_output,output)
                
        }
}

for(c in country) {
        print(c)
        df_country <- df_sample_small
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp + factor(post_temp) | year,
                                   data = df_period,
                                   id = "pid")
                
                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(year)", replacement = "year_", output$term, fixed = TRUE)
                output$term <- gsub(pattern = "factor(post_temp)", replacement = "post_temp_", output$term, fixed = TRUE)
                df_output <- rbind(df_output,output)
                
        }
}

for(c in country) {
        print(c)
        df_country <- df_sample_small
        df_country <- df_country %>%
                filter(country == c)
        year <- unique(sort(df_country$study_period))
        for(y in year) {
                print(y)
                df_period <- df_country %>%
                        filter(study_period == y)
                
                # model
                df_period <- data.frame(df_period)
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp | year,
                                   data = df_period,
                                   id = "pid")

                output <- data.frame(feis_model$coefficients)
                colnames(output) <- "estimate"
                output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
                output$country <- c
                output$study_period <- y
                output$model <- "post_cont"
                output$term <- row.names(output)
                rownames(df_output) <- c()
                output$term <- gsub(pattern = "factor(year)", replacement = "year_", output$term, fixed = TRUE)
                output$term <- gsub(pattern = "factor(post_temp)", replacement = "post_temp_", output$term, fixed = TRUE)
                df_output <- rbind(df_output,output)
                
        }
}

# graph data ----

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(term == "temp") %>%
        arrange(country, study_period)

ggplot(data = df_graph, aes(x = study_period, y = estimate)) +
        facet_wrap(. ~ model) +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
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

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(model == "post") %>%
        arrange(country, study_period)

df_graph_2 <- df_graph %>%
        arrange(study_period,term)

ggplot(data = df_graph, aes(x = study_period, y = estimate)) +
        facet_wrap(. ~ term, scales = "free_y") +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
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

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(model == "post_cont") %>%
        filter(term == "post_temp") %>%
        arrange(country, study_period)
