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
library(dummies)

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

# create categorical dummy variables
df_dummy <- dummy(x = df_sample_1$post_temp)
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

# df_id <- df_sample_1 %>%
#         select(study_period,pid) %>%
#         group_by(study_period,pid) %>%
#         slice(1) %>%
#         group_by(study_period) %>%
#         mutate(id = row_number()) %>%
#         ungroup()
# df_sample_1 <- merge(df_sample_1,df_id) %>%
#         arrange(pid,year)
# df_sample_small <- df_sample_1 %>%
#         filter(id < 1000)

df_sample_small <- df_sample_1



# prepare for output ----
df_output = data.frame() # output
df_yhat = data.frame() # output
country <- unique(sort(df_sample_small$country))

# model data (AU, CH, DE, JP, KO, NE-LISS) countries with annual surveys ----

# for(c in country) {
#         print(c)
#         df_country <- df_sample_small
#         df_country <- df_country %>%
#                 filter(country == c)
#         year <- unique(sort(df_country$study_period))
#         for(y in year) {
#                 print(y)
#                 df_period <- df_country %>%
#                         filter(study_period == y)
# 
#                 # model
#                 df_period <- data.frame(df_period)
#                 feis_model <- feis(ln_hourly_wage ~ temp + factor(year) | 1, 
#                                    data = df_period, 
#                                    id = "pid")
#                 
#                 output <- data.frame(feis_model$coefficients)
#                 colnames(output) <- "estimate"
#                 output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
#                 output$country <- c
#                 output$study_period <- y
#                 output$model <- "fe"
#                 output$term <- row.names(output)
#                 rownames(df_output) <- c()
#                 output$term <- gsub(pattern = "factor(year)", replacement = "year_", output$term, fixed = TRUE)
#                 df_output <- rbind(df_output,output)
# 
#         }
# }

# for(c in country) {
#         print(c)
#         df_country <- df_sample_small
#         df_country <- df_country %>%
#                 filter(country == c)
#         year <- unique(sort(df_country$study_period))
#         for(y in year) {
#                 print(y)
#                 df_period <- df_country %>%
#                         filter(study_period == y)
# 
#                 # model
#                 df_period <- data.frame(df_period)
#                 feis_model <- feis(ln_hourly_wage ~ temp | year,
#                                    data = df_period,
#                                    id = "pid")
# 
#                 output <- data.frame(feis_model$coefficients)
#                 colnames(output) <- "estimate"
#                 output$std.error <- coefficients(summary(feis_model))[, "Std. Error"]
#                 output$country <- c
#                 output$study_period <- y
#                 output$model <- "feis"
#                 output$term <- row.names(output)
#                 rownames(df_output) <- c()
#                 df_output <- rbind(df_output,output)
# 
#         }
# }

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
                feis_model <- feis(ln_hourly_wage ~ temp + post_temp1 + post_temp2 + post_temp3 + post_temp4 + post_temp5 | year,
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
                df_output <- rbind(df_output,output)

                t_0 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0, 
                                                               post_temp6 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_1 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 1, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0, 
                                                               post_temp6 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_2 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 1, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0, 
                                                               post_temp6 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_3 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 1, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 0, 
                                                               post_temp6 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_4 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 1, 
                                                               post_temp5 = 0, 
                                                               post_temp6 = 0),
                                          se.fit = TRUE, 
                                          interval = "confidence"))
                
                t_5 <- data.frame(predict(feis_model, 
                                          newdata = data.frame(temp = 1, 
                                                               post_temp1 = 0, 
                                                               post_temp2 = 0, 
                                                               post_temp3 = 0, 
                                                               post_temp4 = 0, 
                                                               post_temp5 = 1, 
                                                               post_temp6 = 0),
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

write.csv(df_output, file = paste0(results, "results_output_base_unbalanced_DE.csv"))
write.csv(df_yhat, file = paste0(results, "results_yhat_base_unbalanced_DE.csv"))

df_output <- read.csv(paste0(results, "results_output_base_unbalanced_DE.csv"))
df_yhat <- read.csv(paste0(results, "results_yhat_base_unbalanced_DE.csv"))
# df_output_de <- df_output_de %>%
#         filter(model == "post")

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
        facet_wrap(. ~ term) +
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

df_graph <- df_yhat %>%
        select(matches("fit"),year,country,study_period)  %>%
        filter(year == 1 | year == 3 | year == 5) %>%
        rename(fit = fit.fit,
               lwr = fit.lwr,
               upr = fit.upr)

ggplot(data = df_graph, aes(x = study_period, y = fit, color = year, group = year)) +
        facet_wrap(. ~ country) +
        geom_line(size = 1) +
        # scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        scale_colour_gradient(low = "lightblue", high = "darkblue", 
                              breaks =seq(2000,2012,2),
                              name= "7 year study period, beginning") +
        geom_errorbar(aes(ymin = lwr,
                          ymax = upr
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
