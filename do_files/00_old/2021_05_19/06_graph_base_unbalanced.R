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
graphs = "projects/mobility/graphs/"

# LIBRARY
library(tidyverse)
library(ggplot2)

options(scipen = 999) # disable scientific notation

# load output data -----

# df_output <- read.csv(paste0(results, "results_output_base_unbalanced.csv")) 
# df_output$X <- NULL

df_output_fe <- read.csv(paste0(results, "results_output_fe_base_unbalanced.csv")) 
df_output_fe$X <- NULL

df_output_feis <- read.csv(paste0(results, "results_output_feis_base_unbalanced.csv")) 
df_output_feis$X <- NULL

df_output_feis_post <- read.csv(paste0(results, "results_output_feis_post_base_unbalanced.csv")) 
df_output_feis_post$X <- NULL

df_output_feis_post_cont <- read.csv(paste0(results, "results_output_feis_post_cont_base_unbalanced.csv")) 
df_output_feis_post_cont$X <- NULL

df_output_feis_post_cont_sq <- read.csv(paste0(results, "results_output_feis_post_cont_sq_base_unbalanced.csv")) 
df_output_feis_post_cont_sq$X <- NULL

df_output <- rbind(df_output_fe,df_output_feis,df_output_feis_post,df_output_feis_post_cont,df_output_feis_post_cont_sq)
df_output <- df_output %>%
        mutate(model = ifelse(model == "post", yes = "feis_post_cat", no = model))

rm(df_output_fe,df_output_feis,df_output_feis_post,df_output_feis_post_cont,df_output_feis_post_cont_sq)

# load yhat data -----

df_yhat_feis_post <- read.csv(paste0(results, "results_yhat_feis_post_base_unbalanced.csv")) 
df_yhat_feis_post$X <- NULL
df_yhat_feis_post <- df_yhat_feis_post %>%
        rename(period = year) %>%
        mutate(fit = fit.fit,
               type = "cat",
               period = ifelse((country == "IT" | country == "NE-LSP") & period == 2, yes = 4,
                               ifelse((country == "IT" | country == "NE-LSP") & period == 1, yes = 2, no = period))) %>%
        select(study_period,country,fit,se.fit,period,type)

df_yhat_feis_post_cont <- read.csv(paste0(results, "results_yhat_feis_post_cont_base_unbalanced.csv")) 
df_yhat_feis_post_cont$X <- NULL
df_yhat_feis_post_cont <- df_yhat_feis_post_cont %>%
        select(study_period,country,fit,se.fit,period,type)

df_yhat_feis_post_cont_sq <- read.csv(paste0(results, "results_yhat_feis_post_cont_sq_base_unbalanced.csv")) 
df_yhat_feis_post_cont_sq$X <- NULL
df_yhat_feis_post_cont_sq <- df_yhat_feis_post_cont_sq %>%
        select(study_period,country,fit,se.fit,period,type)

df_yhat <- rbind(df_yhat_feis_post,df_yhat_feis_post_cont,df_yhat_feis_post_cont_sq)
rm(df_yhat_feis_post,df_yhat_feis_post_cont,df_yhat_feis_post_cont_sq)

df_yhat <- df_yhat %>%
        mutate(type = ifelse(type == "cont", "linear", 
                             ifelse(type == "cont_sq", "squared", 
                                    ifelse(type == "cat", "categorical", no = ""))))

# graph yhat data ----

df_graph <- df_yhat %>%
        select(fit,se.fit,period,country,study_period,type)  %>%
        filter(period == 2) %>%
        mutate(period = as.factor(period))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = type, group = type)) +
        facet_wrap(. ~ country, scale = "free_y") +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        # scale_colour_gradient(low = "lightblue", high = "darkblue", 
        #                       breaks =seq(2000,2012,2),
        #                       name= "7 year study period, beginning") +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
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

ggsave(paste0(graphs,"graph_period_2_feis_post_comp.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        select(fit,se.fit,period,country,study_period,type)  %>%
        filter(period == 4) %>%
        mutate(period = as.factor(period))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = type, group = type)) +
        facet_wrap(. ~ country, scale = "free_y") +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        # scale_colour_gradient(low = "lightblue", high = "darkblue", 
        #                       breaks =seq(2000,2012,2),
        #                       name= "7 year study period, beginning") +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
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

ggsave(paste0(graphs,"graph_period_4_feis_post_comp.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        select(fit,se.fit,period,country,study_period,type)  %>%
        filter(period == 5) %>%
        mutate(period = as.factor(period))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = type, group = type)) +
        facet_wrap(. ~ country, scale = "free_y") +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        # scale_colour_gradient(low = "lightblue", high = "darkblue", 
        #                       breaks =seq(2000,2012,2),
        #                       name= "7 year study period, beginning") +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
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

ggsave(paste0(graphs,"graph_period_5_feis_post_comp.pdf"), height = 5, width = 7, plot = last_plot())


df_graph <- df_yhat %>%
        select(fit,se.fit,period,country,study_period,type)  %>%
        filter(type == "squared") %>%
        filter(period == 0 | period == 2 | period == 4 | period == 5) %>%
        mutate(period = as.factor(period))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = period, group = period)) +
        facet_wrap(. ~ country, scale = "free_y") +
        geom_line(size = 1) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        # scale_colour_gradient(low = "lightblue", high = "darkblue", 
        #                       breaks =seq(2000,2012,2),
        #                       name= "7 year study period, beginning") +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
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

ggsave(paste0(graphs,"graph_model_base_post_squared.pdf"), height = 5, width = 7, plot = last_plot())

df_graph_2 <- df_yhat %>%
        select(fit,se.fit,period,country,study_period,type)  %>%
        filter(country == "DE" & study_period == 2002) %>%
        filter(period == 2 | period == 4 | period == 5) %>%
        arrange(country,study_period,period,type)

head(df_graph_2)

# graph output data ----

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(term == "temp") %>%
        filter(model == "fe" | model == "feis" | model == "feis_post_cat") %>%
        arrange(country, study_period, model)

ggplot(data = df_graph, aes(x = study_period, y = estimate, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
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

ggsave(paste0(graphs,"graph_period_0.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(term == "temp") %>%
        filter(model == "feis_post_cat" | model == "post_cont" | model == "post_cont_sq") %>%
        arrange(country, study_period, model)

ggplot(data = df_graph, aes(x = study_period, y = estimate, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
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

ggsave(paste0(graphs,"graph_period_0_feis_post_comp.pdf"), height = 5, width = 7, plot = last_plot())

