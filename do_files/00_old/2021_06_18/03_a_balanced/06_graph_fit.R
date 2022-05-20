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
results = "projects/mobility/results/balanced/"
graphs = "projects/mobility/graphs/"

# LIBRARY
library(tidyverse)
library(ggplot2)

options(scipen = 999) # disable scientific notation

# load data -----

df_output_fe_0 <- read.csv(paste0(results, "output_fe.csv")) 
df_output_fe_1 <- read.csv(paste0(results, "output_event1_feif.csv")) 
df_output_fe_2 <- read.csv(paste0(results, "output_event1_feif_post.csv")) 
df_output_fe_3 <- read.csv(paste0(results, "output_event1_feif_post_pre.csv")) 

df_output_fe_4 <- read.csv(paste0(results, "output_event2_feif.csv")) 
df_output_fe_5 <- read.csv(paste0(results, "output_event2_feif_post.csv")) 
df_output_fe_6 <- read.csv(paste0(results, "output_event2_feif_post_pre.csv")) 

df_output_feis_0 <- read.csv(paste0(results, "output_feis.csv")) 
df_output_feis_1 <- read.csv(paste0(results, "output_event1_feis_if.csv")) 
df_output_feis_2 <- read.csv(paste0(results, "output_event1_feis_if_post.csv")) 
df_output_feis_3 <- read.csv(paste0(results, "output_event1_feis_if_post_pre.csv")) 

df_output_feis_4 <- read.csv(paste0(results, "output_event2_feis_if.csv")) 
df_output_feis_5 <- read.csv(paste0(results, "output_event2_feis_if_post.csv")) 
df_output_feis_6 <- read.csv(paste0(results, "output_event2_feis_if_post_pre.csv")) 

df_output <- rbind(df_output_fe_0,df_output_fe_1,df_output_fe_2,df_output_fe_3,df_output_fe_4,df_output_fe_5,df_output_fe_6)
df_output_if <- rbind(df_output_feis_0,df_output_feis_1,df_output_feis_2,df_output_feis_3,df_output_feis_4,df_output_feis_5,df_output_feis_6)
df_output <- rbind(df_output,df_output_if)
df_output$X <- NULL

rm(df_output_fe_0,df_output_fe_1,df_output_fe_2,df_output_fe_3,df_output_fe_4,df_output_fe_5,df_output_fe_6)
rm(df_output_if,df_output_feis_0,df_output_feis_1,df_output_feis_2,df_output_feis_3,df_output_feis_4,df_output_feis_5,df_output_feis_6)

df_output <- select(df_output,model,country,study_period,r2)
df_fit <- unique(df_output)
        
# Compare FE, FEIS ----

df_graph <- filter(df_fit,
                   model=="fe"|model=="feis"
                   )

ggplot(data = df_graph, aes(x = study_period, y = r2, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        xlab("7 year study period beginning") +
        ylab("R2") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_fit_fe_feis.pdf"), height = 5, width = 7, plot = last_plot())

# Compare FE ----

# event 1
df_graph <- df_fit %>%
        filter(model=="fe"|model=="event1_feif"|model=="event1_feif_post"|model=="event1_feif_post_pre") %>%
        arrange(country,study_period)

ggplot(data = df_graph, aes(x = study_period, y = r2, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        xlab("7 year study period beginning") +
        ylab("R2") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_fit_fe_event1.pdf"), height = 5, width = 7, plot = last_plot())

# event 2
df_graph <- df_fit %>%
        filter(model=="fe"|model=="event2_feif"|model=="event2_feif_post"|model=="event2_feif_post_pre") %>%
        arrange(country,study_period)

ggplot(data = df_graph, aes(x = study_period, y = r2, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        xlab("7 year study period beginning") +
        ylab("R2") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_fit_fe_event2.pdf"), height = 5, width = 7, plot = last_plot())

# Compare FEIS ----

# event 1
df_graph <- df_fit %>%
        filter(model=="fe"|model=="event1_feis_if"|model=="event1_feis_if_post"|model=="event1_feis_if_post_pre") %>%
        arrange(country,study_period)

ggplot(data = df_graph, aes(x = study_period, y = r2, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        xlab("7 year study period beginning") +
        ylab("R2") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_fit_feis_event1.pdf"), height = 5, width = 7, plot = last_plot())

# event 2
df_graph <- df_fit %>%
        filter(model=="fe"|model=="event2_feis_if"|model=="event2_feis_if_post"|model=="event2_feis_if_post_pre") %>%
        arrange(country,study_period)

ggplot(data = df_graph, aes(x = study_period, y = r2, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        xlab("7 year study period beginning") +
        ylab("R2") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_fit_feis_event2.pdf"), height = 5, width = 7, plot = last_plot())
