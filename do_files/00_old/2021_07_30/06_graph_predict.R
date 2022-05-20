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

# load data -----

df_yhat_event_1 <- read.csv(paste0(results, "yhat_event_1_country.csv")) 
df_yhat_event_2 <- read.csv(paste0(results, "yhat_event_2_country.csv")) 
df_yhat_event_3 <- read.csv(paste0(results, "yhat_event_3_country.csv")) 
df_yhat_event_4 <- read.csv(paste0(results, "yhat_event_4_country.csv")) 
df_yhat <- rbind(df_yhat_event_1,df_yhat_event_2,df_yhat_event_3,df_yhat_event_4)
rm(df_yhat_event_1,df_yhat_event_2,df_yhat_event_3,df_yhat_event_4)
df_yhat$X <- NULL

# Graph compare ----

df_graph <- df_yhat

df_graph$model <- factor(df_graph$model)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,2,3,4),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)",
                                    "Event 4 (perm to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit)) +
        facet_wrap(. ~ event, scales = "free_y") +
        geom_line(size = .5) +
        # scale_x_continuous(breaks =seq(2000,2012,4)) +
        # geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              # legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_original.pdf"), height = 5, width = 7, plot = last_plot())

# Graph compare ----

df_graph <- df_yhat %>%
        mutate(type = ifelse(event==1 | event == 4, yes = "Type 1", no = "Type 2"))

df_graph$model <- factor(df_graph$model)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,4,2,3),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 4 (perm to temp)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ type) +
        geom_line(size = .5) +
        # scale_x_continuous(breaks =seq(2000,2012,4)) +
        # geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=2,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_events.pdf"), height = 5, width = 7, plot = last_plot())
