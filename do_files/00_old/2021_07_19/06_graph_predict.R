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

df_yhat_event_1 <- read.csv(paste0(results, "yhat_event_1.csv")) 
df_yhat_event_2 <- read.csv(paste0(results, "yhat_event_2.csv")) 
df_yhat_event_3 <- read.csv(paste0(results, "yhat_event_3.csv")) 
df_yhat_event_4 <- read.csv(paste0(results, "yhat_event_4.csv")) 
df_yhat <- rbind(df_yhat_event_1,df_yhat_event_2,df_yhat_event_3,df_yhat_event_4)
rm(df_yhat_event_1,df_yhat_event_2,df_yhat_event_3,df_yhat_event_4)
df_yhat$X <- NULL

# df_yhat$event <- factor(df_yhat$event,
#                            levels = c(1,2,3,4),
#                            labels = c("model 1 (temp to perm)",
#                                       "model 2 (unmp to perm)",
#                                       "model 3 (unmp to temp)",
#                                       "model 4 (perm into temp)"))


# Graph compare model 2 ----

df_graph <- df_yhat %>%
        filter(model == 2) %>%
        filter(event==1 | event == 4)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,2,3,4),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)",
                                    "Event 4 (perm to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_compare_event_1_4_model_2.pdf"), height = 5, width = 7, plot = last_plot())


df_graph <- df_yhat %>%
        filter(model == 2) %>%
        filter(post>=0) %>%
        filter(event==2 | event == 3)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,2,3,4),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)",
                                    "Event 4 (perm to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_compare_event_2_3_model_2.pdf"), height = 5, width = 7, plot = last_plot())


# Graph ----

df_graph <- df_yhat %>%
        filter(event == 1)

df_graph$model <- factor(df_graph$model,
                         levels = c(1,2,3),
                         labels = c("model 1 (Baseline)", "model 2 (+ Unmp)", "model 3 (+ Temp)"))


ggplot(data = df_graph, aes(x = post, y = fit, color = model, group = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event_1.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(event == 2) %>%
        arrange(country,event,model)

df_graph$model <- factor(df_graph$model,
                         levels = c(1,2,3),
                         labels = c("model 1 (Baseline)", "model 2 (+ Unmp)", "model 3 (+ Temp)"))


ggplot(data = df_graph, aes(x = post, y = fit, color = model, group = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event_2.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(event == 3)

df_graph$model <- factor(df_graph$model,
                         levels = c(1,2,3),
                         labels = c("model 1 (Baseline)", "model 2 (+ Unmp)", "model 3 (+ Temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = model, group = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event_3.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(event == 4)

df_graph$model <- factor(df_graph$model,
                         levels = c(1,2,3),
                         labels = c("model 1 (Baseline)", "model 2 (+ Unmp)", "model 3 (+ Temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = model, group = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
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
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event_4.pdf"), height = 5, width = 7, plot = last_plot())
