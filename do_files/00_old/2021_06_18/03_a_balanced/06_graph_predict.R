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

df_yhat_fe_0 <- read.csv(paste0(results, "yhat_fe.csv")) 
df_yhat_fe_1 <- read.csv(paste0(results, "yhat_event1_feif.csv")) 
df_yhat_fe_2 <- read.csv(paste0(results, "yhat_event1_feif_post.csv")) 
df_yhat_fe_3 <- read.csv(paste0(results, "yhat_event1_feif_post_pre.csv")) 

df_yhat_fe_4 <- read.csv(paste0(results, "yhat_event2_feif.csv")) 
df_yhat_fe_5 <- read.csv(paste0(results, "yhat_event2_feif_post.csv")) 
df_yhat_fe_6 <- read.csv(paste0(results, "yhat_event2_feif_post_pre.csv")) 

df_yhat_feis_0 <- read.csv(paste0(results, "yhat_feis.csv")) 
df_yhat_feis_1 <- read.csv(paste0(results, "yhat_event1_feis_if.csv")) 
df_yhat_feis_2 <- read.csv(paste0(results, "yhat_event1_feis_if_post.csv")) 
df_yhat_feis_3 <- read.csv(paste0(results, "yhat_event1_feis_if_post_pre.csv")) 

df_yhat_feis_4 <- read.csv(paste0(results, "yhat_event2_feis_if.csv")) 
df_yhat_feis_5 <- read.csv(paste0(results, "yhat_event2_feis_if_post.csv")) 
df_yhat_feis_6 <- read.csv(paste0(results, "yhat_event2_feis_if_post_pre.csv")) 

df_yhat <- rbind(df_yhat_fe_0,df_yhat_fe_1,df_yhat_fe_2,df_yhat_fe_3,df_yhat_fe_4,df_yhat_fe_5,df_yhat_fe_6)
df_yhat_if <- rbind(df_yhat_feis_0,df_yhat_feis_1,df_yhat_feis_2,df_yhat_feis_3,df_yhat_feis_4,df_yhat_feis_5,df_yhat_feis_6)
df_yhat <- rbind(df_yhat,df_yhat_if)
df_yhat$X <- NULL

rm(df_yhat_fe_0,df_yhat_fe_1,df_yhat_fe_2,df_yhat_fe_3,df_yhat_fe_4,df_yhat_fe_5,df_yhat_fe_6)
rm(df_yhat_if,df_yhat_feis_0,df_yhat_feis_1,df_yhat_feis_2,df_yhat_feis_3,df_yhat_feis_4,df_yhat_feis_5,df_yhat_feis_6)

df_yhat <- arrange(df_yhat,model,country,study_period)

# Compare FE, FEIS ----

df_graph <- filter(df_yhat,model=="fe"|model=="feis")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

ggsave(paste0(graphs,"graph_fe_feis.pdf"), height = 5, width = 7, plot = last_plot())

# Compare FE  ----

table(df_yhat$model)

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="fe"|model=="event1_feif"|model=="event2_feif")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        # guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_fe_event1_event2.pdf"), height = 5, width = 7, plot = last_plot())

table(df_yhat$model)

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="fe"|model=="event1_feif"|model=="event1_feif_post"|model=="event1_feif_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_fe_compare_period_0_event1.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="fe"|model=="event2_feif"|model=="event2_feif_post"|model=="event2_feif_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_fe_compare_period_0_event2.pdf"), height = 5, width = 7, plot = last_plot())

# event 1 ----
df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feif_post")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

ggsave(paste0(graphs,"graph_event1_feif_post_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feif_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

ggsave(paste0(graphs,"graph_event1_feif_post_pre_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feif_post" | model=="event1_feif_post_pre") %>%
        mutate(model = ifelse(model == "event1_feif_post", yes = "model3", no = "model4" ))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post), linetype = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

# event 2 ----
df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event2_feif_post")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

ggsave(paste0(graphs,"graph_event2_feif_post_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event2_feif_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
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

ggsave(paste0(graphs,"graph_event2_feif_post_pre_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

# Compare FEIS  ----

table(df_yhat$model)

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="feis"|model=="event1_feis_if"|model=="event2_feis_if")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        # guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_feis_event1_event2.pdf"), height = 5, width = 7, plot = last_plot())

table(df_yhat$model)

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="feis"|model=="event1_feis_if"|model=="event1_feis_if_post"|model=="event1_feis_if_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_feis_compare_period_0_event1.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0) %>%
        filter(model=="feis"|model=="event2_feis_if"|model=="event2_feis_if_post"|model=="event2_feis_if_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        guides(colour = guide_legend(nrow = 2)) + 
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_feis_compare_period_0_event2.pdf"), height = 5, width = 7, plot = last_plot())

# event 1 ----
df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feis_if_post")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event1_feis_if_post_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feis_if_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event1_feis_if_post_pre_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event1_feis_if_post" | model=="event1_feis_if_post_pre") %>%
        mutate(model = ifelse(model == "event1_feis_if_post", yes = "model3", no = "model4" ))

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post), linetype = model)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

# event 2 ----
df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event2_feis_if_post")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event2_feis_if_post_compare_period.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post==0|post==2|post==4) %>%
        filter(model=="event2_feis_if_post_pre")

ggplot(data = df_graph, aes(x = study_period, y = fit, color = as.factor(post))) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = .5) +
        scale_x_continuous(breaks =seq(2000,2012,4)) +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("7 year study period beginning") +
        ylab("Effeiscts on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_event2_feis_if_post_pre_compare_period.pdf"), height = 5, width = 7, plot = last_plot())
