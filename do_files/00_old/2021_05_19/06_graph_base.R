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

df_output <- read.csv(paste0(results, "results_output_base.csv")) 
df_output$X <- NULL

# graph data ----

df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,model) %>%
        filter(term == "temp") %>%
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

# ggsave(paste0(graphs,"graph_period_0.pdf"), height = 5, width = 7, plot = last_plot())


df_graph <- df_output %>%
        select(term,estimate,std.error,country,study_period,post) %>%
        filter(term == "temp") %>%
        arrange(country, study_period, post)

ggplot(data = df_graph, aes(x = post, y = estimate, color = study_period, group = study_period)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = seq(0,6,1)) + 
        scale_colour_gradient(low = "gray90", high = "gray10", 
                              breaks =seq(2000,2012,2),
                              name= "7 year study period, beginning") +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years after temporary employment contract") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              # legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

# ggsave(paste0(graphs,"graph_model_base.pdf"), height = 5, width = 7, plot = last_plot())

