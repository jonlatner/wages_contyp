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
library(stargazer)
library(ggplot2)

options(scipen = 999) # disable scientific notation

# load data -----

df_model_output <- readRDS(paste0(results, "results_a_predicted.rds"))
with(df_model_output,table(study_period,country))

# graph data ----

df_graph <- df_model_output %>%
        arrange(country, study_period, period)

ggplot(data = df_graph, aes(x = period, y = fit, color = study_period, group = study_period)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = seq(0,6,1)) + 
        scale_colour_gradient(low = "gray90", high = "gray10", 
                              breaks =seq(2000,2012,2),
                              name= "6 year study period, beginning") +
        geom_hline(yintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years after temporary employment contract") +
        ylab("Average marginal effect") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              # legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_base_model.pdf"), height = 5, width = 7, plot = last_plot())
