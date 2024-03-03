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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

support_files = "support_files/"
data_files = "data_files/"
results = "results/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(janitor)

options(scipen = 999) # disable scientific notation

# load sample data -----

df_oecd_01 <- read.csv(paste0(support_files,"OECD/oecd_union_density.csv"))
df_oecd_01 <- clean_names(df_oecd_01)
df_oecd_01 <- df_oecd_01 %>%
  select(reference_area, measure_2, unit_of_measure, time_period, obs_value)

# Graph  ----

ggplot(data = df_oecd_01, aes(x = time_period, y = obs_value)) +
        facet_wrap(. ~ reference_area) +
        geom_line(size = 1) +
        xlab("Year") +
        ylab("Unemployment rate") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

# ggsave(plot=last_plot(),paste0(graphs,"graph_descriptives_union.pdf"), height = 6, width = 9)
