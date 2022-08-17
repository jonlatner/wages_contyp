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

# Adapt this pathway!
setwd("~/GitHub/wages_contyp/")

support_files = "support_files/"
data_files = "data_files/"
results = "results/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car)
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)
library(janitor)
library(readxl)
library(zoo)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load sample data -----

df_original_01 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_original_01 <- df_original_01 %>%
        filter(country!="NE-LISS")
df_original_01$country <- recode(df_original_01$country, "'NE-LSP'='NE'")

df_original_01$country_name <- recode(df_original_01$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_sample_unmp <- df_original_01 %>%
        group_by(country_name, year) %>%
        summarise(unmp = mean(unmp)) %>%
        ungroup() %>%
        mutate(source = "Sample (A)")

df_world_bank_unmp <- df_original_01 %>%
        group_by(country_name, year) %>%
        summarise(unmp = mean(unemployment_rate)/100) %>%
        ungroup() %>%
        mutate(source = "World Bank")

# Graph  ----

df_graph <- rbind(df_sample_unmp,df_world_bank_unmp)

df_graph$source <- relevel(as.factor(df_graph$source), "World Bank")

ggplot(data = df_graph, aes(x = year, y = unmp, color = source)) +
        facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        scale_color_grey(start = 0, end = 0.7) +
        xlab("Year") +
        scale_y_continuous(limits = c(0,.2)) +
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

ggsave(plot=last_plot(),paste0(graphs,"graph_descriptives_unmp_paper.pdf"), height = 6, width = 9)
