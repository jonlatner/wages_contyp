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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "support_files/"
results = "results/"
tables = "tables/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(janitor)
library(car)

options(scipen = 999) # disable scientific notation

# Load data ----

# oecd.stat <- labour <- labour force statistics <- permanent temporary employment <- incidence of permanent employment

df_tmp_01 <- read_csv(paste0(data_files,"OECD/OECD_tmp_emp_share.csv"), quote = "")
df_tmp_01 <- as.data.frame(sapply(df_tmp_01, function(x) gsub("\"", "", x)))
df_tmp_01 <- clean_names(df_tmp_01)

# Clean data ----

df_tmp_02 <- df_tmp_01 %>%
        filter(age_2 == "25 to 54",
               sex_2 == "All persons",
               series == "PER_CENT_TEMP",
               employment_status == "Dependent employment",
               ) %>%
        mutate(value = as.numeric(as.character(value))/100,
               time = as.numeric(as.character(time))) %>%
        rename(year = time,
               ) %>%
        select(country, year, value, series)
df_tmp_02 <- droplevels(df_tmp_02)

df_tmp_02$country_name <- recode(df_tmp_02$country, "'AUS'='Australia'; 'CHE'='Switzerland'; 'DEU'='Germany'; 'ITA'='Italy'; 'JPN'='Japan'; 'KOR'='Korea'; 'NLD'='Netherlands'; 'GBR'='United Kingdom'; else=NA")

df_tmp_02 <- df_tmp_02 %>%
        filter(!is.na(country_name))

# Graph ----

df_graph <- df_tmp_02

ggplot(df_graph, aes(x = year, y = value)) +
        facet_wrap(~country_name, nrow = 2) +
        geom_line(size = 1) +
        scale_x_continuous(limits=c(2000,2020)) +
        scale_y_continuous(breaks = seq(0, .25, by = .05), limits=c(0,.25)) +
        ylab("Percent temporary employed (25-54)") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size = .5),
              axis.line.x = element_line(color="black", size = .5),
              axis.title.x = element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text.align = 0,
              legend.key = element_blank()
        )

# ggsave(paste0(graphs,"oecd_temp_emp_pct.pdf"), height = 4, width = 8, plot = last_plot())

