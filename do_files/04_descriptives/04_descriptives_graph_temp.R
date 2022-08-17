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
library(janitor)
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
        filter(unmp==0) %>%
        group_by(country_name, year) %>%
        summarise(value = mean(temp)) %>%
        ungroup() %>%
        mutate(source = "Sample (B)")

# oecd.stat <- labour <- labour force statistics <- permanent temporary employment <- incidence of permanent employment

df_tmp_01 <- read_csv(paste0(support_files,"OECD/OECD_tmp_emp_share.csv"), quote = "")
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
        select(country, year, value) %>%
        mutate(source = "OECD")
df_tmp_02 <- droplevels(df_tmp_02)

df_tmp_02$country_name <- recode(df_tmp_02$country, "'AUS'='Australia'; 'CHE'='Switzerland'; 'DEU'='Germany'; 'ITA'='Italy'; 'JPN'='Japan'; 'KOR'='Korea'; 'NLD'='Netherlands'; 'GBR'='United Kingdom'; else=NA")

df_tmp_02 <- df_tmp_02 %>%
        select(-country) %>%
        filter(!is.na(country_name))

# Graph  ----

df_graph <- rbind(df_sample_unmp,df_tmp_02)

ggplot(data = df_graph, aes(x = year, y = value, color = source)) +
        facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0,.3)) +
        scale_color_grey(start = 0, end = 0.7) +
        xlab("Year") +
        ylab("Temporary employment rate (25-54)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot=last_plot(),paste0(graphs,"graph_descriptives_temp_paper.pdf"), height = 6, width = 9)
