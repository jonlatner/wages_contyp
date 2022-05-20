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

data_files = "data_files/"
results = "results/"
tables = "tables/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(car)
library(forcats)

options(scipen = 999) # disable scientific notation

# Load data -----

df_sample_0 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_sample_0 <- df_sample_0 %>%
        filter(country!="NE-LISS")
df_sample_0$country <- recode(df_sample_0$country, "'NE-LSP'='NE'")

# All events -----

# Create data 
df_event_t_p_01 <- df_sample_0 %>%
        filter(event_t_p_yes_final == 1) %>%
        mutate(time=year-event_t_p_year) %>%
        mutate(time = ifelse(time < -2, yes = NA, 
                             ifelse(time > 4, yes = NA, 
                                    no = time))) %>%
        filter(!is.na(time)) %>%
        select(country, pid, year, unmp, temp, perm, time)

df_event_p_t_01 <- df_sample_0 %>%
        filter(event_p_t_yes_final == 1) %>%
        mutate(time=year-event_p_t_year) %>%
        mutate(time = ifelse(time < -2, yes = NA, 
                             ifelse(time > 4, yes = NA, 
                                    no = time))) %>%
        filter(!is.na(time)) %>%
        select(country, pid, year, unmp,temp, perm, time)

df_event_u_t_01 <- df_sample_0 %>%
        filter(event_u_t_yes_final == 1) %>%
        mutate(time=year-event_u_t_year) %>%
        mutate(time = ifelse(time < -2, yes = NA, 
                             ifelse(time > 4, yes = NA, 
                                    no = time))) %>%
        filter(!is.na(time)) %>%
        select(country, pid, year, unmp,temp, perm, time)


df_event_u_p_01 <- df_sample_0 %>%
        filter(event_u_p_yes_final == 1) %>%
        mutate(time=year-event_u_p_year) %>%
        mutate(time = ifelse(time < -2, yes = NA, 
                             ifelse(time > 4, yes = NA, 
                                    no = time))) %>%
        filter(!is.na(time)) %>%
        select(country, pid, year, unmp,temp, perm, time)

# Summarize data

event_p_t_02 <- with(df_event_p_t_01,table(country,time))
event_p_t_02

df_event_p_t_agg <- data.frame(event_p_t_02) %>%
        mutate(type="p_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_t_p_02 <- with(df_event_t_p_01,table(country,time))
df_event_t_p_agg <- data.frame(event_t_p_02) %>%
        mutate(type="t_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_p_02 <- with(df_event_u_p_01,table(country,time))
df_event_u_p_agg <- data.frame(event_u_p_02) %>%
        mutate(type="u_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_t_02 <- with(df_event_u_t_01,table(country,time))
df_event_u_t_agg <- data.frame(event_u_t_02) %>%
        mutate(type="u_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

df_event_first <- rbind(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)
rm(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)

# Graph ----

df_graph <- df_event_first %>%
        filter(time!=-3)

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# ggplot(df_graph, aes(x=time, y=Freq)) +
#         facet_grid(country_name~type) +
#         geom_bar(stat="identity")+
#         theme_bw() + 
#         xlab("Years before/after event") +
#         scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
#         geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 2) +
#         theme(panel.grid.minor = element_blank(), 
#               legend.position = "bottom",
#               legend.title = element_blank(),
#               legend.key.width=unit(2, "cm"),
#               axis.title.y = element_blank(),
#               axis.line.y = element_line(color="black", size=.5),
#               axis.line.x = element_line(color="black", size=.5)
#         )
# ggsave(paste0(graphs,"graph_country_first_event_timeseq.pdf"), height = 9, width = 7, plot = last_plot())

ggplot(df_graph, aes(x=time, y=Freq)) +
        facet_grid(type~country_name) +
        geom_bar(stat="identity")+
        theme_bw() + 
        xlab("Years before/after event") +
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 2.5) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_event_timeseq_presentation.pdf"), height = 6, width = 9, plot = last_plot())

