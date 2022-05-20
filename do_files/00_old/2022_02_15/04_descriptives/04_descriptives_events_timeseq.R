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

df_sample_0 <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))
# df_sample_0 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_sample_0 <- df_sample_0 %>%
        filter(country!="NE-LISS")
df_sample_0$country <- recode(df_sample_0$country, "'NE-LSP'='NE'")
df_sample_0$country_name <- recode(df_sample_0$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# All events -----

# Create data 
df_event_t_p <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_t_p, event_t_p_yes, event_t_p_time) %>%
        filter(event_t_p_yes==1&!is.na(event_t_p_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        mutate(pidseq = pid*100+transseq)

df_event_p_t <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_p_t, event_p_t_yes, event_p_t_time) %>%
        filter(event_p_t_yes==1&!is.na(event_p_t_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        mutate(pidseq = pid*100+transseq)

df_event_u_t <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_u_t, event_u_t_yes, event_u_t_time) %>%
        filter(event_u_t_yes==1&!is.na(event_u_t_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        mutate(pidseq = pid*100+transseq)

df_event_u_p <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_u_p, event_u_p_yes, event_u_p_time) %>%
        filter(event_u_p_yes==1&!is.na(event_u_p_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        mutate(pidseq = pid*100+transseq)

# Summarize data

event_p_t <- with(df_event_p_t,table(country,event_p_t_time))
df_event_p_t_agg <- data.frame(event_p_t) %>%
        rename(time=event_p_t_time) %>%
        mutate(type="p_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_t_p <- with(df_event_t_p,table(country,event_t_p_time))
df_event_t_p_agg <- data.frame(event_t_p) %>%
        rename(time=event_t_p_time) %>%
        mutate(type="t_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_p <- with(df_event_u_p,table(country,event_u_p_time))
df_event_u_p_agg <- data.frame(event_u_p) %>%
        rename(time=event_u_p_time) %>%
        mutate(type="u_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_t <- with(df_event_u_t,table(country,event_u_t_time))
df_event_u_t_agg <- data.frame(event_u_t) %>%
        rename(time=event_u_t_time) %>%
        mutate(type="u_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

df_event_all <- rbind(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)
rm(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)

# Graph

df_graph <- df_event_all

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(df_graph, aes(x=time, y=Freq)) +
        facet_grid(country_name~type) +
        geom_bar(stat="identity")+
        theme_bw() + 
        xlab("Years before/after event") +
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 2) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_all_events_timeseq.pdf"), height = 9, width = 7, plot = last_plot())


# First event -----

# Create data 
df_event_t_p <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_t_p, event_t_p_yes, event_t_p_time) %>%
        filter(event_t_p_yes==1&!is.na(event_t_p_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        filter(transseq == 1) %>%
        mutate(pidseq = pid*100+transseq)

df_event_p_t <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_p_t, event_p_t_yes, event_p_t_time) %>%
        filter(event_p_t_yes==1&!is.na(event_p_t_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        filter(transseq == 1) %>%
        mutate(pidseq = pid*100+transseq)

df_event_u_t <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_u_t, event_u_t_yes, event_u_t_time) %>%
        filter(event_u_t_yes==1&!is.na(event_u_t_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        filter(transseq == 1) %>%
        mutate(pidseq = pid*100+transseq)

df_event_u_p <- df_sample_0 %>%
        select(country, pid, pidseq, transseq, year, unmp,temp, perm, event_u_p, event_u_p_yes, event_u_p_time) %>%
        filter(event_u_p_yes==1&!is.na(event_u_p_time)) %>%
        group_by(country,pid,transseq) %>%
        mutate(transseq = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        group_by(country,pid) %>%
        mutate(transseq = cumsum(transseq),
        ) %>%
        ungroup() %>%
        filter(transseq == 1) %>%
        mutate(pidseq = pid*100+transseq)

# Summarize data

event_p_t <- with(df_event_p_t,table(country,event_p_t_time))
df_event_p_t_agg <- data.frame(event_p_t) %>%
        rename(time=event_p_t_time) %>%
        mutate(type="p_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_t_p <- with(df_event_t_p,table(country,event_t_p_time))
df_event_t_p_agg <- data.frame(event_t_p) %>%
        rename(time=event_t_p_time) %>%
        mutate(type="t_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_p <- with(df_event_u_p,table(country,event_u_p_time))
df_event_u_p_agg <- data.frame(event_u_p) %>%
        rename(time=event_u_p_time) %>%
        mutate(type="u_p") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

event_u_t <- with(df_event_u_t,table(country,event_u_t_time))
df_event_u_t_agg <- data.frame(event_u_t) %>%
        rename(time=event_u_t_time) %>%
        mutate(type="u_t") %>%
        group_by(country) %>%
        mutate(baseline = ifelse(time == 0, yes = Freq, no = 0),
               baseline = max(baseline)) %>%
        ungroup() %>%
        mutate(Freq = Freq/baseline) %>%
        arrange(country,time)

df_event_first <- rbind(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)
rm(df_event_p_t_agg,df_event_t_p_agg,df_event_u_p_agg,df_event_u_t_agg)

# Graph

df_graph <- df_event_first

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(df_graph, aes(x=time, y=Freq)) +
        facet_grid(country_name~type) +
        geom_bar(stat="identity")+
        theme_bw() + 
        xlab("Years before/after event") +
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 2) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_first_event_timeseq.pdf"), height = 9, width = 7, plot = last_plot())

ggplot(df_graph, aes(x=time, y=Freq)) +
        facet_grid(type~country_name) +
        geom_bar(stat="identity")+
        theme_bw() + 
        xlab("Years before/after event") +
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 2) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title.y = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_first_event_timeseq_presentation.pdf"), height = 6, width = 9, plot = last_plot())

