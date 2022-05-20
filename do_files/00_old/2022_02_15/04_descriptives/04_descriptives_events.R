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

df_sample_0 <- readRDS(paste0(data_files, "03b_df_sample_cleaned.rds"))

df_sample_0 <- df_sample_0 %>%
        filter(country!="NE-LISS")
df_sample_0$country <- recode(df_sample_0$country, "'NE-LSP'='NE'")
df_sample_0$country_name <- recode(df_sample_0$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_sample_1 <- df_sample_0 %>%
        select(country, pid, year, year_lag, unmp, perm, temp)

# Transition indicator ----

df_sample_2 <- df_sample_1 %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_p = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_t = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

# Summarize data -----

df_events_0 <- df_sample_2 %>%
        group_by(country,pid) %>%
        summarise(event_t_p = sum(event_t_p, na.rm = TRUE),
                  event_p_t = sum(event_p_t, na.rm = TRUE),
                  event_u_p = sum(event_u_p, na.rm = TRUE),
                  event_u_t = sum(event_u_t, na.rm = TRUE),
        ) %>%
        ungroup() %>%
        mutate(event_t_p = ifelse(event_t_p>=3,yes=3,no=event_t_p),
               event_p_t = ifelse(event_p_t>=3,yes=3,no=event_p_t),
               event_u_p = ifelse(event_u_p>=3,yes=3,no=event_u_p),
               event_u_t = ifelse(event_u_t>=3,yes=3,no=event_u_t),
        )

# Graph data (events are yes or no) -----

events_p_t <- with(df_events_0,table(country,event_p_t))
df_events_p_t <- data.frame(prop.table(events_p_t,1))
df_events_p_t <- df_events_p_t %>%
        rename(event=event_p_t) %>%
        mutate(type="p_t")

events_t_p <- with(df_events_0,table(country,event_t_p))
df_events_t_p <- data.frame(prop.table(events_t_p,1))
df_events_t_p <- df_events_t_p %>%
        rename(event=event_t_p) %>%
        mutate(type="t_p")

events_u_p <- with(df_events_0,table(country,event_u_p))
df_events_u_p <- data.frame(prop.table(events_u_p,1))
df_events_u_p <- df_events_u_p %>%
        rename(event=event_u_p) %>%
        mutate(type="u_p")

events_u_t <- with(df_events_0,table(country,event_u_t))
df_events_u_t <- data.frame(prop.table(events_u_t,1))
df_events_u_t <- df_events_u_t %>%
        rename(event=event_u_t) %>%
        mutate(type="u_t")

df_events_1 <- rbind(df_events_p_t,df_events_t_p,df_events_u_p,df_events_u_t)
rm(df_events_p_t,df_events_t_p,df_events_u_p,df_events_u_t)

# Graph data (risk of events) -----

df_graph <- df_events_1 %>%
        mutate(number = ifelse(event=="0", yes = "0", no = ">=1")) %>%
        group_by(country,type,number) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup()

df_graph$number <- fct_relevel(df_graph$number, "0", after = 0) # forcats

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(df_graph, aes(x=number, y=Freq)) +
        facet_grid(country_name~type) +
        geom_bar(stat="identity")+
        theme_bw() + 
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.3f')), vjust = -0.2, size = 3) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_events_yes_no.pdf"), height = 9, width = 7, plot = last_plot())


ggplot(df_graph, aes(x=number, y=Freq)) +
        facet_grid(type~country_name) +
        geom_bar(stat="identity")+
        theme_bw() + 
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 3) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_events_yes_no_presentation.pdf"), height = 6, width = 9, plot = last_plot())

# Graph data (number of events, conditional on at least 1) -----

events_p_t <- with(subset(df_events_0,event_p_t>0),table(country,event_p_t))
df_events_p_t <- data.frame(prop.table(events_p_t,1))
df_events_p_t <- df_events_p_t %>%
        rename(event=event_p_t) %>%
        mutate(type="p_t")

events_t_p <- with(subset(df_events_0,event_t_p>0),table(country,event_t_p))
df_events_t_p <- data.frame(prop.table(events_t_p,1))
df_events_t_p <- df_events_t_p %>%
        rename(event=event_t_p) %>%
        mutate(type="t_p")

events_u_p <- with(subset(df_events_0,event_u_p>0),table(country,event_u_p))
df_events_u_p <- data.frame(prop.table(events_u_p,1))
df_events_u_p <- df_events_u_p %>%
        rename(event=event_u_p) %>%
        mutate(type="u_p")

events_u_t <- with(subset(df_events_0,event_u_t>0),table(country,event_u_t))
df_events_u_t <- data.frame(prop.table(events_u_t,1))
df_events_u_t <- df_events_u_t %>%
        rename(event=event_u_t) %>%
        mutate(type="u_t")

df_events_1 <- rbind(df_events_p_t,df_events_t_p,df_events_u_p,df_events_u_t)
rm(df_events_p_t,df_events_t_p,df_events_u_p,df_events_u_t)

df_graph <- df_events_1

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$event <- factor(df_graph$event,
                        levels = c("1","2","3"),
                        labels = c("1","2",">=3"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(df_graph, aes(x=event, y=Freq)) +
        facet_grid(country_name~type) +
        geom_bar(stat="identity")+
        theme_bw() + 
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.3f')), vjust = -0.2, size = 3) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_events_number.pdf"), height = 9, width = 7, plot = last_plot())


ggplot(df_graph, aes(x=event, y=Freq)) +
        facet_grid(type~country_name) +
        geom_bar(stat="identity")+
        theme_bw() + 
        scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
        geom_text(aes(label=sprintf(Freq, fmt = '%#.2f')), vjust = -0.2, size = 3) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_country_events_number_presentation.pdf"), height = 6, width = 9, plot = last_plot())
