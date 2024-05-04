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

# FOLDERS (ADAPT THIS PATHWAY!)
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

data_files = "data_files/"
results = "results/"
tables = "tables/"
graphs = "graphs/descriptives/"

# LIBRARY
library(tidyverse)
library(car)
library(beepr)

options(scipen = 999) # disable scientific notation

# Load data -----

df_original <- readRDS(paste0(data_files,"03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

df_original <- df_original %>%
        # filter(country == "AU") %>%
        filter(country!="NE-LISS")
df_original$country <- recode(df_original$country, "'NE-LSP'='NE'")

# clean data -----

# Temp to perm
df_event_t_p_01 <- df_original %>%
        filter(!is.na(event_t_p_yes)) %>%
        select(country,pid,pidseq,transseq,year,unmp,temp,perm,matches("event_t_p")) %>%
        filter(unmp == 0) %>%
        group_by(country, pidseq) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup() %>%
        select(-max, -number)

# Perm to temp
df_event_p_t_01 <- df_original %>%
        filter(!is.na(event_p_t_yes)) %>%
        select(country,pid,pidseq,transseq,year,unmp,temp,perm,matches("event_p_t")) %>%
        filter(unmp == 0) %>%
        group_by(country, pidseq) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup() %>%
        select(-max, -number)

# Unmp to perm
df_event_u_p_01 <- df_original %>%
        filter(!is.na(event_u_p_yes)) %>%
        select(country,pid,pidseq,transseq,year,unmp,temp,perm,matches("event_u_p"))

# Unmp to temp
df_event_u_t_01 <- df_original %>%
        filter(!is.na(event_u_t_yes)) %>%
        select(country,pid,pidseq,transseq,year,unmp,temp,perm,matches("event_u_t"))

# Summarize data -----

# Temp to perm
df_event_t_p_02 <- df_event_t_p_01 %>%
        group_by(country,pidseq) %>%
        mutate(count = ifelse(event_t_p_yes_final == 1, yes = 1, no = 0),
               count = max(count)) %>%
        slice(1) %>%
        filter(count>0) %>%
        group_by(country,pid) %>%
        mutate(count = sum(count),
               count = ifelse(count>2, yes = 3, no = count)) %>%
        slice(1) %>%
        ungroup()

df_event_t_p_02 <- with(df_event_t_p_02,table(country,count))
df_event_t_p_02 <- data.frame(df_event_t_p_02,1)
df_event_t_p_02 <- df_event_t_p_02 %>%
        mutate(type="t_p")

# Unmp to perm
df_event_u_p_02 <- df_event_u_p_01 %>%
        group_by(country,pidseq) %>%
        mutate(count = ifelse(event_u_p_yes_final == 1, yes = 1, no = 0),
               count = max(count)) %>%
        slice(1) %>%
        filter(count>0) %>%
        group_by(country,pid) %>%
        mutate(count = sum(count),
               count = ifelse(count>2, yes = 3, no = count)) %>%
        slice(1) %>%
        ungroup()


df_event_u_p_02 <- with(df_event_u_p_02,table(country,count))
df_event_u_p_02 <- data.frame(df_event_u_p_02,1)
df_event_u_p_02 <- df_event_u_p_02 %>%
        mutate(type="u_p")


# Perm to temp
df_event_p_t_02 <- df_event_p_t_01 %>%
        group_by(country,pidseq) %>%
        mutate(count = ifelse(event_p_t_yes_final == 1, yes = 1, no = 0),
               count = max(count)) %>%
        slice(1) %>%
        filter(count>0) %>%
        group_by(country,pid) %>%
        mutate(count = sum(count),
               count = ifelse(count>2, yes = 3, no = count)) %>%
        # slice(1) %>%
        ungroup()

df_event_p_t_02 <- with(df_event_p_t_02,table(country,count))
df_event_p_t_02 <- data.frame(df_event_p_t_02,1)
df_event_p_t_02 <- df_event_p_t_02 %>%
        mutate(type="p_t")

# Unmp to temp
df_event_u_t_02 <- df_event_u_t_01 %>%
        group_by(country,pidseq) %>%
        mutate(count = ifelse(event_u_t_yes_final == 1, yes = 1, no = 0),
               count = max(count)) %>%
        slice(1) %>%
        filter(count>0) %>%
        group_by(country,pid) %>%
        mutate(count = sum(count),
               count = ifelse(count>2, yes = 3, no = count)) %>%
        slice(1) %>%
        ungroup()

df_event_u_t_02 <- with(df_event_u_t_02,table(country,count))
df_event_u_t_02 <- data.frame(df_event_u_t_02,1)
df_event_u_t_02 <- df_event_u_t_02 %>%
        mutate(type="u_t")

df_event_1 <- rbind(df_event_p_t_02,df_event_u_t_02,df_event_t_p_02,df_event_u_p_02)

# rm(df_event_p_t_02,df_event_u_t_02,df_event_t_p_02,df_event_u_p_02)
# rm(df_event_p_t_01,df_event_u_t_01,df_event_t_p_01,df_event_u_p_01)

# Graph data (number of events, conditional on at least 1) -----

df_graph <- df_event_1

df_graph$type <- factor(df_graph$type,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to Perm",
                                   "Perm to Temp",
                                   "Unmp to Perm",
                                   "Unmp to Temp"))

df_graph$count <- factor(df_graph$count,
                        levels = c("1","2","3"),
                        labels = c("1","2",">=3"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# ggplot(df_graph, aes(x=event, y=Freq)) +
#         facet_grid(country_name~type) +
#         geom_bar(stat="identity")+
#         theme_bw() + 
#         scale_y_continuous(limits = c(0,1.1), breaks = c(0,.25,.5,.75,1)) +
#         geom_text(aes(label=sprintf(Freq, fmt = '%#.3f')), vjust = -0.2, size = 3) +
#         theme(panel.grid.minor = element_blank(), 
#               legend.position = "bottom",
#               legend.title = element_blank(),
#               legend.key.width=unit(2, "cm"),
#               axis.title = element_blank(),
#               axis.line.y = element_line(color="black", size=.5),
#               axis.line.x = element_line(color="black", size=.5)
#         )
# 
# ggsave(paste0(graphs,"graph_country_events_number.pdf"), height = 9, width = 7, plot = last_plot())


ggplot(df_graph, aes(x=count, y=Freq)) +
        facet_grid(type~country_name) +
        geom_bar(stat="identity")+
        theme_bw() + 
        scale_y_continuous(labels = function(x) format(x, big.mark = "."), limits = c(0,3000)) +
        geom_text(aes(label=format(Freq, big.mark=".", decimal.mark=",")), vjust = -0.2, size = 3) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_descriptives_multiple_events_num.pdf"), height = 6, width = 9, plot = last_plot())

beep()
