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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wages_contyp/")

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

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

# First event
df_first_contyp <- readRDS(paste0(results,"df_yhat_first_event_contyp.rds"))
df_first_contyp <- df_first_contyp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_first_unmp <- readRDS(paste0(results,"df_yhat_first_event_unmp.rds"))
df_first_unmp <- df_first_unmp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_first <- rbind(df_first_contyp,df_first_unmp) %>%
        mutate(model="FE + IF") %>%
        select(-event)
df_first$type = "First event"

# Multiple events
df_multiple_contyp <- readRDS(paste0(results,"df_yhat_multiple_events_contyp.rds"))
df_multiple_contyp <- df_multiple_contyp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_multiple_unmp <- readRDS(paste0(results,"df_yhat_multiple_events_unmp.rds"))
df_multiple_unmp <- df_multiple_unmp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_multiple <- rbind(df_multiple_contyp,df_multiple_unmp) %>%
        filter(model == "FE + IF")
df_multiple$type = "Multiple events"

df_append <- rbind(df_first,df_multiple)

df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

rm(df_first_contyp,df_first_unmp,df_first,df_multiple_contyp,df_multiple_unmp,df_multiple)

# Prepare data for graphing ----

df_yhat <- df_append %>%
        filter(str_detect(term, '_time')) %>%
        mutate(event = ifelse(str_detect(term, 'event_p_t_time'), yes = "p_t",
                              ifelse(str_detect(term, 'event_t_p_time'), yes = "t_p", 
                                     ifelse(str_detect(term, 'event_u_p_time'), yes = "u_p", 
                                            ifelse(str_detect(term, 'event_u_t_time'), yes = "u_t", 
                                                   no = 0)))))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(model,estimate,std.error,event,country,post,type,country_name)

df_yhat$event <- factor(df_yhat$event,
                        levels = c("t_p","p_t","u_p","u_t"),
                        labels = c("Temp to perm",
                                   "Perm to temp",
                                   "Unmp to perm",
                                   "Unmp to temp"))

# Graph post contyp event ----

df_graph <- df_yhat %>%
        filter(post>=0)

ggplot(data = df_graph, aes(x = post, y = estimate, color = type, group = type)) +
        facet_grid(event~country_name, scales = "free_y") +
        geom_line(size = 1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        # scale_y_continuous(limits = c(-.35,.35), breaks = seq(-.3,.3,.2)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Estimate") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_sensitivity_single_multiple_events.pdf"), height = 4, width = 8, plot = last_plot())
ggsave(paste0(graphs,"graph_sensitivity_single_multiple_events_paper.pdf"), height = 6, width = 9, plot = last_plot())
