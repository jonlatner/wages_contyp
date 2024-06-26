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
library(beepr)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

# Contyp
df_contyp_y <- readRDS(paste0(results,"df_yhat_multiple_events_contyp_age_cat_Y.rds"))
df_contyp_y <- df_contyp_y %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Younger (25-34)")

df_contyp_m <- readRDS(paste0(results,"df_yhat_multiple_events_contyp_age_cat_M.rds"))
df_contyp_m <- df_contyp_m %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Middle (35-44)")

df_contyp_o <- readRDS(paste0(results,"df_yhat_multiple_events_contyp_age_cat_O.rds"))
df_contyp_o <- df_contyp_o %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Older (45-54)")

# Unemployment
df_unmp_y <- readRDS(paste0(results,"df_yhat_multiple_events_unmp_age_cat_Y.rds"))
df_unmp_y <- df_unmp_y %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Younger (25-34)")

df_unmp_m <- readRDS(paste0(results,"df_yhat_multiple_events_unmp_age_cat_M.rds"))
df_unmp_m <- df_unmp_m %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Middle (35-44)")

df_unmp_o <- readRDS(paste0(results,"df_yhat_multiple_events_unmp_age_cat_O.rds"))
df_unmp_o <- df_unmp_o %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               age = "Older (45-54)")


# Append
df_append <- rbind(df_contyp_y,df_contyp_m,df_contyp_o,df_unmp_y,df_unmp_m,df_unmp_o)
# rm(df_contyp_f,df_contyp_m,df_unmp_f,df_unmp_m)

# Prepare data for graphing ----

df_yhat <- df_append %>%
        filter(str_detect(term, '_time_pos')) %>%
        mutate(event = ifelse(str_detect(term, 'event_u_t_time'), yes = "u_t",
                              ifelse(str_detect(term, 'event_u_p_time'), yes = "u_p", 
                                     ifelse(str_detect(term, 'event_t_p_time'), yes = "t_p", 
                                            ifelse(str_detect(term, 'event_p_t_time'), yes = "p_t", 
                                                   no = 0)))))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post,age)

df_yhat$event <- factor(df_yhat$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to perm",
                                    "Perm to temp",
                                    "Unmp to perm",
                                    "Unmp to temp"))

df_yhat$age <- factor(df_yhat$age,
                          levels = c("Younger (25-34)","Middle (35-44)","Older (45-54)"))

df_yhat$country_name <- recode(df_yhat$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Graph post event contyp ----

df_graph <- df_yhat %>%
        # filter(event == "Temp to perm" | event == "Perm to temp") %>%
        filter(post>=0)


p <- ggplot(data = df_graph, aes(x = post, y = estimate, color = age, group = age)) +
        facet_grid(event ~ country_name, scales = "free_y") +
        geom_line(size = 1) +
        # geom_hline(yintercept = 0) +
        # geom_vline(xintercept = 0) +
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

ggsave(plot = p, paste0(graphs,"graph_post_event_age_cat.pdf"), height = 6, width = 9)
