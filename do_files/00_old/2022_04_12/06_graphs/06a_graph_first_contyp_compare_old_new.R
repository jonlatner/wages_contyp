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
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_new <- readRDS(paste0(results,"df_yhat_first_contyp.rds"))
df_new <- df_new %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_new$country_name <- recode(df_new$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_old <- readRDS(paste0(results,"df_yhat_first_contyp_original.rds"))
df_old <- df_old %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_old$country_name <- recode(df_old$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Graph event ----

df_yhat_new <- df_new %>%
        filter(str_detect(term, '_time')) %>%
        mutate(type = "allison") %>%
        mutate(event = ifelse(str_detect(term, 'event_p_t_time'), yes = "p_t",
                               ifelse(str_detect(term, 'event_t_p_time'), yes = "t_p", no = 0)))

df_yhat_old <- df_old %>%
        filter(str_detect(term, '_time')) %>%
        mutate(type = "original") %>%
        mutate(event = ifelse(str_detect(term, 'event_p_t_time'), yes = "p_t",
                              ifelse(str_detect(term, 'event_t_p_time'), yes = "t_p", no = 0)))

df_yhat <- rbind (df_yhat_new,df_yhat_old)

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post,type) %>%
        arrange(event,country,post)

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="p_t" | event == "t_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        facet_grid(type ~ country_name) +
        geom_line(size = 1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        scale_y_continuous(limits = c(-.35,.35), breaks = seq(-.3,.3,.2)) +
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

ggsave(paste0(graphs,"graph_output_hourly_log_contyp_post_compare.pdf"), height = 4, width = 8, plot = last_plot())

