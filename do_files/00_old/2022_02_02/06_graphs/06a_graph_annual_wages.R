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

options(scipen = 999) # disable scientific notation

# function to extract last digit from string
RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# Load data -----

df_yhat_m <- readRDS(paste0(results,"df_yhat_annual_wages_log_male.rds"))
df_yhat_f <- readRDS(paste0(results,"df_yhat_annual_wages_log_female.rds"))

# Clean data -----

df_yhat_m$gender <- "Male"
df_yhat_f$gender <- "Female"

df_yhat <- rbind(df_yhat_m,df_yhat_f)

df_yhat <- df_yhat %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

rm(df_yhat_m,df_yhat_f)

# Prepare data -----

df_yhat <- df_yhat %>%
        filter(str_detect(term, 'event_time'))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        rename(fit=estimate,
               se.fit=std.error) %>%
        select(fit,se.fit,event,country,post,gender)

df_yhat$event <- factor(df_yhat$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

# Graph compare events ----

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Temp to Perm" | event == "Perm to Temp")

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_grid(gender ~ country) +
        geom_line(size = 1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_gender_annual_wages_log_post_event_temp.pdf"), height = 4, width = 8, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Unmp to Perm" | event == "Unmp to Temp")

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_grid(gender ~ country) +
        geom_line(size = 1) +
        # geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_gender_annual_wages_log_post_event_unmp.pdf"), height = 4, width = 8, plot = last_plot())
