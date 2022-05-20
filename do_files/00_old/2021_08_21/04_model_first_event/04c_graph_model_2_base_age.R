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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
results = "projects/mobility/results/"
graphs = "projects/mobility/graphs/"

# LIBRARY
library(tidyverse)
library(ggplot2)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_yhat <- readRDS(paste0(data_files,"df_yhat_first_base_age.rds"))

# Prepare data -----

df_yhat <- df_yhat %>%
        filter(str_detect(term, 'event_time'))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        rename(fit=estimate,
               se.fit=std.error) %>%
        select(fit,se.fit,event,country,post)

event <- unique(df_yhat$event)
country_bi <- c("NE-LSP","IT")
country_ann <- c("AU","CH","DE","JP","KO","NE-LISS","UK")

for(c in country_bi) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- df_yhat %>%
                        filter(event == e,
                               country == c)
                df_base <- data.frame(fit  = c(0),
                                      se.fit = c(0),
                                      event = e,
                                      country = c,
                                      post = -2)
                df_yhat <- rbind(df_yhat,df_base)
        }
}

for(c in country_ann) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- df_yhat %>%
                        filter(event == e,
                               country == c)
                df_base <- data.frame(fit  = c(0),
                                      se.fit = c(0),
                                      event = e,
                                      country = c,
                                      post = -1)
                df_yhat <- rbind(df_yhat,df_base)
        }
}

df_yhat <- df_yhat %>%
        arrange(event,country,post)

rm(df_base,df_event,c,country_ann,country_bi,e,event)

# Graph single country ----

df_graph <- df_yhat %>%
        mutate(type = ifelse(event=="t_p" | event == "p_t", yes = "Type 1", no = "Type 2")) %>%
        filter(country == "DE")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ type, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=2,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_2_first_output_country_DE.pdf"), height = 4, width = 8, plot = last_plot())

# Graph compare events ----

df_graph <- df_yhat %>%
        filter(event=="t_p" | event == "p_t")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
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

ggsave(paste0(graphs,"graph_model_2_first_output_event_temp.pdf"), height = 4, width = 8, plot = last_plot())

df_graph <- df_yhat %>%
        filter(event=="u_t" | event == "u_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
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

ggsave(paste0(graphs,"graph_model_2_first_output_event_unmp.pdf"), height = 4, width = 8, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="u_t" | event == "u_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp",
                                    "Unmp to Perm",
                                    "Unmp to Temp"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        # geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years after event") +
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

ggsave(paste0(graphs,"graph_model_2_first_output_event_unmp_post_event.pdf"), height = 4, width = 8, plot = last_plot())

