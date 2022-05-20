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
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_original <- readRDS(paste0(results,"df_yhat_multiple_events_contyp.rds"))

df_original <- df_original %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_original$country_name <- recode(df_original$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")
table(df_original$country)

# Graph event ----

df_graph <- df_original %>%
        filter(term == "temp" | term == "event_t_p_time_pos3" | term == "event_p_t_time_pos3") %>%
        mutate(event = ifelse(term == "event_t_p_time_pos3", yes = "t_p",
                              ifelse(term == "event_p_t_time_pos3", yes = "p_t", no = model))) %>%
        select(country_name,term, estimate, std.error, event)

df_graph$event <- factor(df_graph$event,
                         levels = c("FE", "FEIS", "t_p","p_t"),
                         labels = c("FE", "FEIS", "T to P", "P to T"))

df_graph %>% mutate(ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error) %>% print(n=40)


ggplot(data=df_graph, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, scales = "fixed", nrow = 2) +
        geom_point(size=1) +
        theme_bw() +
        scale_y_continuous(limits = c(-.35,.35), breaks = seq(-.3,.3,.2)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        geom_hline(yintercept = 0) +
        xlab("Model type/event") +
        ylab("Estimate") +
        geom_text(data = df_graph,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 3,
                  vjust = ifelse(df_graph$estimate>0, yes = -.5, no = 1.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_multiple_events_contyp.pdf"), height = 4, width = 8, plot = last_plot())
ggsave(paste0(graphs,"graph_multiple_events_contyp_paper.pdf"), height = 6, width = 9, plot = last_plot())

# Graph post event ----

# Prepare data 

df_yhat <- df_original %>%
        filter(str_detect(term, '_time')) %>%
        mutate(event = ifelse(str_detect(term, 'event_p_t_time'), yes = "p_t",
                               ifelse(str_detect(term, 'event_t_p_time'), yes = "t_p", no = 0)))

table(df_yhat$term)

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post) %>%
        arrange(event,country,post)

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="p_t" | event == "t_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(. ~ country_name, nrow = 2) +
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

ggsave(paste0(graphs,"graph_multiple_events_contyp_post.pdf"), height = 4, width = 8, plot = last_plot())
ggsave(paste0(graphs,"graph_multiple_events_contyp_post_paper.pdf"), height = 6, width = 9, plot = last_plot())

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(. ~ country_name, nrow = 2, scales = "free_y") +
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

ggsave(paste0(graphs,"graph_multiple_events_contyp_post_free_scale.pdf"), height = 4, width = 8, plot = last_plot())
