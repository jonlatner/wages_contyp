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
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_original <- readRDS(paste0(results,"df_yhat_multiple_events_unmp.rds"))
df_original <- df_original %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_original$country_name <- recode(df_original$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")
table(df_original$country)


# Graph event ----

df_graph <- df_original %>%
        filter(term == "event_u_p_time_pos3" | term == "event_u_t_time_pos3") %>%
        mutate(event = ifelse(term == "event_u_p_time_pos3", yes = "u_p",
                              ifelse(term == "event_u_t_time_pos3", yes = "u_t", no = model))) %>%
        select(country_name,term, estimate, std.error, event)

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("U to P", "U to T"))

df_graph %>% mutate(ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error)

ggplot(data=df_graph, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2, scales = "free_y") +
        geom_point(size=1) +
        theme_bw() +
        # scale_y_continuous(limits = c(-.3,.3)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        # geom_hline(yintercept = 0) +
        xlab("Model type/event") +
        ylab("Estimate") +
        geom_text(data = df_graph,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 3,
                  vjust = ifelse(df_graph$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_multiple_events_unmp.pdf"), height = 4, width = 8, plot = last_plot())

# create better graph

df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_point(size=1) +
        theme_bw() +
        scale_y_continuous(limits = c(0,4)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        # xlab("Model type/event") +
        # ylab("Estimate") +
        geom_text(data = df_graph_1,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 3,
                  vjust = ifelse(df_graph_1$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              axis.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

df_graph_2 <- df_graph %>%
        filter(country_name == "Japan" | country_name == "Korea")

df_graph_2 <- droplevels(df_graph_2)

p2 <- ggplot(data=df_graph_2, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_point(size=1) +
        theme_bw() +
        scale_y_continuous(limits = c(6,10)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        # xlab("Model type/event") +
        # ylab("Estimate") +
        geom_text(data = df_graph_2,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 3,
                  vjust = ifelse(df_graph_2$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.title = element_blank(),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

#create common x and y labels

y.grob <- textGrob("Estimate", 
                   # gp=gpar(fontface="bold", col="blue", fontsize=15), 
                   rot=90)

x.grob <- textGrob("Event type", 
                   # gp=gpar(fontface="bold", col="blue", fontsize=15),
                   )

p3 <- plot_grid(p1, p2, rel_widths = c(3, 1))
p4 <- grid.arrange(arrangeGrob(p3, left = y.grob, bottom = x.grob))

ggsave(plot=p4,paste0(graphs,"graph_multiple_events_unmp_better.pdf"), height = 4, width = 8)
ggsave(plot=p4,paste0(graphs,"graph_multiple_events_unmp_better_paper.pdf"), height = 6, width = 9)

# Graph post event ----

# Prepare data 

df_yhat <- df_original %>%
        filter(str_detect(term, '_time')) %>%
        mutate(event = ifelse(str_detect(term, 'event_u_t_time'), yes = "u_t",
                              ifelse(str_detect(term, 'event_u_p_time'), yes = "u_p", no = 0)))

table(df_yhat$term)

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post) %>%
        arrange(event,country,post)

# Graph 

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="u_p" | event == "u_t")

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("Unmp to Perm",
                                    "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(. ~ country_name, nrow = 2, scales = "free_y") +
        geom_line(size = 1) +
        # geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        # scale_y_continuous(limits = c(-.3,.3)) +
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

ggsave(paste0(graphs,"graph_multiple_events_unmp_post_free_scale.pdf"), height = 4, width = 8, plot = last_plot())

# create better graph

df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(.~country_name,  nrow = 2) +
        geom_line(size = 1) +
        theme_bw() +
        scale_y_continuous(limits = c(0,4)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years before/after event") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_text(hjust = .75),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


df_graph_2 <- df_graph %>%
        filter(country_name == "Japan" | country_name == "Korea")

df_graph_2 <- droplevels(df_graph_2)

p2 <- ggplot(data=df_graph_2, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_line(size = 1) +
        theme_bw() +
        scale_y_continuous(limits = c(6,10)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab(" ") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

#create common x and y labels

y.grob <- textGrob("Estimate", rot=90)

p3 <- ggarrange(
        p1, p2, nrow = 1, widths = c(3, 1), 
        common.legend = TRUE, legend = "bottom"
)

p4 <- grid.arrange(arrangeGrob(p3, left = y.grob))
p4

ggsave(plot=p4,paste0(graphs,"graph_multiple_events_unmp_post_better.pdf"), height = 4, width = 8)
ggsave(plot=p4,paste0(graphs,"graph_multiple_events_unmp_post_better_paper.pdf"), height = 6, width = 9)

