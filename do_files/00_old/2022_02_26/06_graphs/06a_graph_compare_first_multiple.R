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
df_first_contyp <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_contyp.rds"))
df_first_contyp <- df_first_contyp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_first_unmp <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_unmp.rds"))
df_first_unmp <- df_first_unmp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_first <- rbind(df_first_contyp,df_first_unmp)
df_first$type = "First event"

# Multiple events
df_multiple_contyp <- readRDS(paste0(results,"df_yhat_multiple_hourly_wages_log_contyp.rds"))
df_multiple_contyp <- df_multiple_contyp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_multiple_unmp <- readRDS(paste0(results,"df_yhat_multiple_hourly_wages_log_unmp.rds"))
df_multiple_unmp <- df_multiple_unmp %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_multiple <- rbind(df_multiple_contyp,df_multiple_unmp)
df_multiple$type = "Multiple events"

df_append <- rbind(df_first,df_multiple)
df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

rm(df_first_contyp,df_first_unmp,df_first,df_multiple_contyp,df_multiple_unmp,df_multiple)

# Prepare data for graphing ----

df_yhat <- df_append %>%
        filter(str_detect(term, 'event_time'))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post,type)

event <- unique(df_yhat$event)
country_bi <- c("NE","IT")
country_ann <- c("AU","CH","DE","JP","KO","UK")

for(c in country_bi) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- df_yhat %>%
                        filter(event == e,
                               country == c)
                df_base <- data.frame(estimate  = c(0),
                                      std.error = c(0),
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
                df_base <- data.frame(estimate  = c(0),
                                      std.error = c(0),
                                      event = e,
                                      country = c,
                                      post = -1)
                df_yhat <- rbind(df_yhat,df_base)
        }
}

df_yhat <- df_yhat %>%
        arrange(event,country,post)

rm(df_base,df_event,c,country_ann,country_bi,e,event)

df_yhat$event <- factor(df_yhat$event,
                         levels = c("t_p","p_t","u_p","u_t"),
                         labels = c("Temp to perm",
                                    "Perm to temp",
                                    "Unmp to perm",
                                    "Unmp to temp"))

df_yhat$country_name <- recode(df_yhat$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Graph post contyp event ----

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Perm to temp" | event == "Temp to perm")

ggplot(data = df_graph, aes(x = post, y = estimate, color = type, group = type)) +
        facet_grid(event ~ country_name) +
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

ggsave(paste0(graphs,"graph_contyp_post_compare_first_multiple.pdf"), height = 4, width = 8, plot = last_plot())

# Graph post unmp event ----

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Unmp to temp" | event == "Unmp to perm")



df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x = post, y = estimate, color = type, group = type)) +
        facet_grid(event~country_name) +
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
              strip.text.y = element_blank(),
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

p2 <- ggplot(data=df_graph_2, aes(x = post, y = estimate, color = type, group = type)) +
        facet_grid(event~country_name,drop=TRUE) +
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

ggsave(plot=p4,paste0(graphs,"graph_unmp_post_compare_first_multiple.pdf"), height = 4, width = 8)
