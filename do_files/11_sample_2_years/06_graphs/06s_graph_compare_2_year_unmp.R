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
graphs = "graphs/sample_2_years/"

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

df_age_1 <- readRDS(paste0(results,"df_yhat_multiple_events_unmp.rds"))
df_age_1 <- df_age_1 %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))
df_age_1$type = "Main sample (n>=3)"

df_age_2 <- readRDS(paste0(results,"sample_2_years/df_yhat_multiple_events_unmp.rds"))
df_age_2 <- df_age_2 %>%
  filter(country!="NE-LISS") %>%
  mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))
df_age_2$type = "Sample (n>=2)"

df_append <- rbind(df_age_1,df_age_2)
df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")


# Graph event ----

df_graph <- df_append %>%
        filter(term == "event_u_p_time_pos3" | term == "event_u_t_time_pos3") %>%
        mutate(event = ifelse(term == "event_u_p_time_pos3", yes = "u_p",
                              ifelse(term == "event_u_t_time_pos3", yes = "u_t", no = model))) %>%
        select(country_name,term, estimate, std.error, event, type)

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("U to P", "U to T"))

df_graph %>% mutate(ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error)

p <- ggplot(data=df_graph, aes(x=event, y=estimate, color = type)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_point(size=1, position = position_dodge(0.9)) +
        theme_bw() +
        # scale_y_continuous(limits = c(0,4)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2, 
        position = position_dodge(0.9)) +
        geom_text(data = df_graph,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 4, position = position_dodge(0.9),
                  vjust = ifelse(df_graph$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              axis.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

p

ggsave(plot=p,paste0(graphs,"graph_sensitivity_compare_unmp_sample_2_years.pdf"), height = 6, width = 9)
