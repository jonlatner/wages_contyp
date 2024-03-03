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

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_age_1 <- readRDS(paste0(results,"df_yhat_multiple_events_contyp.rds"))
df_age_1 <- df_age_1 %>%
  filter(country!="NE-LISS") %>%
  mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))
df_age_1$type = "Main sample (n>=3)"

df_age_2 <- readRDS(paste0(results,"sample_2_years/df_yhat_multiple_events_contyp.rds"))
df_age_2 <- df_age_2 %>%
  filter(country!="NE-LISS") %>%
  mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))
df_age_2$type = "Sample (n>=2)"

df_append <- rbind(df_age_1,df_age_2)
df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Graph event ----

df_ch <- df_append %>%
  filter(country_name=="Switzerland")
table(df_ch$model)

df_graph <- df_append %>%
  filter(model!="FEIS") %>%
        filter(term == "temp" | term == "event_t_p_time_pos3" | term == "event_p_t_time_pos3") %>%
        mutate(event = ifelse(term == "event_t_p_time_pos3", yes = "t_p",
                              ifelse(term == "event_p_t_time_pos3", yes = "p_t", no = model))) %>%
        select(country_name,term, estimate, std.error, event, type)

df_graph$event <- factor(df_graph$event,
                         levels = c("FE", "FEIS", "t_p","p_t"),
                         labels = c("FE", "FEIS", "T to P", "P to T"))

df_ch <- df_graph %>%
  filter(country_name=="Switzerland")
df_ch


p <- ggplot(data=df_graph, aes(x=event, y=estimate, color = type)) +
        facet_wrap(~country_name,drop=TRUE, scales = "free", nrow = 2) +
        geom_point(size=1, position=position_dodge(0.9)) +
        theme_bw() +
        # scale_y_continuous(limits = c(-.5,.5), breaks = seq(-.5,.5,.1)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2, position=position_dodge(0.9)) +
        geom_hline(yintercept = 0) +
        xlab("Model/event type") +
        ylab("Pr(Beta)") +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

p

ggsave(plot=p,paste0(graphs,"graph_sensitivity_compare_contyp_sample_2_years.pdf"), height = 6, width = 9)
