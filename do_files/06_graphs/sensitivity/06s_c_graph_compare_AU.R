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

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

# Contyp
df_contyp_1 <- readRDS(paste0(results,"df_yhat_multiple_events_contyp.rds"))
df_contyp_1 <- df_contyp_1 %>%
        filter(country=="AU") %>%
        mutate(type = "All temporary")

df_contyp_2 <- readRDS(paste0(results,"df_yhat_multiple_events_contyp_AU.rds"))
df_contyp_2 <- df_contyp_2 %>%
        filter(country=="AU") %>%
        rename(model = event) %>%
        mutate(type = "FTC only")

# Unemployment
df_unmp_1 <- readRDS(paste0(results,"df_yhat_multiple_events_unmp.rds"))
df_unmp_1 <- df_unmp_1 %>%
        filter(country=="AU") %>%
        mutate(type = "All temporary")

df_unmp_2 <- readRDS(paste0(results,"df_yhat_multiple_events_unmp_AU.rds"))
df_unmp_2 <- df_unmp_2 %>%
        filter(country=="AU") %>%
        rename(model = event) %>%
        mutate(type = "FTC only")

# Append
df_append <- rbind(df_contyp_1,df_contyp_2,df_unmp_1,df_unmp_2)
rm(df_contyp_1,df_contyp_2,df_unmp_1,df_unmp_2)

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
        select(estimate,std.error,event,country,post,type)

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
        facet_wrap(~event, scales = "free_y") +
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

ggsave(paste0(graphs,"graph_sensitivity_AU_paper.pdf"), height = 6, width = 9, plot = last_plot())
