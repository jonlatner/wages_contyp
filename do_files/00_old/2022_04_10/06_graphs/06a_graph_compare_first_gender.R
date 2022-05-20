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
library(beepr)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

# Contyp
df_contyp_f <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_contyp_gender_f.rds"))
df_contyp_f <- df_contyp_f %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Female")

df_contyp_m <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_contyp_gender_m.rds"))
df_contyp_m <- df_contyp_m %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Male")


df_contyp_all <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_contyp.rds"))
df_contyp_all <- df_contyp_all %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Both")

# Unemployment
df_unmp_f <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_unmp_gender_f.rds"))
df_unmp_f <- df_unmp_f %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Female")

df_unmp_m <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_unmp_gender_m.rds"))
df_unmp_m <- df_unmp_m %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Male")

df_unmp_all <- readRDS(paste0(results,"df_yhat_first_hourly_wages_log_unmp.rds"))
df_unmp_all <- df_unmp_all %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country),
               gender = "Both")


# Append
df_append <- rbind(df_contyp_f,df_contyp_m,df_contyp_all,df_unmp_f,df_unmp_m,df_unmp_all)
# rm(df_contyp_f,df_contyp_m,df_unmp_f,df_unmp_m)

# Prepare data for graphing ----

df_yhat <- df_append %>%
        filter(str_detect(term, 'event_time'))

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post,gender)

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
        # filter(country=="AU") %>%
        filter(event=="Perm to temp" | event == "Temp to perm")

arrange(df_graph,country,event,post,gender)

ggplot(data = df_graph, aes(x = post, y = estimate, color = gender, group = gender)) +
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

ggsave(paste0(graphs,"graph_contyp_post_compare_first_gender.pdf"), height = 4, width = 8, plot = last_plot())

# Graph post unmp event ----


df_graph_1 <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Unmp to temp" | event == "Unmp to perm") %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x = post, y = estimate, color = gender, group = gender)) +
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

df_graph_2 <- df_yhat %>%
        filter(post>=0) %>%
        filter(event=="Unmp to temp" | event == "Unmp to perm") %>%
        filter(country_name == "Japan" | country_name == "Korea")

df_graph_2 <- droplevels(df_graph_2)

p2 <- ggplot(data=df_graph_2, aes(x = post, y = estimate, color = gender, group = gender)) +
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

ggsave(plot=p4,paste0(graphs,"graph_unmp_post_compare_first_gender.pdf"), height = 4, width = 8)

beep()
