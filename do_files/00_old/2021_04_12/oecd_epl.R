# TOP COMMANDS -----------------------------------------

rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

data = "SECCOPA/OECD/data_files/"
graphs = "SECCOPA/projects/mobility/graphs/"

# LIBRARY
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

options(scipen = 999) # disable scientific notation

# LOAD DATA -----------------------------------------

df_epl_temp <- read.csv(paste0(data,"EPL_T.csv")) %>%
        filter(Series == "Version 1 (1985-2019)") %>%
        select(Country,Time,Value) %>%
        rename(epl_temp = Value)
df_epl_perm <- read.csv(paste0(data,"EPL_R.csv")) %>%
        filter(Series == "Version 1 (1985-2019)") %>%
        select(Country,Time,Value) %>%
        rename(epl_perm = Value)

df_epl <- merge(df_epl_perm,df_epl_temp) %>%
        rename(country=Country) %>%
        rename(year=Time)

country <- c("Australia", "Switzerland", "Germany", 
             "Italy", "Netherlands", 
             "Japan", "Korea", "United Kingdom")
country_code <- c("AU", "CH", "DE", 
                  "IT", "NE", 
                  "JP", "KO", "UK")

region <- cbind(country, country_code)

df_epl <- merge(df_epl,region) %>%
        # mutate(gap = epl_perm - epl_temp) %>%
        select(-country_code)
rm(country, country_code,df_epl_perm,df_epl_temp, region)

df_epl_long <- melt(df_epl,id.vars = c("country","year"))

# Graph EPL -----------------------------------------

df_graph <- df_epl_long %>%
        filter(year>=1990)
ggplot(data = df_graph, aes(x = year, y = value, group = variable, color = variable)) +
        geom_line(size = 1) +
        facet_wrap(.~country)+
        ylab("Employment protection legislation") +
        scale_color_manual(values = c("black",
                                         "gray"),
                              labels = c("Permanent",
                                         "Temporary")) +
        scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5)) +
        scale_x_continuous(breaks = seq(1990, 2020, by = 5), limits = c(1990, 2020)) +
        geom_vline(xintercept=2000,size=.5,color="black") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              axis.title.x = element_blank(),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(paste0(graphs,"graph_epl.pdf"), height = 5, width = 7, plot = last_plot())

