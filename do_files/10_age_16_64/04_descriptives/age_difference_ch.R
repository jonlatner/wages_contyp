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

data_files = "data_files/CH/"
support_files = "support_files/"
graphs = "graphs/age_16_64/"

# LIBRARY
library(tidyverse)
library(car)

options(scipen = 999) # disable scientific notation

# Load data -----

df_ch <- readRDS(paste0(data_files,"covars.rds"))

table(df_ch$pw36,useNA = "ifany")

df_ch <- df_ch %>%
  select(pw36,pw37,age) %>%
  filter(pw36 > 0) %>% # no missing info on job duration
  filter(pw37 > 0) %>% # filter for temporary contract (i.e. no permanent)
  rename(temp = pw36,
         reason = pw37)

# Clean/recode variables 

df_ch$reason <- recode(df_ch$reason, 
                   "1='Apprenticeship';
                    2='Training';
                    3='Interim, temporary work';
                    4='Project';
                    5='Occasional work (e.g. holiday job)';
                    6='Seasonal work';
                    7='An occupational program';
                    8='A trial period';
                    9='A position regularlarly renewed';
                    10='Other'")

with(df_ch,table(reason,temp,useNA = "ifany"))

# Create samples ----

df_ch_main <- df_ch %>%
  filter(age>=25 & age<=54) %>%
  count(reason) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  mutate(sample = "25-54")

df_ch_sens_1 <- df_ch %>%
  filter(age>=16 & age<=64) %>%
  count(reason) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  mutate(sample = "16-64")

df_ch_sens_2 <- df_ch %>%
  filter(age>=16 & age<=54) %>%
  count(reason) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  mutate(sample = "16-54")

df_ch_sens_3 <- df_ch %>%
  filter(age>=25 & age<=64) %>%
  count(reason) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  mutate(sample = "25-64")

df_sensitivity <- rbind(df_ch_main,df_ch_sens_1,df_ch_sens_2,df_ch_sens_3)

# Graph ----

table(df_sensitivity$sample)

df_graph <- ggplot(df_sensitivity, aes(x = pct, y = reason, fill = sample)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 75, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_ch_compare_sample_age.pdf"), height = 6, width = 9)

