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
tables = "projects/mobility/tables/"
results = "projects/mobility/results/"
graphs = "projects/mobility/graphs/"

# LIBRARY
library(tidyverse)
library(xtable)
library(dummies)
library(reshape2)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

# clean data -----

df_sample_1 <- df_sample_0 %>%
  select(-post_temp_sq,post_temp_first) %>%
  filter(country != "NE-LISS")

df_dummy <- dummy(x = df_sample_1$age_cat, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)

df_dummy <- dummy(x = df_sample_1$edu_cat, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

# make table -----

df_sample_2 <- df_sample_1 %>%
  select(country,study_period,pid,year,temp,matches("age_cat_"),matches("edu_cat_"),male) %>%
  filter(temp == 1) %>%
  group_by(country,pid,study_period,year) %>%
  slice(1) %>%
  ungroup() %>%
  select(-study_period)

df_sample_3 <- unique(df_sample_2)

df_sample_mean <- df_sample_3 %>%
  select(-pid,-temp) %>%
  group_by(country,year) %>%
  summarise_all(mean) %>%
  ungroup()

df_sample_sd <- df_sample_3 %>%
  select(-pid,-temp) %>%
  group_by(country,year) %>%
  summarize_all(sd) %>%
  ungroup()

df_sample_count <- df_sample_3 %>%
  select(-pid,-temp) %>%
  group_by(country,year) %>%
  summarize_all(length) %>%
  ungroup()


df_sample_mean_long <- melt(df_sample_mean, id.vars = c("country", "year")) %>%
  rename(mean=value)
df_sample_sd_long <- melt(df_sample_sd, id.vars = c("country", "year")) %>%
  rename(sd=value)
df_sample_count_long <- melt(df_sample_count, id.vars = c("country", "year")) %>%
  rename(n=value)


df_descriptives <- merge(df_sample_mean_long,df_sample_sd_long)
df_descriptives <- merge(df_descriptives,df_sample_count_long)

df_descriptives$group <- gsub( "_.*$", "", df_descriptives$variable)
df_descriptives$variable <- factor(df_descriptives$variable,
                            levels = c(
                              "age_cat_1", "age_cat_2", "age_cat_3",
                              "edu_cat_1", "edu_cat_2", "edu_cat_3", 
                              "male"
                            ),
                            labels = c(
                              "25-34", "35-44", "45-54",
                              "< Secondary", "Secondary", "> Secondary",
                              "Male"
                            ))


rm(df_sample_0,df_sample_1,df_sample_2,df_sample_3,df_sample_mean,df_sample_mean_long,df_sample_sd,df_sample_sd_long,df_sample_count,df_sample_count_long)

df_descriptives <- df_descriptives %>%
  mutate(se=sd/sqrt(n))

# graph -----

df_graph <- df_descriptives %>%
  filter(group == "age")

ggplot(data = df_graph, aes(x = year, y = mean, color = variable)) +
  facet_wrap(. ~ country) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("gray80", "gray50", "black")) +
  geom_errorbar(aes(ymin = mean-1.96*se,
                    ymax = mean+1.96*se
  ),
  width=.2) +
  xlab("Year") +
  ylab("Average") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", size=.5),
        axis.line.x = element_line(color="black", size=.5)
  )

ggsave(paste0(graphs,"graph_descriptives_age.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_descriptives %>%
  filter(group == "edu")

ggplot(data = df_graph, aes(x = year, y = mean, color = variable)) +
  facet_wrap(. ~ country) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("gray80", "gray50", "black")) +
  geom_errorbar(aes(ymin = mean-1.96*se,
                    ymax = mean+1.96*se
  ),
  width=.2) +
  xlab("Year") +
  ylab("Average") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", size=.5),
        axis.line.x = element_line(color="black", size=.5)
  )

ggsave(paste0(graphs,"graph_descriptives_edu.pdf"), height = 5, width = 7, plot = last_plot())

df_graph <- df_descriptives %>%
  filter(group == "male")

ggplot(data = df_graph, aes(x = year, y = mean)) +
  facet_wrap(. ~ country) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean-1.96*se,
                    ymax = mean+1.96*se
  ),
  width=.2) +
  xlab("Year") +
  ylab("Average") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", size=.5),
        axis.line.x = element_line(color="black", size=.5)
  )

ggsave(paste0(graphs,"graph_descriptives_male.pdf"), height = 5, width = 7, plot = last_plot())

