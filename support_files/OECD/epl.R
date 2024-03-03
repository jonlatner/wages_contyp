# Top commands ----

rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wages_contyp/")
data_files = "support_files/OECD/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(readxl)
library(janitor)

options(scipen = 999) # disable scientific notation

# Load data ----

# https://www.oecd.org/employment/emp/oecdindicatorsofemploymentprotection.htm
# url <- "https://www.oecd.org/els/emp/OECDEmploymentProtectionLegislationDatabase.xlsx"

# EPL (temporary) ----

df_temp <- read_xlsx(paste0(data_files,"OECDEmploymentProtectionLegislationDatabase.xlsx"), sheet = "EPL temporary workers V1-V3 ", skip = 5)
df_temp <- clean_names(df_temp)

df_temp <- df_temp %>%
        filter(country == "Australia" 
               | country == "Switzerland"
               | country == "Germany"
               | country == "Netherlands"
               | country == "Japan"
               | country == "Korea"
               | country == "Italy"
               | country == "United Kingdom"
        ) %>%
        select(country, year, version_1, version_3) %>%
  mutate(type = "Temporary")

# EPL (permanent) ----

df_perm <- read_xlsx(paste0(data_files,"OECDEmploymentProtectionLegislationDatabase.xlsx"), sheet = "EPL dismissal regular workers", skip = 5)
df_perm <- clean_names(df_perm)

df_perm <- df_perm %>%
  filter(country == "Australia" 
         | country == "Switzerland"
         | country == "Germany"
         | country == "Netherlands"
         | country == "Japan"
         | country == "Korea"
         | country == "Italy"
         | country == "United Kingdom"
  ) %>%
  select(country, year, version_1, version_3_4) %>%
  mutate(type = "Permanent") %>%
  rename(version_3=version_3_4)

# Combine ----

df_oecd <- rbind(df_perm,df_temp) %>%
  pivot_longer(!c(country,year,type)) %>%
  filter(name == "version_1") %>%
  filter(year>=2000 & year < 2019)

df_oecd %>% filter(country == "Germany") %>% print(n=60)

# Graph ----

ggplot() +
        facet_wrap(~country, nrow = 2) +
        geom_line(aes(x = year, y = value, color = type), data = df_oecd, size = 1) + 
        theme_bw() +
  scale_color_grey(start = 0, end = 0.7) +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(limits = c(2000, 2020)) +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              axis.text.x = element_text(size = 8),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_epl.pdf"), plot = last_plot(), height = 6, width = 9)
