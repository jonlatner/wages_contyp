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

# FOLDERS (ADAPT THIS PATHWAY!)
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")
data_files = "data_files/"

# LIBRARY
library(tidyverse)
library(zoo)
library(Hmisc)
library(data.table)
library(beepr)

options(scipen = 999) # disable scientific notation

# Load data -----

df_sample_0 <- readRDS(file = paste0(data_files, "03a_df_sample.rds"))

df_sample_1 <- df_sample_0 

# Clean wages ----

# If unemployed, then wages == 0
df_sample_1 <- df_sample_1 %>%
        mutate(
                wages = ifelse(unmp==1, yes = 0, no = wages),
                ln_wages = log(wages),
                ln_wages = ifelse(wages == 0 & unmp==1, yes = 0, no = ln_wages),
                hourly_wage = ifelse(unmp==1, yes = 0, no = hourly_wage),
                ln_hourly_wage = log(hourly_wage),
                ln_hourly_wage = ifelse(hourly_wage == 0 & unmp==1, yes = 0, no = ln_hourly_wage),
                hourly_wage_dollars = ifelse(unmp==1, yes = 0, no = hourly_wage_dollars),
                ln_hourly_wage_dollars = log(hourly_wage_dollars),
                ln_hourly_wage_dollars = ifelse(hourly_wage_dollars == 0 & unmp==1, yes = 0, no = ln_hourly_wage_dollars),
        )

# Clean employment status ----

# If unemployed, then no work contract (temp or permanent)
# recode employment status (0=unemployed; 1=temp contract; 2=perm contract; 3=temp, not not FTC)
with(df_sample_1, table(unmp,emp_status, useNA = "ifany"))
table(df_sample_1$emp_status,useNA = "ifany")
with(df_sample_1, table(country,emp_status, useNA = "ifany"))

df_sample_1 <- df_sample_1 %>%
        mutate(emp_status = ifelse(unmp == 1, yes = 0, no = emp_status),
               perm = ifelse(unmp==0 & emp_status==1, yes = 1, no = 0),
               temp = ifelse(unmp==0 & emp_status==2, yes = 1, no = 0))

with(df_sample_1, table(country,emp_status, useNA = "ifany"))


# determine minimum difference between sample periods (i.e. annual or biannual)
df_sample_1 <- df_sample_1 %>%
        group_by(country, pid) %>%
        mutate(year_lag = year - lag(year,1,default = NA)) %>%
        group_by(country) %>%
        mutate(year_lag = min(year_lag,na.rm = TRUE)) %>%
        ungroup()

# Save data sets ----

saveRDS(df_sample_1, file = paste0(data_files, "03b_df_sample_cleaned.rds"))

beep()



df_graph <- df_sample_1 %>%
  filter(unmp==0) %>%
  group_by(country, year) %>%
  summarise(value = mean(temp)) %>%
  ungroup()

ggplot(data = df_graph, aes(x = year, y = value)) +
  facet_wrap(. ~ country, nrow = 2) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0,.3)) +
  scale_color_grey(start = 0, end = 0.7) +
  xlab("Year") +
  ylab("Temporary employment rate (25-54)") +
  theme_bw() +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", size=.5),
        axis.line.x = element_line(color="black", size=.5)
  )

t <- with(df_sample_1, table(year,emp_status))
prop.table(t,1)
