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
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter_01 <- readRDS(file = paste0(data_files,"3a_df_filter_steps.rds"))
df_filter_01 <- df_filter_01 %>%
  filter(country!="NE-LISS")
table(df_filter_01$country)
df_filter_01$country <- recode(df_filter_01$country, "'NE-LSP'='NE'")

df_event_data_01 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))
df_event_data_01 <- df_event_data_01 %>%
  filter(country!="NE-LISS")
table(df_event_data_01$country)
df_event_data_01$country <- recode(df_event_data_01$country, "'NE-LSP'='NE'")

# clean data -----

# FE/FEIS - keep if employed
df_employed <- df_event_data_01 %>%
  filter(unmp == 0) %>%
  select(country,pid,year,temp) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total = n(),
            obs_diff = mean(temp),
            obs_num = sum(temp)) %>%
  mutate(step = "A") %>%
  filter(row_number()==1)

# if treated, then must be employed after treatment
# if not treated, then must be employed
# must be observable at least 3 times

# Temp to perm
df_event_t_p <- df_event_data_01 %>%
  filter((event_t_p_yes == 0 & unmp == 0) | (event_t_p_yes == 1 & event_p_t_drop_01 == 0 & event_p_t_drop_02 == 0)) %>%
  select(country,pid,year,event_t_p_yes) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total = n(),
            obs_diff = mean(event_t_p_yes),
            obs_num = sum(event_t_p_yes)) %>%
  mutate(step = "B") %>%
  filter(row_number()==1)


# Perm to temp
df_event_p_t <- df_event_data_01 %>%
  filter((event_p_t_yes == 0) | (event_p_t_yes == 1 & event_t_p_drop_01 == 0 & event_t_p_drop_02 == 0)) %>%
  select(country,pid,year,event_p_t_yes) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total = n(),
            obs_diff = mean(event_p_t_yes),
            obs_num = sum(event_p_t_yes)) %>%
  mutate(step = "C") %>%
  filter(row_number()==1)
  
# Unmp to perm
df_event_u_p <- df_event_data_01 %>%
  filter((event_u_p_yes == 0 & unmp == 0) | (event_u_p_yes == 1 & event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0)) %>%
  select(country,pid,year,event_u_p_yes) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total = n(),
            obs_diff = mean(event_u_p_yes),
            obs_num = sum(event_u_p_yes)) %>%
  mutate(step = "D") %>%
  filter(row_number()==1)
  
# Unmp to temp
df_event_u_t <- df_event_data_01 %>%
  filter((event_u_t_yes == 0 & unmp == 0) | (event_u_t_yes == 1 & event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0)) %>%
  select(country,pid,year,event_u_t_yes) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total = n(),
            obs_diff = mean(event_u_t_yes),
            obs_num = sum(event_u_t_yes)) %>%
  mutate(step = "E") %>%
  filter(row_number()==1)

df_event_data_02 <- rbind(df_employed,df_event_t_p,df_event_p_t,df_event_u_p,df_event_u_t)

df_event_data_02

df_event_data_02 <- df_event_data_02 %>%
  mutate(obs_diff = paste0(round(obs_diff*100,0),"\\%"))

# Summarize data ----

df_filter_02 <- df_filter_01 %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  select(step,total) %>%
  mutate(obs_num = NA) 
  
# Clean data ----

df_filter_02 <- df_filter_02 %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
         step = as.character(step),
  )
  

df_filter_03 <- rbind(df_filter_02,df_event_data_02) 

df_filter_04 <- df_filter_03 %>%
  mutate(notes = ifelse(step == "0", yes = "Raw data",
                        ifelse(step == "1", yes = "Panel years between 2000 and 2018",
                               ifelse(step == "2", yes = "Labour force participant",
                                      ifelse(step == "3", yes = "Prime age (25 - 54)",
                                             ifelse(step == "4", yes = "Unemployed or employed with contract type",
                                                    ifelse(step == "5", yes = "Unemployed or employed with monthly hours $>= 1$",
                                                           ifelse(step == "6", yes = "Non missing education or gender",
                                                                  ifelse(step == "7", yes = "Drop observations with hourly wages in the top/bottom 0.005 percentile",
                                                                         ifelse(step == "A", yes = "Employed and observable at least 3 times (for FE/FEIS models)",
                                                                                ifelse(step == "B", yes = "Temp $\\rightarrow$ perm",
                                                                                       ifelse(step == "C", yes = "Perm $\\rightarrow$ temp",
                                                                                              ifelse(step == "D", yes = "Unmp $\\rightarrow$ perm",
                                                                                                     ifelse(step == "E", yes = "Unmp $\\rightarrow$ temp",
                                                                                                            no = NA)))))))))))))) %>%
  select(step,notes,total,obs_diff,obs_num)

df_filter_04

# Table -----------------------------------------

df_table <- df_filter_04


# VARIABLE LABLES

panel_a_columns <- c("\\multicolumn{5}{l}{{\\bf Panel A:} Sample selection criteria} \\\\ \n
                     Step     &  Description     & Uniqe n  & $\\Delta$  & \\\\ \n
                     \\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\\\[-1.8ex] \n")
panel_b_columns <- c("\\hline \\\\[-1.8ex]  \n 
                     \\multicolumn{5}{l}{{\\bf Panel B:} Steps to create data sets by event type} \\\\ \n
                     1 & \\multicolumn{4}{l}{if treated, then must be employed after treatment} \\\\ \n
                     2 & \\multicolumn{4}{l}{if not treated, then must be employed} \\\\ \n
                     3 & \\multicolumn{4}{l}{must be observable at least 3 times} \\\\ \n")
panel_c_columns <- c("\\hline \\\\[-1.8ex]  \n 
                     \\multicolumn{4}{l}{{\\bf Panel C:} Data sets by event type} \\\\ \n
                     Data set  & Event type  & Uniqe n  & \\multicolumn{2}{l}{Treated (\\%/\\#)} \\\\ \n
                     \\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-5} \\\\[-1.8ex] \n")

hline_top <- ("\\hline\\hline \\\\[-1.8ex] \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              "l", # description
              "l", #unique obs
              "l", #diff. unique
              "l" # number treated
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,8,8,13),
        command = c(hline_top,
                    panel_a_columns,
                    panel_b_columns,
                    panel_c_columns,
                    hline_bot)),
      comment = FALSE
)

t
