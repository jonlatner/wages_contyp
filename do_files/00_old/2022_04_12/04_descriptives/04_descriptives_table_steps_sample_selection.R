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
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(car)
library(beepr)

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

step_8 <- df_event_data_01 %>%
  select(country,pid) %>%
  group_by(country,pid) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(count = sum(n())) %>%
  ungroup()
step_8$step <- 8

step_9 <- df_event_data_01 %>%
  filter(unmp==0) %>%
  select(country,pid) %>%
  group_by(country,pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  group_by(country) %>%
  summarise(count = sum(n())) %>%
  ungroup()
step_9$step <- 9

df_filter_02 <- rbind(df_filter_01,step_8,step_9)

# clean data -----

# if treated, then must be employed after treatment
# if not treated, then must be employed
# must be observable at least 3 times

# Temp to perm
df_event_t_p <- df_event_data_01 %>%
  filter(unmp==0) %>%
  select(country,pid,year,event_t_p_yes_final) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(obs_diff = mean(event_t_p_yes_final),
            total = sum(event_t_p_yes_final)) %>%
  mutate(step = "A") %>%
  filter(row_number()==1)


# Perm to temp
df_event_p_t <- df_event_data_01 %>%
  filter(unmp==0) %>%
  select(country,pid,year,event_p_t_yes_final) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(obs_diff = mean(event_p_t_yes_final),
            total = sum(event_p_t_yes_final)) %>%
  mutate(step = "B") %>%
  filter(row_number()==1)
  
# Unmp to perm
df_event_u_p <- df_event_data_01 %>%
  select(country,pid,year,event_u_p_yes_final) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(obs_diff = mean(event_u_p_yes_final),
            total = sum(event_u_p_yes_final)) %>%
  mutate(step = "C") %>%
  filter(row_number()==1)
  
# Unmp to temp
df_event_u_t <- df_event_data_01 %>%
  select(country,pid,year,event_u_t_yes_final) %>%
  group_by(country, pid) %>%
  mutate(number = row_number(),
         max = max(number)) %>%
  filter(max>2) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(obs_diff = mean(event_u_t_yes_final),
            total = sum(event_u_t_yes_final)) %>%
  mutate(step = "D") %>%
  filter(row_number()==1)

df_event_data_02 <- rbind(df_event_u_p,df_event_u_t,df_event_t_p,df_event_p_t)

df_event_data_02

df_event_data_02 <- df_event_data_02 %>%
  mutate(obs_diff = paste0(round(obs_diff*100,0),"\\%"))

# Summarize data ----

df_filter_03 <- df_filter_02 %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  select(step,total)

df_filter_03
  
# Clean data ----

df_filter_04 <- df_filter_03 %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
         step = as.character(step),
  )
  

df_filter_05 <- rbind(df_filter_04,df_event_data_02) 

df_filter_06 <- df_filter_05 %>%
  mutate(order = row_number()) %>%
  mutate(notes = ifelse(step == "0", yes = "Raw data",
                        ifelse(step == "1", yes = "Panel years between 2000 and 2018",
                               ifelse(step == "2", yes = "Labour force participant",
                                      ifelse(step == "3", yes = "Prime age (25 - 54)",
                                             ifelse(step == "4", yes = "Unemployed or employed with contract type",
                                                    ifelse(step == "5", yes = "Unemployed or employed with monthly hours $>= 1$",
                                                           ifelse(step == "6", yes = "Non missing education or gender",
                                                                  ifelse(step == "7", yes = "Drop observations with hourly wages in the top/bottom 0.005 percentile",
                                                                         ifelse(step == 8, yes = "Data set A: Must be observable at least 3 times",
                                                                                ifelse(step == 9, yes = "Data set B: + always employed",
                                                                                       ifelse(step == "A", yes = "Temp $\\rightarrow$ perm",
                                                                                              ifelse(step == "B", yes = "Perm $\\rightarrow$ temp",
                                                                                                     ifelse(step == "C", yes = "Unmp $\\rightarrow$ perm",
                                                                                                            ifelse(step == "D", yes = "Unmp $\\rightarrow$ temp",
                                                                                                                   no = NA))))))))))))))) %>%
  mutate(step = ifelse(step == "A", yes = "B", 
                       ifelse(step == "B", yes = "B",
                              ifelse(step == "C", yes = "A",
                                     ifelse(step == "D", yes = "A", no = step))))) %>%
  select(step,notes,total,obs_diff,order) %>%
  arrange(order)

df_filter_06

# Table for paper -----------------------------------------

df_table <- df_filter_06 %>%
  select(-order)


# VARIABLE LABLES

panel_a_columns <- c("\\multicolumn{2}{l}{{\\bf Panel A:} Sample selection criteria} & \\multicolumn{2}{l}{Observations} \\\\  \\cmidrule(lr){3-4} \n 
                     Step     &  Description     & Uniqe n  & $\\Delta$ \\\\ \n
                     \\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\\\[-1.8ex] \n")
panel_b_columns <- c("\\hline \\\\[-1.8ex]  \n 
                     \\multicolumn{2}{l}{{\\bf Panel B:} Data sets by event type} & \\multicolumn{2}{l}{Treated} \\\\  \\cmidrule(lr){3-4} \n
                     Data set  & Event type  (if treated, must be employed after treatment)  & \\#  & \\% \\\\ \n
                     \\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\\\[-1.8ex] \n")

hline_top <- ("\\hline\\hline \\\\[-1.8ex] \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              "l", # description
              "l", #unique obs
              "l" #diff. unique
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
        pos = list(0,0,10,14),
        command = c(hline_top,
                    panel_a_columns,
                    panel_b_columns,
                    hline_bot)),
      comment = FALSE
)

t

# Table for presentation -----------------------------------------

df_table <- df_filter_06 %>%
        filter(step!="A") %>%
        filter(step!="B") %>%
        filter(step!="C") %>%
        filter(step!="D") %>%
        select(-order)

# VARIABLE LABLES

panel_a_columns <- c("\\multicolumn{2}{l}{Sample selection criteria} & \\multicolumn{2}{l}{Observations} \\\\  \\cmidrule(lr){3-4} \n 
                     Step     &  Description     & Uniqe n  & $\\Delta$ \\\\ \n
                     \\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\\\[-1.8ex] \n")

data_set_a <- c("&  \\multicolumn{3}{l}{\\hspace{5mm}Used to examine $U \\rightarrow T$ and $U \\rightarrow P$} \\\\ \n")
data_set_b <- c("&  \\multicolumn{3}{l}{\\hspace{5mm}Used to examine $P \\rightarrow T$ and $T \\rightarrow P$} \\\\ \n")

hline_top <- ("\\hline\\hline \\\\[-1.8ex] \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              "l", # description
              "l", #unique obs
              "l" #diff. unique
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_presentation.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
              pos = list(0,0,9,10,10),
              command = c(hline_top,
                          panel_a_columns,
                          data_set_a,
                          data_set_b,
                          hline_bot)),
      comment = FALSE
)

t

beep()

