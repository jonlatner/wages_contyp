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
library(data.table)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter_01 <- readRDS(file = paste0(data_files,"3a_df_filter_steps.rds"))

df_filter_02 <- readRDS(file = paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

# Clean/summarize main data ----

step_8 <- df_filter_02 %>%
  select(country,pid) %>%
  mutate(total = sum(n())) 
step_8 <- data.table(step_8, key = "country,pid")
step_8 <- step_8[, head(.SD, 1), by = key(step_8)]
step_8 <- data.table(step_8, key = "country")
step_8 <- step_8[, .(count = .N), by = key(step_8)]
step_8$step <- 8

df_filter_01 <- rbind(df_filter_01,step_8)

df_filter <- df_filter_01 %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  select(step,total)

df_filter

# Clean/summarize event data ----

df_event_t_p <- df_filter_02 %>%
  filter(event_t_p_yes == 1 & event_t_p_keep == 1)

df_event_u_p <- df_filter_02 %>%
  filter(event_u_p_yes == 1 & event_u_p_keep == 1)

df_event_u_t <- df_filter_02 %>%
  filter(event_u_t_yes == 1 & event_u_t_keep == 1)

df_event_p_t <- df_filter_02 %>%
  event_p_t_yes == 1 & event_p_t_keep == 1

# Clean/summarize data ----

df_filter <- df_filter %>%
  filter(country!="NE-LISS")
table(df_filter$country)

df_filter$country <- recode(df_filter$country, "'NE-LSP'='NE'")
df_filter$country_name <- recode(df_filter$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

# Clean data ----

df_filter <- df_filter %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
  )
df_filter


df_filter <- df_filter %>%
  mutate(notes = ifelse(step == 0, yes = "Raw data",
                        ifelse(step == 1, yes = "Panel years between 2000 and 2018",
                               ifelse(step == 2, yes = "Labour force participant",
                                      ifelse(step == 3, yes = "Prime age (25 - 54)",
                                             ifelse(step == 4, yes = "Unemployed or employed with contract type",
                                                    ifelse(step == 5, yes = "Unemployed or employed with monthly hours $>= 1$",
                                                           ifelse(step == 6, yes = "Non missing education or gender",
                                                                  ifelse(step == 7, yes = "Drop observations with hourly wages in the top/bottom 0.005 percentile",
                                                                         ifelse(step == 8, yes = "Keep observations in at least 2 periods",
                                                                                       no = NA))))))))))
df_filter

# Table -----------------------------------------

df_table <- df_filter %>%
  select(step,total,obs_diff,notes)


# VARIABLE LABLES

columns_header <- c("[-1.8ex]
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{l}{Unique observations} &
\\multicolumn{1}{l}{Percent change} & 
\\multicolumn{1}{l}{Notes} 
\\\\  \n 
")

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              "l", #unique obs
              "l", #diff. unique
              ">{\\raggedright\\arraybackslash}p{4in}" # notes
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = FALSE,
      add.to.row = list(
        pos = list(0,0,8),
        command = c(hline_top,
                    columns_header,
                    hline_bot)),
      comment = FALSE
)

t
