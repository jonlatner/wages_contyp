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
library(car) # recode
library(forcats)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter <- readRDS(file = paste0(data_files,"3a_df_filter_steps.rds"))

# Clean/summarize data ----

df_filter <- df_filter %>%
  filter(country!="NE-LISS")
table(df_filter$country)

df_filter$country <- recode(df_filter$country, "'NE-LSP'='NE'")
df_filter$country_name <- recode(df_filter$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_filter_country <- df_filter %>%
  group_by(country,step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(country,step) %>%
  select(country_name,step,total)

df_filter_total <- df_filter %>%
  group_by(step) %>%
  mutate(total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  mutate(country_name = "Total") %>%
  select(country_name,step,total)

df_filter <- rbind(df_filter_country,df_filter_total)
df_filter <- df_filter %>%
  arrange(country_name,step)

df_filter$country_name <- fct_relevel(df_filter$country_name, "Total", after = 0) # forcats

# Clean data ----

df_filter <- df_filter %>%
  group_by(country_name) %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
  ) %>%
  ungroup()

df_filter <- df_filter %>%
  mutate(notes = ifelse(step == 0, yes = "Raw data",
                        ifelse(step == 1, yes = "Panel years between 2000 and 2018",
                               ifelse(step == 2, yes = "Labour force participant",
                                      ifelse(step == 3, yes = "Prime age (25 - 54)",
                                             ifelse(step == 4, yes = "Unemployed or employed with contract type",
                                                    ifelse(step == 5, yes = "Unemployed or employed with monthly hours $>= 1$",
                                                           ifelse(step == 6, yes = "Non missing education or gender",
                                                                  ifelse(step == 7, yes = "Drop observations with hourly wages in the top/bottom 0.005 percentile",
                                                                                       no = NA)))))))))
df_filter

# Reshape data ----

df_filter_wide <- df_filter %>%
  pivot_wider(names_from = country_name, values_from = c("total","obs_diff"))
df_filter_wide <- df_filter_wide %>%
  select(step, notes, 
         matches("_Total"),
         matches("Australia"), 
         matches("Germany"), 
         matches("Italy"), 
         matches("Japan"), 
         matches("Korea"), 
         matches("Netherlands"),
         matches("Switzerland"),
         matches("United Kingdom"),
  )
df_filter_wide

# Table -----------------------------------------

df_table <- df_filter_wide


# VARIABLE LABLES

columns_header_top <- c("[-1.8ex]
\\multicolumn{2}{l}{Step} & 
\\multicolumn{2}{l}{Total (all countries)} &
\\multicolumn{2}{l}{Australia} &
\\multicolumn{2}{l}{Germany} &
\\multicolumn{2}{l}{Italy} &
\\multicolumn{2}{l}{Japan} &
\\multicolumn{2}{l}{Korea} &
\\multicolumn{2}{l}{Netherlands} &
\\multicolumn{2}{l}{Switzerland} &
\\multicolumn{2}{l}{United Kingdom}
\\\\  \n 
")

columns_header_mid <- c("
\\cmidrule(lr){1-2}
\\cmidrule(lr){3-4}
\\cmidrule(lr){5-6}
\\cmidrule(lr){7-8}
\\cmidrule(lr){9-10}
\\cmidrule(lr){11-12}
\\cmidrule(lr){13-14}
\\cmidrule(lr){15-16}
\\cmidrule(lr){17-18}
\\cmidrule(lr){19-20}
\\\\  \n 
")

columns_header_bot <- c("
& & \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
& \\# & $\\Delta$
\\\\  \n 
")

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \\\\[-1.8ex] \\multicolumn{12}{l}{Note: \\# Indicates unique observations.  $\\Delta$ Indicates difference in observations from previous step} \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", "l",
              ">{\\raggedright\\arraybackslash}p{2in}", # notes
              "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l", "l"
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = FALSE,
      add.to.row = list(
        pos = list(0,0,0,0,8),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_mid,
                    columns_header_bot,
                    hline_bot)),
      comment = FALSE
)

t
