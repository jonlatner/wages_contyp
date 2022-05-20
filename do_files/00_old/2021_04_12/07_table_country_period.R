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

# LIBRARY
library(tidyverse)
library(xtable)
library(dummies)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "df_sample_clean.rds")) 

df_example <- df_sample_0 %>%
  filter(study_period==2001) %>%
  filter((pid==100142 | pid == 100270)) %>%
  select(pid,year,ln_hourly_wage,unmp,perm,temp,post_temp,post_temp_sq)
df_example

df_sample_0 <- df_sample_0 %>%
  select(country,study_period,pid,year)

# Calculate repeated observations -----------------------------------------

df_obs_0 <- df_sample_0 %>%
  group_by(country,study_period,pid) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(country,pid,study_period)

df_obs_1 <- df_obs_0 %>%
  group_by(country,pid) %>%
  mutate(count = row_number()) %>%
  filter(row_number()==n()) %>%
  ungroup()

df_obs_2 <- df_obs_1 %>%
  group_by(country) %>%
  summarise(count = mean(count)) %>%
  ungroup()

t_df_obs_2 =  as.data.frame(t(df_obs_2[,-1]))
colnames(t_df_obs_2) <- df_obs_2$country
t_df_obs_2

rm(df_obs_2,df_obs_1,df_obs_0)

# Calculate unique observations -----------------------------------------

df_unique_1 <- df_sample_0 %>%
  group_by(country,pid) %>%
  slice(1) %>%
  ungroup()

df_unique_2 <- df_unique_1 %>%
  group_by(country) %>%
  tally() %>%
  ungroup()
names <- df_unique_2$country

t_df_unique_2 =  as.data.frame(t(df_unique_2[,-1]))
colnames(t_df_unique_2) <- names
t_df_unique_2

rm(df_unique_1,df_unique_2,names)

# Study period table -----------------------------------------

df_table <- data.frame(with(df_sample_0,table(country,study_period)))
df_table

df_table_wide <- df_table %>%
  spread(key = country, value = Freq) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Sum"))) %>%
  mutate(study_period = ifelse(is.na(study_period), yes = "Total", no = as.character(study_period)))

df_table <- df_table_wide
df_table[1] <- NULL
rm(df_table_wide)

df_table <- rbind(df_table,t_df_unique_2,t_df_obs_2)

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

columns_header <- c("[-1.8ex] Study Period &

\\multicolumn{1}{l}{AU} & 
\\multicolumn{1}{l}{CH} &
\\multicolumn{1}{l}{DE} &
\\multicolumn{1}{l}{IT} &
\\multicolumn{1}{l}{JP} &
\\multicolumn{1}{l}{KO} &
\\multicolumn{1}{l}{NE (LISS)} &
\\multicolumn{1}{l}{NE (LSP)} &
\\multicolumn{1}{l}{UK}
\\\\  \n 
")

rownames(df_table) <- c("\\\\[-1.8ex] 2000 - 2006",
                        "2001 - 2007",
                        "2002 - 2008",
                        "2003 - 2009",
                        "2004 - 2010",
                        "2005 - 2011",
                        "2006 - 2012",
                        "2007 - 2013",
                        "2008 - 2014",
                        "2009 - 2015",
                        "2010 - 2016",
                        "2011 - 2017",
                        "2012 - 2018",
                        "Total N",
                        "Unique N",
                        "Avg. number of study period"
)

digits_1 <- matrix(c(0),nrow=15,ncol=10)
digits_2 <- matrix(c(2),nrow=1,ncol=10)
mdat <- rbind(digits_1, digits_2)
mdat

t <- xtable(df_table, digits = mdat)

align(t) <- c("l", #first
              "l", #step
              "l", #total obs
              "l", #unique obs
              "l", #diff total
              "l", #diff. unique
              "l", #first
              "l", #first
              "l", #first
              "l" #first
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"table_study_period_country.tex"),
      include.rownames = TRUE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE", 
      hline.after = c(0,13),
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row = list(
        pos = list(0,0,16),
        command = c(hline_top,
                    columns_header,
                    hline_bot)),
      comment = FALSE
)
t

# Example table -----------------------------------------

df_table <- df_example
df_table

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

columns_header <- c("[-1.8ex]
\\multicolumn{1}{l}{pid} & 
\\multicolumn{1}{l}{year} &
\\multicolumn{1}{l}{Hourly wage (LN)} &
\\multicolumn{1}{l}{Unmp} &
\\multicolumn{1}{l}{Perm} &
\\multicolumn{1}{l}{Temp} &
\\multicolumn{1}{l}{Post temp} &
\\multicolumn{1}{l}{Post temp$^2$}
\\\\  \n 
")

mdigits <- matrix(c(0,0,0,2,0,0,0,0,0),nrow=14,ncol=9, byrow=TRUE)
mdigits

t <- xtable(df_example, digits = mdigits)
t

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"table_example.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE", 
      hline.after = c(0,7),
      format.args = list(big.mark = "", decimal.mark = "."),
      add.to.row = list(
        pos = list(0,0,14),
        command = c(hline_top,
                    columns_header,
                    hline_bot)),
      comment = FALSE
)
t

