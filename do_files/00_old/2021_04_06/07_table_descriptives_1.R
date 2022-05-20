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


# clean data -----

df_sample_1 <- df_sample_0 %>%
  select(-post_temp_sq,post_temp_first)

df_sample_1 <- df_sample_1 %>%
  mutate(post_temp = ifelse((country == "IT" | country == "NE-LSP") & post_temp == 1, yes  = 2,
                            ifelse((country == "IT" | country == "NE-LSP") & post_temp == 3, yes  = 4,
                                   ifelse((country == "IT" | country == "NE-LSP") & post_temp == 5, yes  = 6,
                                          no = post_temp))))
                                          
df_dummy <- dummy(x = df_sample_1$age_cat, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)

df_dummy <- dummy(x = df_sample_1$edu_cat, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

df_dummy <- dummy(x = df_sample_1$post_temp, sep = "_")
df_sample_1 <- cbind(df_sample_1, df_dummy)
rm(df_dummy)

df_sample_1 <- df_sample_1 %>%
  mutate(wage_temp = ln_hourly_wage*temp,
         wage_unmp = ln_hourly_wage*unmp,
         wage_perm = ln_hourly_wage*perm)
  
# make table (1) post_temp -----

df_sample_2 <- df_sample_1 %>%
  filter(post_temp>0) %>%
  select(country,post_temp_1,post_temp_2,post_temp_3,post_temp_4,post_temp_5,post_temp_6)

df_table_1 <- df_sample_2 %>%
  group_by(country) %>%
  summarise_all(.funs = mean) %>%
  ungroup()
df_table_1[df_table_1 == 0] <- NA

df_table_1

# make table (2) categorical variables -----

df_sample_2 <- df_sample_1 %>%
  select(country,unmp,temp,perm,age_cat_1,age_cat_2,age_cat_3,edu_cat_1,edu_cat_2,edu_cat_3,male)

df_table_2 <- df_sample_2 %>%
  group_by(country) %>%
  summarise_all(.funs = mean) %>%
  ungroup()

# make table (3) continuous variables -----

df_sample_2 <- df_sample_1 %>%
  select(country,ln_hourly_wage,wage_unmp,wage_temp,wage_perm)

df_table_3 <- df_sample_2 %>%
  group_by(country) %>%
  summarise_all(.funs = mean) %>%
  ungroup()

# clean table -----

df_descriptives <- merge(df_table_3,df_table_2)
df_descriptives <- merge(df_descriptives,df_table_1)
rm(df_table_3,df_table_2,df_table_1)

n <- df_descriptives$country # first keep the  rownames
df_descriptives <- as.data.frame(t(df_descriptives[,-1])) # transpose all but the first column (name)

df_total <- as.data.frame(with(df_sample_0,table(country)))
df_total <- as.data.frame(t(df_total[,-1]))
rownames(df_total) <- "Total"
df_total

df_descriptives <- rbind(df_descriptives,df_total)
df_descriptives
colnames(df_descriptives) <- n
df_descriptives

# Descriptives table -----------------------------------------

df_table <- df_descriptives
df_table

# VARIABLE LABLES

columns_header <- c("[-1.8ex] &

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

# VARIABLE LABLES
rownames(df_table) <- c("\\\\[-1.8ex] 
                        \\multicolumn{10}{l}{\\emph{Dependent variable}} \\\\
                        \\hspace{10mm}Hourly wage (LN)",
                        "\\hspace{10mm}Hourly wage (LN) - Unmp",
                        "\\hspace{10mm}Hourly wage (LN) - Temp",
                        "\\hspace{10mm}Hourly wage (LN) - Perm",
                        "\\\\[-1.8ex] 
                        \\multicolumn{10}{l}{\\emph{Employment status}} \\\\
                        \\hspace{10mm}Unmp",
                        "\\hspace{10mm}Temp",
                        "\\hspace{10mm}Perm",
                        "\\\\[-1.8ex] 
                        \\multicolumn{10}{l}{\\emph{Demographic characteristics}} \\\\
                        \\hspace{10mm}Younger age (25 - 34)",
                        "\\hspace{10mm}Middle age (35 - 44)",
                        "\\hspace{10mm}Older age (45 - 54)",
                        "\\\\[-1.8ex] 
                        \\hspace{10mm}Younger edu ($<$ Secondary)",
                        "\\hspace{10mm}Middle edu (Secondary)",
                        "\\hspace{10mm}Older edu ($>$ Secondary)",
                        "\\\\[-1.8ex] 
                        \\hspace{10mm}Male",
                        "\\\\[-1.8ex] 
                        \\multicolumn{10}{l}{\\emph{Years contract $|$ temp contract}} \\\\
                        \\hspace{10mm}1 Year",
                        "\\hspace{10mm}2 Years",
                        "\\hspace{10mm}3 Years",
                        "\\hspace{10mm}4 Years",
                        "\\hspace{10mm}5 Years",
                        "\\hspace{10mm}6 Years",
                        "\\\\[-1.8ex] 
                        Total observations"
)

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

digits_1 <- matrix(c(2),nrow=20,ncol=10)
digits_2 <- matrix(c(0),nrow=1,ncol=10)
mdat <- rbind(digits_1, digits_2)
mdat

t <- xtable(df_table, digits = mdat)
t

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
      file = paste0(tables,"table_descriptives.tex"),
      include.rownames = TRUE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE", 
      hline.after = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row = list(
        pos = list(0,0,21),
        command = c(hline_top,
                    columns_header,
                    hline_bot)),
      comment = FALSE
)
t
