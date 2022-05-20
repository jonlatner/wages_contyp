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
results = "projects/mobility/results/"
tables = "projects/mobility/tables/"

# LIBRARY
library(tidyverse)
library(xtable)

options(scipen = 999) # disable scientific notation

# load data -----

df_filter <- read.csv(paste0(data_files, "df_filter_steps.csv")) 
df_filter[1] <- NULL
df_filter

# clean data -----

df_filter <- df_filter %>%
  mutate(steps = row_number()-1) %>%
  mutate(Total = rowSums(.))

df_filter


df_filter <- gather(df_filter,key = steps)
df_filter <- df_filter %>%
  rename(country=steps) %>%
  group_by(country) %>%
  mutate(steps = row_number()-1) %>%
  mutate(change = ifelse(steps<3, yes = value/lag(value)-1, no = NA)) %>%
  ungroup()

df_filter

df_filter <- df_filter %>%
  pivot_wider(values_from = c(value,change), names_from = steps) %>%
  select(country, value_0, matches("_1"), matches("_2"), value_3, value_4)
df_filter

# Table -----------------------------------------

df_table <- df_filter 
df_table

df_table <- df_table %>%
  select(-value_3)

# VARIABLE LABLES

columns_top <- c("[-1.8ex]
\\multicolumn{1}{l}{Country} & \\multicolumn{5}{l}{Unique Observations} & \\multicolumn{1}{l}{Total}
\\\\  \n 
")

columns_header <- c("
 & 
\\multicolumn{1}{l}{Raw data} & 
\\multicolumn{2}{l}{Individual filters} & 
\\multicolumn{2}{l}{Panel filters} &
\\\\  \n 
")

midrule_1 <- c("
            \\cmidrule(lr){2-6} 
             ")
midrule_2 <- c("
            \\cmidrule(lr){3-4} 
            \\cmidrule(lr){5-6} 
             ")

columns_subheader <- c("
 & &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{$\\Delta$} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{$\\Delta$}
\\\\  \n 
")

df_table <- 
  df_table %>% remove_rownames %>% column_to_rownames(var="country")
hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

mdat <- matrix(c(0,0,0,3,0,3,0),
               nrow = 10, ncol=7, byrow=TRUE)
t <- xtable(df_table,digits=mdat)


align(t) <- c("l", #first
              "l", #step
              "l", #unique obs
              "l", #diff total
              "l", #diff. unique
              "l", #step
              "l" #unique obs
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"table_steps_wide.tex"),
      include.rownames = TRUE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE", 
      hline.after = c(0,9),
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row = list(
              pos = list(0,0,0,0,0,0,10),
              command = c(hline_top,
                          columns_top,
                          midrule_1,
                          columns_header,
                          midrule_2,
                          columns_subheader,
                          hline_bot)),
      comment = FALSE
)
t
