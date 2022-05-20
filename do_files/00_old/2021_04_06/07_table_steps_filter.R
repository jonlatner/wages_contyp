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
  mutate(total = rowSums(.))

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
  pivot_wider(values_from = c(value,change), names_from = country) %>%
  select(steps, matches("_AU"), matches("_CH"), matches("_DE"), matches("_IT"), matches("_JP"), matches("_KO"), matches("_NE.LSP"), matches("_NE.LISS"), matches("_UK"), matches("_total"))


df_filter

df_filter <- df_filter %>%
        mutate(notes = ifelse(steps == 0, yes = "Raw data (2000 - 2018)",
                              ifelse(steps == 1, yes = "Individaul filters",
                                     ifelse(steps == 2, yes = "Panel filters",
                                            ifelse(steps == 3, yes = "Overlapping study periods (w/ duplicates)", 
                                                   ifelse(steps == 4, yes = "Total number of observations", 
                                                          no = NA))))))
df_filter

# Table -----------------------------------------

df_table <- df_filter
df_table

# VARIABLE LABLES

columns_header <- c("[-1.8ex]
\\multicolumn{1}{l}{Steps} & 
\\multicolumn{2}{l}{AU} & 
\\multicolumn{2}{l}{CH} &
\\multicolumn{2}{l}{DE} &
\\multicolumn{2}{l}{IT} &
\\multicolumn{2}{l}{JP} &
\\multicolumn{2}{l}{KO} &
\\multicolumn{2}{l}{NE (LSP)} &
\\multicolumn{2}{l}{NE (LISS)} &
\\multicolumn{2}{l}{UK} &
\\multicolumn{2}{l}{Total} &
\\multicolumn{1}{l}{Notes}
\\\\  \n 
")

midrule <- c("
            \\cmidrule(lr){2-3} 
            \\cmidrule(lr){4-5} 
            \\cmidrule(lr){6-7} 
            \\cmidrule(lr){8-9} 
            \\cmidrule(lr){10-11} 
            \\cmidrule(lr){12-13} 
            \\cmidrule(lr){14-15} 
            \\cmidrule(lr){16-17}
            \\cmidrule(lr){18-19}
            \\cmidrule(lr){20-21}
             ")

columns_subheader <- c("
 & 
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\multicolumn{1}{l}{\\#} &
\\multicolumn{1}{l}{\\%} &
\\\\  \n 
")

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = c(0,0,0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0,2,
                                 0))
t

align(t) <- c("l", #first
              "l", #step
              "l", #total obs
              "l", #unique obs
              "l", #diff total
              "l", #diff. unique
              "l", #first
              "l", #step
              "l", #total obs
              "l", #unique obs
              "l", #diff total
              "l", #diff. unique
              "l", #first
              "l", #step
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l", #total obs
              "l" #unique obs
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"table_steps.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE", 
      hline.after = c(0,3),
      format.args = list(big.mark = ",", decimal.mark = "."),
      add.to.row = list(
              pos = list(0,0,0,0,5),
              command = c(hline_top,
                          columns_header,
                          midrule,
                          columns_subheader,
                          hline_bot)),
      comment = FALSE
)
t
