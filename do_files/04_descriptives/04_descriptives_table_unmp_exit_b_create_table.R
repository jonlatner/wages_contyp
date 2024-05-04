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
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(car)

options(scipen = 999) # disable scientific notation

# load data -----

df_table <- read.csv(paste0(data_files, "04_df_descriptives_table_unmp_exit.csv"))
df_table


df_table_long <- df_table %>% 
  pivot_longer(!country) 

df_table_long <- df_table_long %>%
  group_by(country) %>%
  mutate(obs_diff = paste0(round((value/lag(value,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
  ) %>%
  ungroup()

df_table_long
df_table_long$country <- recode(df_table_long$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE-LSP'='Netherlands'; 'UK'='United Kingdom'")

df_table_wide <- df_table_long %>%
  pivot_wider(names_from = country, values_from = c("value","obs_diff"))
df_table_wide


df_table_wide <- df_table_wide %>%
  mutate(periods = row_number()) %>%
  select(periods, name,  
         matches("Australia"), 
         matches("Germany"), 
         matches("Italy"), 
         matches("Japan"), 
         matches("Korea"), 
         matches("Netherlands"),
         matches("Switzerland"),
         matches("United Kingdom"),
  )
df_table_wide$name <- c("Total observations", "Ever unemployed", "+ Exit unemployment", "+ 1 period after exit")
df_table_wide$periods <- c("", "1 period", "2 periods", "3 periods")

# create table -----

df_table <- df_table_wide
df_table

# VARIABLE LABLES

columns_header_top <- c("
&&
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
\\\\[-1.8ex]  \n 
")

columns_header_bot_1 <- c("
\\multicolumn{1}{l}{Periods} & 
\\multicolumn{1}{l}{Description} 
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
& n & $\\Delta$
\\\\ 
\\cmidrule(lr){1-2}
\\cmidrule(lr){3-4}
\\cmidrule(lr){5-6}
\\cmidrule(lr){7-8}
\\cmidrule(lr){9-10}
\\cmidrule(lr){11-12}
\\cmidrule(lr){13-14}
\\cmidrule(lr){15-16}
\\cmidrule(lr){17-18}
\\\\[-1.8ex]  \n 
")

hline_top <- c("\\toprule \n")
hline_bot <- c("\\bottomrule  \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", "l", "l", "l", 
              "l", "l", "l", "l", 
              "l", "l", "l", "l", 
              "l", "l", "l", "l", 
              "l", "l", "l"
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_unemployment.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,0,4),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_bot_1,
                    hline_bot)),
      comment = FALSE
)

t

