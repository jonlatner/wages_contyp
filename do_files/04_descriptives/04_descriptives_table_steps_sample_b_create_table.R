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

# Adapt this pathway!
setwd("~/GitHub/wages_contyp/")

data_files = "data_files/"
results = "results/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(xtable)
library(texreg)
library(beepr)

options(scipen = 999) # disable scientific notation

# load data -----

df_append <- readRDS(file = paste0(data_files,"04_df_descriptives_table_steps_sample.rds"))

# Table, by country -----------------------------------------

df_table <- df_append
df_table

# VARIABLE LABLES

columns_header_top <- c("
\\multicolumn{14}{l}{{\\bf Panel A:} Sample selection criteria} \\\\ \n
&  & 
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
\\cmidrule(lr){3-4}
\\cmidrule(lr){5-6}
\\cmidrule(lr){7-8}
\\cmidrule(lr){9-10}
\\cmidrule(lr){11-12}
\\cmidrule(lr){13-14}
\\cmidrule(lr){15-16}
\\cmidrule(lr){17-18}
\\cmidrule(lr){19-20}
\\\\[-1.8ex]  \n 
")

columns_header_bot_1 <- c("
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{l}{Description} 
& n & $\\Delta$
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
\\cmidrule(lr){19-20}
\\\\[-1.8ex]  \n 
")

columns_header_bot_2 <- c("
\\hline \\\\[-1.8ex]  \n 
\\multicolumn{14}{l}{{\\bf Panel B:} Data sets by event type (if treated, must be employed after treatment)} \\\\ \n
& 
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
& \\# & \\%
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
\\cmidrule(lr){19-20}
\\\\[-1.8ex]  \n 
")

hline_top <- ("\\toprule \n")
hline_bot <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{20}{p{12in}}{Note: n - is unique observations.  $\\Delta$ - is difference in n from previous step.  \\# - is unique n who experienced at least 1 event.  \\% - is percent who experienced an event.} \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", "l",
              ">{\\raggedright\\arraybackslash}p{2.5in}", # notes
              "l", "l", "l", "l", "l", "l", "l", "l", "l",
              "l", "l", "l", "l", "l", "l", "l", "l", "l"
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,0,8,12),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_bot_1,
                    columns_header_bot_2,
                    hline_bot)),
      comment = FALSE
)

t

beep()

# Table, total across all countries -----------------------------------------


df_table <- df_append[1:4]
df_table

# VARIABLE LABLES

columns_header_top <- c("
\\multicolumn{4}{l}{{\\bf Panel A:} Sample selection criteria} \\\\ \n
&  & 
\\multicolumn{2}{l}{Total (all countries)}
\\\\  \n 
")

columns_header_mid <- c("
\\cmidrule(lr){3-4}
\\\\[-1.8ex]  \n 
")

columns_header_bot_1 <- c("
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{l}{Description} 
& n & $\\Delta$
\\\\ 
\\cmidrule(lr){1-2}
\\cmidrule(lr){3-4}
\\\\[-1.8ex]  \n 
")

columns_header_bot_2 <- c("
\\hline \\\\[-1.8ex]  \n 
\\multicolumn{4}{l}{{\\bf Panel B:} Data sets by event type} \\\\ \n
& & 
\\# & \\%
\\\\ 
\\cmidrule(lr){1-2}
\\cmidrule(lr){3-4}
\\\\[-1.8ex]  \n 
")

hline_top <- ("\\toprule \n")
hline_bot <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{4}{p{6in}}{Note: n - is unique observations.  $\\Delta$ - is difference in n from previous step.  \\# - is unique n who experienced at least 1 event.  \\% - is percent who experienced an event.} \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", "l",
              ">{\\raggedright\\arraybackslash}p{4in}", # notes
              "l", "l"
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
              pos = list(0,0,0,8,12),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_bot_1,
                          columns_header_bot_2,
                          hline_bot)),
      comment = FALSE
)

t

# Table, total across all countries (presentation) -----------------------------------------

t <- t %>%
        filter(row_number()<9)

hline_bot <- c("\\bottomrule \n")

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
              pos = list(0,0,0,8),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_bot_1,
                          hline_bot)),
      comment = FALSE
)

t
