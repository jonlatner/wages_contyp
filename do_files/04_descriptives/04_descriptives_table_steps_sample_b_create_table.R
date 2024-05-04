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

df_append <- df_append %>%
  mutate(notes = ifelse(step == 0, yes = "Raw data",
                        ifelse(step == 1, yes = "Panel years between 2000 and 2018",
                               ifelse(step == 2, yes = "Prime age (25 - 54)",
                                      ifelse(step == 3, yes = "Labour force participant (employed or unemployed)",
                                             ifelse(step == 4, yes = "Unemployed or employed with contract type",
                                                    ifelse(step == 5, yes = "Unemployed or employed with wages",
                                                           ifelse(step == 6, yes = "Unemployed or employed with monthly hours between 40 and 320",
                                                                  ifelse(step == 7, yes = "Non missing education or gender",
                                                                         ifelse(step == 8, yes = "Hourly wages within the top/bottom 0.005 percentile",
                                                                                ifelse(step == 9, yes = "Sample A: At least 3 observations",
                                                                                       ifelse(step == 10, yes = "Sample B: + always employed",
                                                                                              ifelse(step == "A", yes = "Temp $\\rightarrow$ perm",
                                                                                                     ifelse(step == "B", yes = "Perm $\\rightarrow$ temp",
                                                                                                            ifelse(step == "C", yes = "Unmp $\\rightarrow$ perm",
                                                                                                                   ifelse(step == "D", yes = "Unmp $\\rightarrow$ temp",
                                                                                       no = NA)))))))))))))))) %>%
  select(step, notes, everything())

# Table, by country -----------------------------------------

df_table <- df_append
df_table

# VARIABLE LABLES

columns_header_top <- c("
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

hline_top <- c("\\toprule \n")
hline_bot <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{20}{p{14in}}{Notes: In Panel A: n - is unique observations and $\\Delta$ - is difference in n from previous step.  In Panel B: \\# - is unique n who experienced at least 1 event and \\% - is percent who experienced an event.} \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", "l",
              ">{\\raggedright\\arraybackslash}p{2.5in}", # notes
              "l", "l", "l", "l", "l", "l", "l", "l", "l",
              "l", "l", "l", "l", "l", "l", "l", "l", "l"
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country_full.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,0,11,15),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_bot_1,
                    columns_header_bot_2,
                    hline_bot)),
      comment = FALSE
)

t


# Table, only panel A -----------------------------------------

t_a <- t %>%
  filter(row_number()<12)
t_a

columns_header_top <- c("
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

hline_bot <- c("\\bottomrule \n")

print(t_a, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country_simple.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,0,10),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_bot_1,
                    hline_bot)),
      comment = FALSE
)

t_a


# Table, only panel b -----------------------------------------

t_b <- t %>%
  filter(row_number()>11)
t_b

columns_header_bot_1 <- c("
\\multicolumn{1}{l}{Data set} & 
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

align(t_b) <- c("l", "l",
              "l", # notes
              "l", "l", "l", "l", "l", "l", "l", "l", "l",
              "l", "l", "l", "l", "l", "l", "l", "l", "l"
) 

columns_header_top <- c("
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

hline_bot <- c("\\bottomrule \n")

print(t_b, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_country_transitions.tex"),
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

t_b

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
\\cmidrule(lr){2-3}
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
hline_bot <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{4}{p{6in}}{Notes: In Panel A: n - is unique observations and $\\Delta$ - is difference in n from previous step.  In Panel B: \\# - is unique n who experienced at least 1 event and \\% - is percent who experienced an event.} \n")

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
        pos = list(0,0,0,11,15),
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
        filter(row_number()<12)

columns_header_top <- c("
&  & 
\\multicolumn{2}{l}{Total (all countries)}
\\\\  \n 
")

align(t) <- c("l", "l",
              ">{\\raggedright\\arraybackslash}p{4in}", # notes
              "l", "l"
) 

align(t) <- c("l", "l",
              "l", # notes
              "l", "l"
) 

hline_bot <- c("\\bottomrule \n")

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_presentation.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.argsø = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
              pos = list(0,0,0,11),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_bot_1,
                          hline_bot)),
      comment = FALSE
)

t

# Table, total across all countries (paper) -----------------------------------------

hline_bot <- c("\\bottomrule \\\\[-1.8ex] \n
               \\multicolumn{4}{p{6.5in}}{Notes: n refers to unique individuals. $\\Delta$ refers to percent change from previous step.  Please see Appendix \\ref{sec:sample_selection} for more details on the sample selection criteria, including country specific frequency counts.} \n")

hline_bot <- c("\\bottomrule")

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_table_steps_paper.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,0,11),
        command = c(hline_top,
                    columns_header_top,
                    columns_header_bot_1,
                    hline_bot)),
      comment = FALSE
)

t
