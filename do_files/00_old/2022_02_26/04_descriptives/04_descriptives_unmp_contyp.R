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
library(zoo)
library(Hmisc)
library(beepr)
library(car)
library(forcats)
library(xtable)

options(scipen = 999) # disable scientific notation

# load data -----

df_first <- readRDS(paste0(data_files, "03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_first <- df_first %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))


# clean sample data -----

# Unmp to temp
df_sample_u_t <- df_first %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_t")) %>%
        filter(event_u_t_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_u_t_drop_01 = ifelse(year > event_u_t_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_t_drop_01 = last(event_u_t_drop_01),
               event_u_t_drop_02 = ifelse(year > event_u_t_year & event_u_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0) %>%
        mutate(difference = year - event_u_t_year,
               number = ifelse(year > event_u_t_year & difference < 5, yes = row_number(), no = 0),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
               ) %>%
        filter(max>2) %>%
        ungroup() %>%
        mutate(event_u_t_yes_final = 1)

with(df_sample_u_t,table(country,event_u_t))

# Unmp to perm
df_sample_u_p <- df_first %>%
        select(country,pid,year,age,unmp,temp,perm,matches("event_u_p")) %>%
        filter(event_u_p_yes == 1) %>%
        group_by(country, pid) %>%
        mutate(event_u_p_drop_01 = ifelse(year > event_u_p_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_p_drop_01 = last(event_u_p_drop_01),
               event_u_p_drop_02 = ifelse(year > event_u_p_year & event_u_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0) %>%
        mutate(difference = year - event_u_p_year,
               number = ifelse(year > event_u_p_year & difference < 5, yes = row_number(), no = 0),
               max = max(number),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        filter(max>2) %>%
        ungroup() %>%
        mutate(event_u_p_yes_final = 1)

with(df_sample_u_p,table(country,event_u_p))

# Event - unmp into emp ----

df_unmp_01 <- df_first %>%
        select(country, pid, age, year, year_lag, unmp, perm, temp, matches("u_t"), matches("u_p")) %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
               exit_unmp_yes = max(event_exit_unmp),
               unmp_ever = max(unmp),
               count = row_number(),
               max = max(count),
               age_max = max(age),
               unique = ifelse(row_number()==1, yes = 1, no = 0),
        ) %>%
        filter(unmp_ever==1)

# Total possible events
t_01 <- with(subset(df_unmp_01,unique == 1),table(country,useNA = "ifany"))
t_01 <- data.frame(t_01)
t_01$step <- 1
t_01

# must exit unemployment
df_unmp_02 <- df_unmp_01 %>%
        filter(exit_unmp_yes == 1)

t_02 <- with(subset(df_unmp_02,unique == 1),table(country,useNA = "ifany"))
t_02 <- data.frame(t_02)
t_02$step <- 2
t_02

# must be observable after treatment (i.e. 3 periods of observation)
df_unmp_03_u_p <- df_unmp_02 %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),matches("event_u_p"),unique) %>%
        group_by(country, pid) %>%
        mutate(event_u_p_drop_01 = ifelse(year > event_u_p_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_p_drop_01 = last(event_u_p_drop_01),
               event_u_p_drop_02 = ifelse(year > event_u_p_year & event_u_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_p_drop_01 == 0) %>%
        mutate(difference = year - event_u_p_year,
               number = ifelse(year > event_u_p_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup()

df_unmp_03_u_t <- df_unmp_02 %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),matches("event_u_t"),unique) %>%
        group_by(country, pid) %>%
        mutate(event_u_t_drop_01 = ifelse(year > event_u_t_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_t_drop_01 = last(event_u_t_drop_01),
               event_u_t_drop_02 = ifelse(year > event_u_t_year & event_u_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_t_drop_01 == 0) %>%
        mutate(difference = year - event_u_t_year,
               number = ifelse(year > event_u_t_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup()

with(subset(df_unmp_03_u_p,unique == 1),table(country,useNA = "ifany"))

with(subset(df_unmp_03_u_t,unique == 1),table(country,useNA = "ifany"))

t_03_u_p <- with(subset(df_unmp_03_u_p,unique == 1),table(country,useNA = "ifany"))
t_03_u_p <- data.frame(t_03_u_p)
t_03_u_p

t_03_u_t <- with(subset(df_unmp_03_u_t,unique == 1),table(country,useNA = "ifany"))
t_03_u_t <- data.frame(t_03_u_t)
t_03_u_t

t_03 <- rbind(t_03_u_p,t_03_u_t) %>%
        group_by(country) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup()

t_03$step <- 3
t_03

# must be employed after transition into employment
df_unmp_04_u_p <- df_unmp_03_u_p %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),matches("event_u_p"),unique) %>%
        group_by(country, pid) %>%
        mutate(event_u_p_drop_01 = ifelse(year > event_u_p_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_p_drop_01 = last(event_u_p_drop_01),
               event_u_p_drop_02 = ifelse(year > event_u_p_year & event_u_p_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_p_drop_01 == 0 & event_u_p_drop_02 == 0) %>%
        mutate(difference = year - event_u_p_year,
               number = ifelse(year > event_u_p_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup()

df_unmp_04_u_t <- df_unmp_03_u_t %>%
        select(country,pid,year,unmp,temp,perm,matches("unmp"),matches("event_u_t"),unique) %>%
        group_by(country, pid) %>%
        mutate(event_u_t_drop_01 = ifelse(year > event_u_t_year, yes = 0, no = 1), # keep if observable after treatment
               event_u_t_drop_01 = last(event_u_t_drop_01),
               event_u_t_drop_02 = ifelse(year > event_u_t_year & event_u_t_time > 0 & unmp == 1, yes = 1, no = 0), # keep employed observations after treatment event
        ) %>%
        filter(event_u_t_drop_01 == 0 & event_u_t_drop_02 == 0) %>%
        mutate(difference = year - event_u_t_year,
               number = ifelse(year > event_u_t_year & difference < 5, yes = row_number(), no = 0),
               max = max(number)) %>%
        filter(max>2) %>%
        ungroup() 

with(subset(df_unmp_04_u_p,unique == 1),table(country,useNA = "ifany"))

with(subset(df_unmp_04_u_t,unique == 1),table(country,useNA = "ifany"))

t_04_u_p <- with(subset(df_unmp_04_u_p,unique == 1),table(country,useNA = "ifany"))
t_04_u_p <- data.frame(t_04_u_p)
t_04_u_p

t_04_u_t <- with(subset(df_unmp_04_u_t,unique == 1),table(country,useNA = "ifany"))
t_04_u_t <- data.frame(t_04_u_t)
t_04_u_t

t_04 <- rbind(t_04_u_p,t_04_u_t) %>%
        group_by(country) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup()

t_04$step <- 4
t_04

# summarise selection criteria ----

beep()

# There are duplicates where individuals move back to unemployment, but this does not affect unique case counts
with(df_sample_u_t,table(country,event_u_t)) # total number of events
with(subset(df_sample_u_t,unique==1),table(country,event_u_t)) # total number of individuals

with(df_sample_u_p,table(country,event_u_p)) # total number of events
with(subset(df_sample_u_p,unique==1),table(country,event_u_p)) # total number of individuals

df_table <- rbind(t_01,t_02,t_03,t_04)
df_table$event <- "NA"

t_04_u_p$event <- "u_p"
t_04_u_p$step <- 5
t_04_u_t$event <- "u_t"
t_04_u_t$step <- 5

df_table <- bind_rows(df_table,t_04_u_p,t_04_u_t) %>%
        arrange(country, step, event)

df_table

df_table_total <- df_table %>%
        group_by(step,event) %>%
        summarise(Freq = sum(Freq)) %>%
        ungroup() %>%
        mutate(country = "Total")

df_table_total

df_append <- rbind(df_table,df_table_total)

df_append$country_name <- recode(df_append$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'; 'Total'='Total' ")
df_append$country_name <- fct_relevel(df_append$country_name, "Total", after = 0) # forcats
df_append$event <- as.factor(df_append$event)

# reshape table long to wide ----

df_table_wide <- df_append %>%
        select(-country) %>%
        arrange(country_name,step) %>%
        group_by(country_name) %>%
        mutate(row = row_number(),
               obs_diff = paste0(round((Freq/lag(Freq,1)-1)*100,0),"\\%"),
               obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
               obs_diff = ifelse(event == "u_p", yes = paste0(round((Freq/lag(Freq,1))*100,0),"\\%"),
                                 ifelse(event == "u_t", yes = paste0(round((Freq/lag(Freq,2))*100,0),"\\%"),
                                        no = obs_diff
                                        ))
        ) %>%
        pivot_wider(names_from = c("country_name"),
                    values_from = c("Freq", "obs_diff"),
                    ) %>%
        ungroup()

df_table_wide

df_table_wide <- df_table_wide %>%
        select(step, event, 
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
df_table_wide


df_table_wide <- df_table_wide %>%
        mutate(event = ifelse(step == 1, yes = "Total unemployment events",
                              ifelse(step == 2, yes = "Must exit unemployment",
                                     ifelse(step == 3, yes = "Observable after exit",
                                            ifelse(step == 4, yes = "Employed after exit",
                                                   ifelse(step == 5, yes = "Unmp $\\rightarrow$ perm",
                                                          ifelse(step == "5", yes = "Unmp $\\rightarrow$ temp",
                                                                 no = NA))))))) %>%
        mutate(step = ifelse(step == 5, yes = NA, no = step))

# Table -----------------------------------------

df_output <- df_table_wide
df_output


# VARIABLE LABLES

columns_header_top <- c("[-1.8ex]
\\multicolumn{20}{l}{{\\bf Panel A:} Sample selection criteria} \\\\ \n
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
\\multicolumn{20}{l}{{\\bf Panel B:} Exit to employment, by contract type} \\\\ \n
& 
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
&  \\# & \\%
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

hline_top <- ("\\\\[-1.8ex]\\hline\\hline \\\\ \n")
hline_bot <- c("\\hline \\\\[-1.8ex] \\multicolumn{20}{p{11in}}{Note: n - is unique observations.  $\\Delta$ - is difference in n from previous step.  \\# - is number of transitions.  \\% - is percent of total transitions from step 4.} \n")

t <- xtable(df_output, digits = 0)

align(t) <- c("l", "l", "l",
              "l", "l", "l", "l", "l", "l", "l", "l", "l",
              "l", "l", "l", "l", "l", "l", "l", "l", "l"
) 

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_unmp_steps_contyp.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
              pos = list(0,0,0,4,6),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_bot_1,
                          columns_header_bot_2,
                          hline_bot)),
      comment = FALSE
)

t
