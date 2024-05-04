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
library(car)
library(beepr)
library(zoo)
library(xtable)

options(scipen = 999) # disable scientific notation

# load data -----

df_sample_0 <- readRDS(paste0(data_files, "03c_df_sample_cleaned_prepared_multiple_events_data.rds"))

df_multiple_events <- df_sample_0 %>%
        filter(country != "NE-LISS") %>%
        select(country,pid,pidseq,year,year_lag,unmp,temp,perm,matches("event_u_p_yes"),matches("event_u_t_yes"))

df_multiple_events$country <- recode(df_multiple_events$country, "'NE-LSP'='NE'")

# Step 1: experience unemployment ----

df_step_01 <- df_multiple_events %>%
        arrange(country, pid, pidseq, year) %>%
        group_by(country, pid, pidseq) %>%
        mutate(event_exit_unmp = ifelse(unmp == 0 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1)+year_lag, yes = 1, no = 0), # identify treatment
               exit_unmp_yes = max(event_exit_unmp),
               unmp_ever = max(unmp),
        ) %>%
        filter(unmp_ever==1) %>%
        ungroup()

df_step_01a <- df_step_01 %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup()

# Total possible events 
t_01 <- with(df_step_01a,table(country,useNA = "ifany"))
t_01 <- data.frame(t_01)
t_01$step <- 1
# t_01$total <- sum(t_01$Freq)
t_01

# Step 2: must exit unemployment ----
df_step_02 <- df_step_01 %>%
        # filter(country == "AU") %>%
        filter(exit_unmp_yes == 1)

df_step_02 %>% select(country,pid, pidseq, year, unmp,temp,perm,event_u_p_yes, event_u_t_yes) %>% filter(pid==100153 & country == "AU")

df_step_02a <- df_step_02 %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup()

t_02 <- with(df_step_02a,table(country,useNA = "ifany"))
t_02 <- data.frame(t_02)
t_02$step <- 2
t_02

# Step 3: must be observable after treatment within 5 years ----
# This is really only an issue in Australia, where about 1/3 of cases are not observed in an temporary or permanent contract after unemployment
df_step_03 <- df_step_02 %>%
        filter(event_u_p_yes == 1 | event_u_t_yes == 1)

df_step_03 %>% select(pid, pidseq, year, unmp,temp,perm,event_u_p_yes, event_u_t_yes) %>% filter(pid==5202)

df_step_03a <- df_step_03 %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup()

t_03 <- with(df_step_03a,table(country,useNA = "ifany"))
t_03 <- data.frame(t_03)
t_03$step <- 3
# t_03$total <- sum(t_03$Freq)
t_03

# Step 4: must be employed after treatment within 5 years ----
df_step_04 <- df_step_03 %>%
        filter(event_u_p_yes_final == 1 | event_u_t_yes_final == 1)

df_step_04a <- df_step_04 %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup()

t_04 <- with(subset(df_step_04a),table(country,useNA = "ifany"))
t_04 <- data.frame(t_04)
t_04$step <- 4
# t_04$total <- sum(t_04$Freq)
t_04

# Step 5: identify event, i.e. transition from U to T ----
df_step_05_u_t <- df_step_04 %>%
        filter(event_u_t_yes_final == 1)

df_step_05_u_t <- df_step_05_u_t %>%
        group_by(country, pid, pidseq) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, pid) %>%
        mutate(u_t = 1)

t_05_u_t <- with(df_step_05_u_t,table(country,useNA = "ifany"))
t_05_u_t <- data.frame(t_05_u_t)
t_05_u_t$step <- 5
# t_05_u_t$total <- sum(t_05_u_t$Freq)
t_05_u_t

# Step 6: identify event, i.e. transition from U to P ----
df_step_06_u_p <- df_step_04 %>%
        filter(event_u_p_yes_final == 1)

df_step_06_u_p <- df_step_06_u_p %>%
        group_by(country, pid, pidseq) %>%
        slice(1) %>%
        group_by(country, pid) %>%
        slice(1) %>%
        ungroup() %>%
        select(country, pid) %>%
        mutate(u_p = 1)

t_06_u_p <- with(df_step_06_u_p,table(country,useNA = "ifany"))
t_06_u_p <- data.frame(t_06_u_p)
t_06_u_p$step <- 6
# t_06_u_p$total <- sum(t_06_u_p$Freq)
t_06_u_p

# Step 7: identify the number of observations who transition from U to P and U to T ----

df_step_07 <- merge(df_step_05_u_t,df_step_06_u_p,all = TRUE) %>%
        filter(u_t == 1 & u_p == 1)
df_step_07
t_07 <- with(df_step_07,table(country,useNA = "ifany"))
t_07 <- data.frame(t_07)
t_07$step <- 7
# t_07$total <- sum(t_07$Freq)
t_07

# summarise selection criteria ----

df_table <- rbind(t_01,t_02,t_04) # don't include t_03
df_table$event <- "NA"
df_table

t_05_u_t$event <- "u_t"
t_06_u_p$event <- "u_p"
t_07$event <- "both"

df_table <- rbind(df_table,t_05_u_t,t_06_u_p,t_07) %>%
        mutate(step = ifelse(step>3, yes = step - 1, no = step)) %>%
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

df_append

df_table_wide <- df_append %>%
        select(-country) %>%
        arrange(country_name,step) %>%
        group_by(country_name) %>%
        mutate(row = row_number(),
               obs_diff = paste0(round((Freq/lag(Freq,1)-1)*100,0),"\\%"),
               obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
               obs_diff = ifelse(event == "u_t", yes = paste0(round((Freq/lag(Freq,1))*100,0),"\\%"),
                                 ifelse(event == "u_p", yes = paste0(round((Freq/lag(Freq,2))*100,0),"\\%"),
                                        ifelse(event == "both", yes = NA,
                                               no = obs_diff
                                 )))
        ) %>%
        pivot_wider(names_from = c("country_name"),
                    values_from = c("Freq", "obs_diff"),
        ) %>%
        ungroup()

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


df_table_wide <- df_table_wide %>%
        mutate(event = ifelse(step == 1, yes = "Total unemployment events (From data set A)",
                              ifelse(step == 2, yes = "Must exit unemployment",
                                     ifelse(step == 3, yes = "Employed at least 1 period after exit (within 5 years)",
                                            ifelse(step == 4 & event == "u_t", yes = "Unmp $\\rightarrow$ temp",
                                                   ifelse(step == 5 & event == "u_p", yes = "Unmp $\\rightarrow$ perm",
                                                          ifelse(step == 6, yes = "U $\\rightarrow$ P \\& U $\\rightarrow$ T",
                                                                 no = NA)))))))

df_table_wide

# Table -----------------------------------------

df_output <- df_table_wide %>%
        mutate(step = ifelse(step>3, yes = NA, no = step))

# VARIABLE LABLES

columns_header_top <- c("
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
\\multicolumn{20}{l}{{\\bf Panel B:} Frequency, by event type} \\\\ \n
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

hline_top <- c("\\toprule \n")
hline_bot <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{20}{p{12in}}{Notes: 
               In Panel A: n - is unique observations and $\\Delta$ - is difference in n from previous step.  
               In Panel B:                
               \\# - is number of transitions.  
               \\% - is percent of total transitions from step 3. 
               \\% is more than 100\\% because some individuals experience both a transition from Unmp to Perm and Unmp to Temp.
               } \n")

t <- xtable(df_output, digits = 0)

align(t) <- c("l", "l", 
              ">{\\raggedright\\arraybackslash}p{2in}", # notes
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
              pos = list(0,0,0,3,6),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_bot_1,
                          columns_header_bot_2,
                          hline_bot)),
      comment = FALSE
)

t

beep()
