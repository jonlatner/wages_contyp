# Top commands ----
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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/")
# setwd("C:/Users/ba1ks6/OneDrive/SECCOPA/")

raw_data = "data/NE_LISS/raw_data/R/"
data_files = "projects/mobility/data_files/NE/LISS/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight
# no occupation or prestige variables

# Background variables ----

# Data
df_background_1 <- read.csv(file = paste0(raw_data,"background/data_1.csv"), sep = ";")
df_background_2 <- read.csv(file = paste0(raw_data,"background/data_2.csv"), sep = ";")
df_background_3 <- read.csv(file = paste0(raw_data,"background/data_3.csv"), sep = ";")
df_background_4 <- read.csv(file = paste0(raw_data,"background/data_4.csv"), sep = ";")

df_background <- rbind(df_background_1,df_background_2,df_background_3,df_background_4)
rm(df_background_1,df_background_2,df_background_3,df_background_4)

df_background <- df_background %>%
        arrange(nomem_encr,wave) %>%
        select(nomem_encr,geslacht) %>%
        rename(pid=nomem_encr,
               gender=geslacht,
               )
df_background <- unique(df_background)
# drop duplicates
filter(df_background,pid==866693)
pid <- df_background$pid
df_background$duplicated <- duplicated(pid)
df_background <- df_background %>%
        group_by(pid) %>%
        mutate(duplicated = last(duplicated)) %>%
        ungroup() %>%
        filter(duplicated==FALSE) %>%
        select(-duplicated)

# work and schooling ----

waves <- c("cw08a","cw09b","cw10c","cw11d","cw12e","cw13f","cw14g","cw15h","cw16i","cw17j","cw18k","cw19l")

# Variables
vars <- c("nomem_encr", # pid
          "_m", # year month of survey
          "002", # birth year
          "005", # education
          "121", # employment status - contract type/self-employed
          "126", # hours per week, according to work contract
          "088", # paid work
          "089", # unemployed
          # "090", # employed, but unpaid
          # "091", # unemployed, but looking for work
          # "092", # unemployed, but not looking for work
          # "093", # first time job seeker
          # "094", # unemployed, but looking for work
          "095", # student
          "096", # home maker
          "097", # not working - live off private means
          "098", # early retirement
          "099", # pensioner
          "100", # disabled
          # "101", # voluntary
          # "102", # employed, but looking for work
          "104" # labor force status
)

# Data
for (i in seq_along(waves)) {
        print(waves[i])
        x <- readRDS(file = paste0(raw_data,"work_schooling/",waves[i],"_EN.rds"))
        colnames(x) <- gsub(waves[i], "", colnames(x)) # remove year identifier
        x <- select(x, all_of(vars))
        assign(paste0("df_wave_",waves[i]), x)
        rm(x)
}

# append
df_work_schooling = data.frame()
for (i in seq_along(waves)) {
        y <- get(paste0("df_wave_", waves[i]))
        df_work_schooling <- bind_rows(df_work_schooling,y)
        rm(y)
}

rm(list=ls(pattern="^df_wave")) # remove
rm(i,vars,waves)

# rename
df_work_schooling <- df_work_schooling %>%
        rename(year='_m',
               pid=nomem_encr,
               birth_year='002',
               edu='005',
               work_088_emp='088',
               work_089_unemp='089',
               # work_090='090',
               # work_091='091',
               # work_092='092',
               # work_093='093',
               # work_094='094',
               work_095_nw_student='095',
               work_096_nw_homemaker='096',
               work_097_nw_private='097',
               work_098_nw_retired_early='098',
               work_099_nw_retired='099',
               work_100_nw_disabled='100',
               # work_101_emp_voluntary='101',
               # work_102_emp_looking='102',
               lfs='104',
               hours='126',
               empstat='121'
        )

df_work_schooling <- df_work_schooling %>%
        arrange(pid,year) %>%
        mutate(year=as.numeric(substr(as.character(year),1,4)))

summary(df_work_schooling)

# income ----

waves <- c("ci08a","ci09b","ci10c","ci11d","ci12e","ci13f","ci14g","ci15h","ci16i","ci17j","ci18k","ci19l")
waves_1 <- c("ci08a","ci09b","ci10c","ci11d","ci12e","ci13f","ci14g","ci15h","ci16i","ci17j","ci18k")
waves_2 <- c("ci19l")

# Variables
vars_1 <- c("nomem_encr", # pid
            "_m", # year month of survey
            "010" # How much were your gross wages in total in 2007 at your employer (/at the employer where you earned most in 2007/ (hereafter referred to as employer 1))?
)

vars_2 <- c("nomem_encr", # pid
            "_m", # year month of survey
            "372" # annual gross wages
)

# Data
for (i in seq_along(waves_1)) {
        print(waves_1[i])
        x <- readRDS(file = paste0(raw_data,"income/",waves_1[i],"_EN.rds"))
        colnames(x) <- gsub(waves[i], "", colnames(x)) # remove year identifier
        x <- select(x, all_of(vars_1))
        assign(paste0("df_wave_",waves_1[i]), x)
        rm(x)
}

for (i in seq_along(waves_2)) {
        print(waves_2[i])
        x <- readRDS(file = paste0(raw_data,"income/",waves_2[i],"_EN.rds"))
        colnames(x) <- gsub(waves_2[i], "", colnames(x)) # remove year identifier
        x <- select(x, all_of(vars_2))
        x <- rename(x, '010'='372') 
        assign(paste0("df_wave_",waves_2[i]), x)
        rm(x)
}

# append
df_income = data.frame()
for (i in seq_along(waves)) {
        y <- get(paste0("df_wave_", waves[i]))
        df_income <- bind_rows(df_income,y)
        rm(y)
}

rm(list=ls(pattern="^df_wave")) # remove
rm(i,vars_1,vars_2,waves,waves_1,waves_2)

# rename
df_income <- df_income %>%
        rename(year='_m',
               pid=nomem_encr,
               wages='010'
        )

df_income <- df_income %>%
        arrange(pid,year) %>%
        mutate(year=as.numeric(substr(as.character(year),1,4)))

summary(df_income)

# merge ----

df_liss <- merge(df_income,df_work_schooling,by = c("pid","year"), all.x = TRUE)
df_liss <- merge(df_liss,df_background,by = c("pid"), all.x = TRUE)

summary(df_liss)

# Save data sets ----

saveRDS(df_liss, file = paste0(data_files, "covars.rds"))
