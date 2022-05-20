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

raw_data = "data/IT_SHIW/raw_data/R/"
data_files = "projects/wages_contyp/data_files/IT/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight
# no occupation or prestige variables

# rper (income) ----
# Variables
vars_rper <- c("anno", # year
          "nquest", # household ID
          "nord", # household member ID
          "yl1" # net wages and salaries
)


# Data
df_rper <- readRDS(file = paste0(raw_data,"rper.rds"))
df_rper <- select(df_rper, all_of(vars_rper))

# rename
df_rper <- df_rper %>%
        rename(year=anno,
               hid=nquest,
               pid=nord,
               wages=yl1
        )
summary(df_rper)

# comp (Household member characteristics) ----
# Variables
vars_comp <- c("anno", # year
               "nquest", # household ID
               "nord", # household member ID
               "nordp", # Household  member  ID  in  the  previous  wave 
               "anasc", # year of birth
               "sesso", # gender
               "studio", # education
               "qualp3", # main employment, work status (self-employed)
               "nonoc" # labor force status
)


# Data
df_comp <- readRDS(file = paste0(raw_data,"comp.rds"))
df_comp <- select(df_comp, all_of(vars_comp))

# rename
df_comp <- df_comp %>%
        rename(year=anno,
               hid=nquest,
               pid=nord,
               birth_year=anasc,
               gender=sesso,
               edu=studio,
               lfs=nonoc,
               empstat=qualp3
        )
summary(df_comp)

# ldip (payroll employment) ----
# Variables
vars_ldip <- c("anno", # year
               "mesilav", # months worked
               "nquest", # household ID
               "nord", # household member ID
               "attivp", # type of activity (main/secondary)
               "contratt", # contract type
               "oretot", # total hours per week
               "orestra" # overtime hours per week
)


# Data
df_ldip <- readRDS(file = paste0(raw_data,"ldip.rds"))
df_ldip <- select(df_ldip, all_of(vars_ldip))

# rename
df_ldip <- df_ldip %>%
        rename(year=anno,
               months_worked=mesilav,
               hid=nquest,
               pid=nord,
               activity=attivp,
               contyp=contratt,
               hours_total=oretot,
               hours_overtime=orestra
        )
summary(df_ldip)

# merge ----

# View(filter(df_ldip, hid==24194 & pid == 3  & year == 2012))
# variable mesilov - months worked in a year distinguishes the two observations 

df_shiw <- merge(df_rper,df_ldip,by = c("hid","pid","year"), all.x = TRUE)

df_shiw <- merge(df_shiw,df_comp,by = c("hid","pid","year"), all.x = TRUE)

# Save data sets ----

saveRDS(df_shiw, file = paste0(data_files, "covars.rds"))
