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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

raw_data = "panel_data/NE_LSP/raw_data/R/"
data_files = "projects/mobility/data_files/NE/LSP/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight
# no occupation or prestige variables

# Variables
vars <- c("koppelnr", # pid
          "aa001", # gender
          "aa003", # birth year
          "ba016", # education
          "ca001", # employment status
          "eb002", # contract type
          "ea104", # hours per week, according to work contract
          "db001" # wages
)

year <- seq(2000,2014,2)
waves <- c("_00", "_02", "_04", "_06", "_08", "_10", "_12", "_14")

# Data
for (i in seq_along(year)) {
        print(waves[i])
        x <- readRDS(file = paste0(raw_data,"Arbeidsaanbodpanel_",year[i],".rds"))
        colnames(x) <- gsub(waves[i], "", colnames(x)) # remove year identifier
        x <- select(x, all_of(vars))
        x$year <- year[i]
        assign(paste0("df_wave_",year[i]), x)
        rm(x)
}

# append
df_liss = data.frame()
for (i in seq_along(year)) {
        y <- get(paste0("df_wave_", year[i]))
        df_liss <- bind_rows(df_liss,y)
        rm(y)
}

rm(list=ls(pattern="^df_wave")) # remove
rm(i,vars,waves)

# rename
df_liss <- df_liss %>%
        rename(pid=koppelnr,
               gender=aa001,
               birth_year=aa003,
               edu=ba016,
               contyp=eb002,
               hours=ea104,
               wages=db001,
               empstat=ca001
        )

summary(df_liss)

# Save data sets ----

saveRDS(df_liss, file = paste0(data_files, "covars.rds"))
