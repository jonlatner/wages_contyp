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

# FOLDERS (ADAPT THIS PATHWAY!)
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

raw_data = "data_files/KO/raw_data/"
data_files = "data_files/KO/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# Variables ----
# pid, year, gender, birth year, occupation, prestige, education, employment status, contract type, hours, wages, cpi, weight
# no occupation or prestige variables

# individual
vars <- c("pid", # cross-wave person identifier
          "p__0104", # year of birth
          "p__0101", # gender
          "p__0110", # education
          "p__0203", # Employment status
          "p__0501", # work contract
          "p__0211", # self-employed
          "p__0350", # occupation
          "p__1006", # hours - regular weekly work hours(wage-earners)
          "p__1642" # average monthly pay
)

# Load covars ----

waves <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

year <- c(1998)
for (y in seq_along(waves)) {
        year <- year + 1
        test <- paste0("p",waves[y])
        x <- readRDS(file = paste0(raw_data,"eklips",waves[y],"p.rds"))
        colnames(x) <- gsub(test, "p__", colnames(x)) # remove year identifier
        x <- select(x, all_of(vars))
        x$year <- year
        assign(paste0("df_year_",waves[y]), x)
        rm(x)
}

# append
df_klips = data.frame()
for (y in seq_along(waves)) {
        y <- get(paste0("df_year_", waves[y]))
        df_klips <- rbind(df_klips,y)
        rm(y)
}

rm(list=ls(pattern="^df_year")) # remove

# label vars ----

df_klips <- df_klips %>%
        rename(gender = p__0101,
               birth_year = p__0104,
               edu = p__0110,
               empstat = p__0203,
               contyp = p__0501,
               emptype = p__0211,
               occ = p__0350,
               hours = p__1006,
               wages = p__1642
        )

# Save data sets ----

saveRDS(df_klips, file = paste0(data_files, "covars.rds"))
