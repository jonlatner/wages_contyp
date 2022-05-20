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

raw_data = "data/CH_FORS/raw_data/R/"
data_files = "projects/mobility/data_files/CH/"

# LIBRARY
library(dplyr)

options(scipen = 999) # disable scientific notation

# Variables ----

years <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18")
years_1 <- c("00", "01")
years_2 <- c("02", "03")
years_3 <- c("04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18")

vars_ind_1 <- c("idpers", # "idpers" cross-wave person identifier
                "isced", # "iscedXX" ISCED levels (education)
                "wstat", # "wstatXX" Labor force status
                "occupa", # "ocupaXX" Employment status - Full-time/part-time
                "pw36", # "pXXW36"  Duration - is your job limited in time?
                "pw37", # "pXXW37"  Contract type - What type of contract limited by time do you have?
                "age", # "ageXX" year of birth
                "sex", # "sexXX" gender
                "pw74", # N contractual hours per week
                "is4maj", # "is4maj" occupation (4 digit ISCO)
                "tr1maj", # "tr1maj" Treiman prestige scale 1 for main job
                "iwmg" # "iwmg" monthly income from employment: gross (1999 - 2001)
                )

vars_ind_2 <- c("idpers", # "idpers" cross-wave person identifier
                "isced", # "iscedXX" ISCED levels (education)
                "wstat", # "wstatXX" Labor force status
                "occupa", # "ocupaXX" Employment status - Full-time/part-time
                "pw36", # "pXXW36"  Duration - is your job limited in time?
                "pw37", # "pXXW37"  Contract type - What type of contract limited by time do you have?
                "age", # "ageXX" year of birth
                "sex", # "sexXX" gender
                "pw74", # N contractual hours per week
                "is4maj", # "is4maj" occupation (4 digit ISCO)
                "tr1maj", # "tr1maj" Treiman prestige scale 1 for main job
                "iempmg" # "iempmg" monthly income from employment: gross (2002 - )
)

vars_ind_3 <- c("idpers", # "idpers" cross-wave person identifier
                "isced", # "iscedXX" ISCED levels (education)
                "wstat", # "wstatXX" Labor force status
                "occupa", # "ocupaXX" Employment status - Full-time/part-time
                "pw36", # "pXXW36"  Duration - is your job limited in time?
                "pw37", # "pXXW37"  Contract type - What type of contract limited by time do you have?
                "age", # "ageXX" year of birth
                "sex", # "sexXX" gender
                "pw74", # N contractual hours per week
                "is4maj", # "is4maj" occupation (4 digit ISCO)
                "tr1maj", # "tr1maj" Treiman prestige scale 1 for main job
                "iempmg" # "iempmg" monthly income from employment: gross (2002 - )
)

# Load covars from FORS files (individual) ----

# x <- readRDS(file = paste0(raw_data,"shp",11,"_p_user.rds"))
# select(x,idpers,matches("emp"),matches("I11ptot")) %>% filter(idpers == 10776101)
# df_test <- filter(x, idpers == 10776101)

year <- c(1999)
for (y in seq_along(years_1)) {
        print(years_1[y])
        year <- year + 1
        x <- readRDS(file = paste0(raw_data,"shp",years_1[y],"_p_user.rds"))
        colnames(x) <- gsub(years_1[y], "", colnames(x)) # remove year identifier
        x <- unique.matrix(x, MARGIN=2) # remove duplicate column names created by removing XX (this appears to happen in 2012, not sure why)
        x <- select(x, all_of(vars_ind_1))
        x <- rename(x, wages = iwmg)
        x$year <- year
        assign(paste0("df_ind_year_",years_1[y]), x)
        rm(x)
}


year <- c(2001)
for (y in seq_along(years_2)) {
        print(years_2[y])
        year <- year + 1
        x <- readRDS(file = paste0(raw_data,"shp",years_2[y],"_p_user.rds"))
        colnames(x) <- gsub(years_2[y], "", colnames(x)) # remove year identifier
        x <- unique.matrix(x, MARGIN=2) # remove duplicate column names created by removing XX (this appears to happen in 2012, not sure why)
        x <- select(x, all_of(vars_ind_2))
        x <- rename(x, wages = iempmg)
        x$year <- year
        assign(paste0("df_ind_year_",years_2[y]), x)
        rm(x)
}

year <- c(2003)
for (y in seq_along(years_3)) {
        print(years_3[y])
        year <- year + 1
        x <- readRDS(file = paste0(raw_data,"shp",years_3[y],"_p_user.rds"))
        colnames(x) <- gsub(years_3[y], "", colnames(x)) # remove year identifier
        x <- unique.matrix(x, MARGIN=2) # remove duplicate column names created by removing XX (this appears to happen in 2012, not sure why)
        x <- select(x, all_of(vars_ind_3))
        x <- rename(x, wages = iempmg)
        x$year <- year
        assign(paste0("df_ind_year_",years_3[y]), x)
        rm(x)
}

# append
df_fors_ind = data.frame()
for (y in seq_along(years)) {
        y <- get(paste0("df_ind_year_", years[y]))
        df_fors_ind <- rbind(df_fors_ind,y)
        rm(y)
}

rm(list=ls(pattern="^df_ind_year")) # remove

# Save data sets ----

saveRDS(df_fors_ind, file = paste0(data_files, "covars.rds"))
