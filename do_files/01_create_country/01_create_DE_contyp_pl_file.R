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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

soep_raw_data = "data_files/DE_SOEP/raw_data/"
data_files = "data_files/DE/"

# PACKAGES

# LIBRARY
library(tidyverse)
library(data.table) # faster merging

options(scipen = 999) # disable scientific notation

# LOAD DATA --------------------------------------------------------------

# harmonized contract type
# df_pl <- readRDS(file = paste0(soep_raw_data,"pl.rds"))
# df_contyp <- select(df_pl, pid, hid, syear, plb0037_h) %>%
#         rename(contyp = plb0037_h)
# saveRDS(df_contyp, file = paste0(soep_raw_data, "pl_contyp.rds"))
df_contyp <- readRDS(file = paste0(soep_raw_data,"pl_contyp.rds"))


# Graph pre recode
df_graph <- group_by(df_contyp, syear) %>%
        filter(contyp > 0) %>% 
        mutate(total = n()) %>%
        group_by(syear, contyp) %>%
        mutate(n = n(), rel.freq = n / total) %>%
        summarise(pct = mean(rel.freq)) %>%
        ungroup() %>%
        arrange(contyp, syear)
ggplot(data = df_graph, aes(x = syear, y = pct, group = contyp, color = as.factor(contyp))) +
        geom_line(size = 2)
