# --------------------------------------------------------------
# TOP COMMANDS
# About this dofile:
# 1 Extracts contract type from annual person data ($p) and gap data ($pluecke)
# 2 Combines this information in one data set (contract.rds)  
# --------------------------------------------------------------
# Top commands --------------------------------------------------------------
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

soep_raw_data = "data/DE_SOEP/raw_data/R/"

# PACKAGES
# install.packages("dplyr")
# install.packages("readstata13")
# install.packages("beepr")

# LIBRARY
library(tidyverse)
library(reshape2)
library(beepr)

options(scipen = 999) # disable scientific notation

# --------------------------------------------------------------
# PREPARE CODES FOR YEAR (NUMBER + LETTERS)
# Duration Of Work Contract
# --------------------------------------------------------------

letters_seq <- c(letters, sapply(letters, function(x) paste0(x, letters)))

# 1991 - 1996
letters_seq1 <- letters_seq[c(8:13)]
years1 <- c(1991:1996)
vars1 = c("hp32g", "ip32g","jp32g","kp44","lp42", "mp40")

# 1997 - 2017
letters_seq2 <- letters_seq[c(14:26)]
letters_seq2 <- c(letters_seq2,"ba","bb","bc","bd","be","bf","bg","bh")
years2 <- c(1997:2017)
vars2 = c("np3401", "op3401", "pp3701", "qp3501", "rp3901", "sp3901", "tp6501", "up3601", "vp41", "wp34", "xp45", "yp44", "zp40", "bap36", "bbp40", "bcp38", "bdp49", "bep38", "bfp61", "bgp57", "bhp_60")

rm(letters_seq)

# 1 Extract contract type from annual person data (and gap data) --------------------------------------------------------------

# 1991 - 1996
for(i in seq_along(letters_seq1)) {
        print(years1[i])
        print(letters_seq1[i])
        # read
        df_x <- readRDS(paste0(soep_raw_data,letters_seq1[i],"p.rds"))
        df_y <- readRDS(paste0(soep_raw_data,letters_seq1[i],"pluecke.rds"))
        # select
        df_x <- select(df_x, hhnr, persnr, syear, vars1[i])
        df_y <- select(df_y, hhnr, persnr, syear, vars1[i])
        # append
        df_z <- rbind(df_x,df_y)
        colnames(df_z)[colnames(df_z) == paste0(vars1[i])] <- "contyp" # rename contract type variable
        df_z <- select(df_z, hhnr, persnr, syear, contyp) %>%
                rename(hid = hhnr,
                       pid = persnr)
        assign(paste0("df_year_",years1[i]), df_z)
}
rm(df_x,df_y,df_z)

# 1997 - 2017
for(i in seq_along(letters_seq2)) {
        print(years2[i])
        print(letters_seq2[i])
        # read
        df_x <- readRDS(paste0(soep_raw_data,letters_seq2[i],"p.rds"))
        df_x$syear <- years2[i]
        colnames(df_x)[colnames(df_x) == paste0(vars2[i])] <- "contyp" # rename contract type variable
        df_x <- select(df_x, hhnr, persnr, syear, contyp) %>%
                rename(hid = hhnr,
                       pid = persnr)
        assign(paste0("df_year_",years2[i]), df_x)
}
rm(df_x)
rm(letters_seq1,letters_seq2,vars1,vars2,years1,years2,i)

# --------------------------------------------------------------
# 2 Combine in one data set (and delete auxiliary data)
# In 2006, the values for fixed-term and permanent contracts were switched!
# In 2000, category 3 changed from 'self-employed' to 'no contract'
# --------------------------------------------------------------

# Combine in one data set
df_contyp = data.frame()
for(c in 1992:2017) {
        c
        df_x <- get(paste0("df_year_", c))
        df_contyp <- rbind(df_contyp,df_x)
        rm(df_x)
}

# Delete auxiliary data
for(c in 1992:2017) {
  rm(list=paste0("df_year_", c))
}


# Graph pre recode
df_graph <- group_by(df_contyp, syear) %>%
        filter(contyp > 0) %>% 
        mutate(total = n()) %>%
        group_by(syear, contyp) %>%
        mutate(n = n(), rel.freq = n / total) %>%
        summarise(pct = mean(rel.freq)) %>%
        ungroup() %>%
        arrange(contyp, syear)
df_graph_table <- dcast(df_graph, formula = syear ~ contyp, value.var = "pct")
ggplot(data = df_graph, aes(x = syear, y = pct, group = contyp, color = as.factor(contyp))) +
        geom_line(size = 2)

# recode values for years after 2005
df_contyp <- mutate(df_contyp, contyp2 = ifelse(syear <= 2005, yes = contyp,
                                     ifelse(syear > 2005 & contyp == 1, yes = 2,
                                            ifelse(syear > 2005 & contyp == 2, yes = 1, no = contyp))))

# Graph post recode
df_graph <- group_by(df_contyp, syear) %>%
  filter(contyp > 0) %>% 
  mutate(total = n()) %>%
  group_by(syear, contyp2) %>%
  mutate(n = n(), rel.freq = n / total) %>%
  summarise(pct = mean(rel.freq)) %>%
  ungroup() %>%
  arrange(contyp2, syear)
df_graph_table <- reshape2::dcast(df_graph, formula = syear ~ contyp2, value.var = "pct")

ggplot(data = df_graph, aes(x = syear, y = pct, group = contyp2, color = as.factor(contyp2))) +
  geom_line(size = 2)

df_contyp <- select(df_contyp, -contyp) %>%
        rename(contyp = contyp2)

# Save data sets --------------------------------------------------------------
# saveRDS(df_contyp, file = paste0(data_files, "contyp.rds"))
beep()

