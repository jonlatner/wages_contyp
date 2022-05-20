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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/booth_etal_2002/data_files/update/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)

options(scipen = 999) # disable scientific notation

# load data -----

df_uk <- readRDS(paste0(data_files, "bhps.rds")) 
# df_uk <- df_uk %>%
#         filter(year < 2000)

df_uk <- df_uk %>%
        filter(year >= 2000)

# df_uk <- df_uk %>%
#         filter(emp == 1)

# View(filter(df_uk,pid == 409956365) %>% select(pid, year, contyp, matches("ftc")))

# clean data -----

# df_dummies <- dummy(df_uk$pre_ftc, sep = "_")
# df_dummies <- df_dummies[, -c(1)]
# df_uk <- cbind(df_uk, df_dummies)

# df_dummies <- dummy(df_uk$post_ftc, sep = "_")
# df_dummies <- df_dummies[, -c(1)]
# df_uk <- cbind(df_uk, df_dummies)

df_m <- df_uk %>%
        filter(male == 1)

df_f <- df_uk %>%
        filter(male == 0)

df_unique <- df_uk %>%
        group_by(pid) %>%
        filter(row_number()==1) %>%
        ungroup()

# plm model -----

iv <- "unmp + as.factor(pre_ftc) + ftc + as.factor(post_ftc) + tmp + age + age_2 + factor(year)"

# iv <- "unmp + tmp*poly(age,2,raw=TRUE) + ftc*poly(age,2,raw=TRUE) + factor(year)"

# plm_model_m <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_m, age >= 20),
#                    index = c("pid","year"))
# plm_model_f <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_f, age >= 20),
#                    index = c("pid","year"))
# plm_model_m <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_m, age < 55),
#                    index = c("pid","year"))
# plm_model_f <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_f, age < 55),
#                    index = c("pid","year"))
# plm_model_m <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = df_m,
#                    index = c("pid","year"))
# plm_model_f <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = df_f,
#                    index = c("pid","year"))
# plm_model_m <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_m, age > 24 & age < 55),
#                    index = c("pid","year"))
# plm_model_f <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_f, age > 24 & age < 55),
#                    index = c("pid","year"))
# stargazer(plm_model_m, plm_model_f, type = "text", omit = ("year"))

# plm_model_m <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
#                    data = subset(df_m, age > 24 & age < 55),
#                    index = c("pid","year"))
# plm_model_f <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
#                    data = subset(df_f, age > 24 & age < 55),
#                    index = c("pid","year"))
# stargazer(plm_model_m, plm_model_f, type = "text", omit = ("year"))

# yhat_m <- predict(plm_model_m)
# df_m <- cbind(df_m,yhat_m)
# yhat_f <- predict(plm_model_f)
# df_f <- cbind(df_f,yhat_f)

# View(filter(df_f,pid == 409956365) %>% select(pid, year, contyp, ln_wages, pre_ftc, ftc, post_ftc, yhat_f))


# plm_model_m <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_m, age > 24 & age < 55),
#                    index = c("pid","year"))
# 
# plm_model_f <- plm(as.formula(paste0("ln_wages ~ ",iv)),
#                    data = subset(df_f, age > 24 & age < 55),
#                    index = c("pid","year"))

# plm model -----

iv <- "unmp + as.factor(pre_ftc) + ftc + as.factor(post_ftc) + tmp + age + age_2 + factor(year)"

plm_model_m_ann <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                   data = df_m,
                   index = c("pid","year"))
plm_model_f_ann <- plm(as.formula(paste0("ln_wages ~ ",iv)),
                   data = df_f,
                   index = c("pid","year"))

plm_model_m_hrly <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
                   data = df_m,
                   index = c("pid","year"))
plm_model_f_hrly <- plm(as.formula(paste0("ln_hourly_wage ~ ",iv)),
                   data = df_f,
                   index = c("pid","year"))

stargazer(plm_model_m_ann, plm_model_f_ann, plm_model_m_hrly, plm_model_f_hrly, type = "text", omit = ("year"))

plm_model_m_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_m,
                        index = c("pid","year"))
plm_model_f_status <- plm(as.formula(paste0("prestige ~ ",iv)),
                        data = df_f,
                        index = c("pid","year"))
stargazer(plm_model_m_status, plm_model_f_status, type = "text", omit = ("year"))

