# TOP COMMANDS -----
# https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/index/
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package,character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

# FOLDERS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/")

data_files = "data_files/"
results = "results/"

# LIBRARY
library(tidyverse)
library(texreg)
library(beepr)
library(feisr)
library(broom)
library(broomExtra)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}


# load data -----

df_original <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_original <- df_original %>%
        filter(country == "IT")

# create sample ----

set.seed(1234)

df_sample <- df_original %>%
        select(country,pid) %>%
        group_by(country,pid) %>%
        slice(1) %>%
        ungroup() %>%
        sample_frac(.2,replace = TRUE) %>%
        mutate(sample = 1)
df_original <- merge(df_original,df_sample,by = c("country", "pid")) %>%
        filter(sample == 1) %>%
        arrange(country,pid,year)

# clean data -----


df_original <- df_original %>%
        filter(unmp == 0)  %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2)

# if treated, then must be employed after treatment
# if not treated, then must be employed

# Temp to perm
df_event_t_p <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_t_p"),unemployment_rate) %>%
        filter(event_t_p_yes == 1 | (event_t_p_yes == 1 & event_t_p_drop == 0)) %>%
        mutate(event_t_p_time = ifelse(event_t_p_time < -2, yes = -3,
                                       ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
               event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
        ) %>%
        rename(event_time_pos = event_t_p_time_pos)
# 
# df_event_t_p_treat_y <- df_event_t_p %>%
#         filter(event_t_p_yes == 1 & event_t_p_drop == 0)
# 
# df_event_t_p_treat_n <- df_event_t_p %>%
#         filter(event_t_p_yes == 0 & unmp == 0)
# 
# df_event_t_p <- bind_rows(df_event_t_p_treat_y,df_event_t_p_treat_n)
# rm(df_event_t_p_treat_y,df_event_t_p_treat_n)

# Perm to temp
df_event_p_t <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_p_t"),unemployment_rate) %>%
        filter(event_p_t_yes == 0 | event_p_t_yes == 1 & event_p_t_drop == 0) %>%
        mutate(event_p_t_time = ifelse(event_p_t_time < -2, yes = -3,
                                       ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
               event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
        ) %>%
        rename(event_time_pos = event_p_t_time_pos)
# 
# df_event_p_t_treat_y <- df_event_p_t %>%
#         filter(event_p_t_yes == 1 & event_p_t_drop == 0)
# 
# df_event_p_t_treat_n <- df_event_p_t %>%
#         filter(event_p_t_yes == 0 & unmp == 0)
# 
# df_event_p_t <- bind_rows(df_event_p_t_treat_y,df_event_p_t_treat_n)
# rm(df_event_p_t_treat_y,df_event_p_t_treat_n)

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

# FE Models ---- 

# FE model
model_fe <- feis(ln_hourly_wage ~ age + unemployment_rate + temp + factor(year) | 1,
                 data = data.frame(df_original), 
                 robust = TRUE,
                 id = "pid")

df_output <- tidy_parameters(model_fe)
df_output$event <- "FE"
df_yhat <- rbind(df_yhat,df_output)

# FEIS model
model_feis <- feis(ln_hourly_wage ~ age + unemployment_rate + temp + factor(year) | year,
                   data = data.frame(df_original), 
                   robust = TRUE,
                   id = "pid")

df_output <- tidy_parameters(model_feis)
df_output$event <- "FEIS"
df_yhat <- rbind(df_yhat,df_output)


# Dummy IF Models ---- 

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
df_event_t_p$event_time_pos <- relevel(factor(df_event_t_p$event_time_pos), ref = "0")
model_event_t_p <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
              data = data.frame(df_event_t_p), 
              robust = TRUE,
              id = "pid")

df_output <- tidy_parameters(model_event_t_p)
df_output$event <- "t_p"
df_yhat <- rbind(df_yhat,df_output)

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
df_event_p_t$event_time_pos <- relevel(factor(df_event_p_t$event_time_pos), ref = "0")
model_event_p_t <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                        data = data.frame(df_event_p_t), 
                        robust = TRUE,
                        id = "pid")

df_output <- tidy_parameters(model_event_p_t)
df_output$event <- "p_t"
df_yhat <- rbind(df_yhat,df_output)

# Graph post event ----

# Prepare data 

df_yhat_2 <- df_yhat %>%
        filter(str_detect(term, 'event_time'))

df_yhat_2$post <- as.numeric(as.character(RIGHT(df_yhat_2$term,1)))

df_yhat_2 <- df_yhat_2 %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,post)

df_graph <- df_yhat_2 %>%
        # filter(post>=0) %>%
        filter(event=="p_t" | event == "t_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("t_p","p_t"),
                         labels = c("Temp to Perm",
                                    "Perm to Temp"))

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        # facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        scale_y_continuous(limits = c(-.3,.3)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Estimate") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


# Table ----

screenreg(
        list(model_fe, model_feis, model_event_t_p, model_event_p_t),
        custom.model.names = c("FE", "FEIS", "Temp to perm", "Perm to temp"),
        table = FALSE,
        custom.coef.map = list(
                "temp"="temp",
                "event_time_pos1"="Pre event (-2)",
                "event_time_pos2"="Pre event (-1)",
                "event_time_pos3"="event",
                "event_time_pos4"="Post event (+1)",
                "event_time_pos5"="Post event (+2)",
                "event_time_pos6"="Post event (+3)",
                "event_time_pos7"="Post event (+4)"
        ),
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept|position"),
)


