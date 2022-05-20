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
        filter(country == "DE") %>%
        arrange(country, pid, year)

# create sample ----

set.seed(1234)

df_sample <- df_original %>%
        select(country,pid) %>%
        group_by(country,pid) %>%
        slice(1) %>%
        ungroup() %>%
        sample_frac(.1,replace = FALSE) %>%
        mutate(sample = 1) %>%
        arrange(country,pid)

df_original <- merge(df_original,df_sample,by = c("country", "pid")) %>%
        filter(sample == 1) %>%
        arrange(country, pid, year)

# clean data -----

# if treated, then must be employed after treatment
# if not treated, then must be employed and present for at least 3 observations

# Unmp to perm
df_event_u_p <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_u_p"),unemployment_rate) %>%
        mutate(event_u_p_time = ifelse(event_u_p_time < -2, yes = -2,
                                       ifelse(event_u_p_time > 4, yes = 4, no = event_u_p_time)),
               event_u_p_time_pos = ifelse(event_u_p_yes == 1, yes = event_u_p_time + 2, no = 0),
        ) %>%
        rename(event_time_pos = event_u_p_time_pos)

df_event_u_p %>%
        group_by(event_u_p_yes,event_time_pos) %>%
        summarise(mean=mean(ln_hourly_wage)) %>%
        ungroup()

df_event_u_p_treat_y <- df_event_u_p %>%
        filter(event_u_p_yes == 1 & event_u_p_drop == 0) %>%
        mutate(event_time_pos = ifelse(event_time_pos<1, yes = 0, no = event_time_pos))

df_event_u_p_treat_y %>%
        group_by(event_time_pos) %>%
        summarise(mean=mean(ln_hourly_wage)) %>%
        ungroup()

df_event_u_p_treat_n <- df_event_u_p %>%
        filter(event_u_p_yes == 0 & unmp == 0)

df_event_u_p <- bind_rows(df_event_u_p_treat_y,df_event_u_p_treat_n)
rm(df_event_u_p_treat_y,df_event_u_p_treat_n)

df_event_u_p <- df_event_u_p %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2)

table(df_event_u_p$max)

# Unmp to temp
df_event_u_t <- df_original %>%
        select(country,pid,year,age,ln_hourly_wage,unmp,temp,perm,matches("event_u_t"),unemployment_rate) %>%
        mutate(event_u_t_time = ifelse(event_u_t_time < -2, yes = -2,
                                       ifelse(event_u_t_time > 4, yes = 4, no = event_u_t_time)),
               event_u_t_time_pos = ifelse(event_u_t_yes == 1, yes = event_u_t_time + 3, no = 0),
        ) %>%
        rename(event_time_pos = event_u_t_time_pos)


df_event_u_t_treat_y <- df_event_u_t %>%
        filter(event_u_t_yes == 1) %>%
        filter(event_u_t_yes == 1 & event_u_t_drop == 0) %>%
        mutate(event_time_pos = ifelse(event_time_pos<1, yes = 0, no = event_time_pos))

df_event_u_t_treat_n <- df_event_u_t %>%
        filter(event_u_t_yes == 0 & unmp == 0)

df_event_u_t <- bind_rows(df_event_u_t_treat_y,df_event_u_t_treat_n)
rm(df_event_u_t_treat_y,df_event_u_t_treat_n)

df_event_u_t <- df_event_u_t %>%
        group_by(country, pid) %>%
        mutate(number = row_number(),
               max = max(number)) %>%
        ungroup() %>%
        filter(max>2)

table(df_event_u_t$max)

# Prepare for models ----

df_yhat <- data.frame()

# Events - Temp to Perm (t_p), Perm to Temp (p_t), Unmp to Perm (u_p), and Unmp to Temp (p_t)

# FE Models ---- 

# Dummy IF Models ---- 

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
# df_event_u_p$event_time_pos <- relevel(factor(df_event_u_p$event_time_pos), ref = "2")
df_event_u_p$event_time_pos <- factor(df_event_u_p$event_time_pos)
model_event_u_p <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                        data = data.frame(df_event_u_p), 
                        robust = TRUE,
                        id = "pid")

df_output <- tidy_parameters(model_event_u_p)
df_output$event <- "u_p"
df_yhat <- rbind(df_yhat,df_output)

# model - baseline is 1 period before event.  in annual data this is event_time_pos==2
# df_event_u_t$event_time_pos <- relevel(factor(df_event_u_t$event_time_pos), ref = "2")
df_event_u_t$event_time_pos <- factor(df_event_u_t$event_time_pos)
model_event_u_t <- feis(ln_hourly_wage ~ age + unemployment_rate + event_time_pos + as.factor(year) | 1,
                        data = data.frame(df_event_u_t), 
                        robust = TRUE,
                        id = "pid")

df_output <- tidy_parameters(model_event_u_t)
df_output$event <- "u_t"
df_yhat <- rbind(df_yhat,df_output)


# Graph post event ----

df_yhat_2 <- df_yhat %>%
        filter(str_detect(term, 'event_time'))

df_yhat_2$post <- as.numeric(as.character(RIGHT(df_yhat_2$term,1)))

df_yhat_2 <- df_yhat_2 %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,post)

df_graph <- df_yhat_2 %>%
        filter(post>=0) %>%
        filter(event=="u_t" | event == "u_p")

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("Unmp to Perm",
                                    "Unmp to Temp"))

ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        # facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        # geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        # scale_y_continuous(limits = c(-.3,.3)) +
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
        list(model_event_u_p, model_event_u_t),
        custom.model.names = c("Unmp to perm", "Unmp to temp"),
        table = FALSE,
        # custom.coef.map = list(
        #         "event_time_pos3"="event",
        #         "event_time_pos1"="Pre event (-2)",
        #         "event_time_pos2"="Pre event (-1)",
        #         "event_time_pos4"="Post event (+1)",
        #         "event_time_pos5"="Post event (+2)",
        #         "event_time_pos6"="Post event (+3)",
        #         "event_time_pos7"="Post event (+4)"
        # ),
        include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept|position"),
)


# beep()

