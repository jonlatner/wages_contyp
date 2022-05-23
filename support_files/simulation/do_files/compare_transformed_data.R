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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/wage_mobility/support_files/simulation/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "data_files/"
tables = "tables/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(texreg)
library(feisr)

options(scipen = 9999) # disable scientific notation

# Load data -----

id_1 <- data.frame(
        unmp = c(0,0,0,0,0,0),
        temp = c(1,0,1,0,1,1),
        perm = c(0,1,0,1,0,0),
        ln_hourly_wage = c(50,90,110,130,140,150),
        year = c(1,2,3,4,5,6)
)
id_1$pid = 1
id_1$year_lag = 1
id_1$country = "DE"

df_original <- id_1


# Graph data ----

df_graph <- df_original %>%
        mutate(emp_status = ifelse(temp==1, yes = "Temporary",
                                   ifelse(perm==1, yes = "Permanent", no = 0))) %>%
        mutate(ID = as.factor(pid))

df_graph$ID <- factor(df_graph$ID, 
                      levels = c("1", "2"), 
                      labels = c("A (Temp to Perm)", "B (Perm to Temp)"))

counterfactual_fe <- df_graph %>%
        group_by(ID,temp) %>%
        mutate(ln_hourly_wage = ifelse(temp==1, yes = mean(ln_hourly_wage), no = NA)) %>%
        group_by(ID) %>%
        mutate(ln_hourly_wage = ifelse(first(!is.na(ln_hourly_wage)), yes = first(ln_hourly_wage), no = last(ln_hourly_wage))) %>%
        ungroup() %>%
        mutate(counterfactual = "FE")

counterfactual_feis <- df_graph %>%
        group_by(ID,temp) %>%
        mutate(ln_hourly_wage = ifelse(temp==1, yes = mean(ln_hourly_wage), no = NA)) %>%
        group_by(ID) %>%
        mutate(ln_hourly_wage = ifelse(first(!is.na(ln_hourly_wage)), yes = first(ln_hourly_wage), no = last(ln_hourly_wage))) %>%
        ungroup() %>%
        mutate(ln_hourly_wage = ifelse(pid==1, yes = 50.49-12.39 + (year)*19.8, no = ln_hourly_wage),
               counterfactual = "FEIS",
        )

counterfactual_t_p_1 <- df_graph %>%
        mutate(ln_hourly_wage = ifelse(pid==1 & year > 1, yes = 50, no = ln_hourly_wage),
               counterfactual = "T - P (1)",
        )

counterfactual_t_p_2 <- df_graph %>%
        mutate(ln_hourly_wage = ifelse(pid==1 & year > 3, yes = 110, no = ln_hourly_wage),
               counterfactual = "T - P (2)",
        )

counterfactual_p_t_1 <- df_graph %>%
        mutate(ln_hourly_wage = ifelse(pid==1 & year > 2, yes = 90, no = ln_hourly_wage),
               counterfactual = "P - T (1)",
        )

counterfactual_p_t_2 <- df_graph %>%
        mutate(ln_hourly_wage = ifelse(pid==1 & year > 4, yes = 130, no = ln_hourly_wage),
               counterfactual = "P - T (2)",
        )

df_counterfactual <- rbind(counterfactual_fe,counterfactual_feis,counterfactual_t_p_1,counterfactual_t_p_2,counterfactual_p_t_1,counterfactual_p_t_2)
table(df_counterfactual$counterfactual)

df_counterfactual$counterfactual <- factor(df_counterfactual$counterfactual,
                                           levels = c("FE", "FEIS", "T - P (1)", "T - P (2)", "P - T (1)", "P - T (2)"))

ggplot() +
        geom_line(data = df_graph, aes(x = year, y = ln_hourly_wage)) +
        geom_point(data = df_graph, aes(x = year, y = ln_hourly_wage, shape = emp_status), size = 2.5) +
        scale_size_manual(values = c(.5,.75,1)) +
        scale_shape_manual(values = c(0,15)) +
        scale_color_manual(values = c("black", "gray60")) +
        scale_y_continuous(breaks =seq(0,200,50), limits = c(0,200)) +
        scale_x_continuous(breaks =seq(1,6,1), limits = c(.5,6.5)) +
        geom_line(data = df_counterfactual, aes(x = year, y = ln_hourly_wage, linetype = counterfactual)) +
        theme_bw() +
        xlab("Time") +
        ylab("Income") +
        labs(shape="Contract type", linetype = "Counterfactual") +
        geom_text(data = df_graph,
                  aes(x = year,
                      y = ln_hourly_wage,
                      label = ln_hourly_wage),
                  show.legend = FALSE,
                  size = 4,
                  vjust = -1) +
        theme(panel.grid.minor = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        )

ggsave(paste0(graphs,"graph_compare_transformed_data_paper.pdf"), height = 6, width = 9, plot = last_plot())

# this calculates the FEIS counterfactual by hand
lm(ln_hourly_wage ~ temp + pid, data = df_original)
lm(ln_hourly_wage ~ temp + pid*year, data = df_original)

feis <- counterfactual_feis$ln_hourly_wage
original <- df_original$ln_hourly_wage

compare <- cbind(feis,original)
df_compare <- data.frame(compare)
df_compare$temp <- df_original$temp
df_compare <- df_compare %>%
        group_by(temp) %>%
        mutate(difference = original-feis,
               mean= mean(difference),
        ) %>%
        ungroup()

df_compare

# Transition indicator ----

df_sample_2 <- df_original %>%
        arrange(country, pid, year) %>%
        group_by(country, pid) %>%
        mutate(event_t_p_yes = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_p_t_yes = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_p_yes = ifelse(perm == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_u_t_yes = ifelse(temp == 1 & lag(unmp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

# create new data set for each transition
df_transition <- df_sample_2 %>%
        select(country,pid,year,trans,transseq,matches("event_")) %>%
        filter(trans==1) %>%
        group_by(country,pid,transseq) %>%
        filter(row_number()==1) %>%
        mutate(eventtime=0,
               eventyear=year) %>%
        ungroup()

# keep data set for individuals without any transitions
df_transition_non <- df_sample_2 %>%
        group_by(country,pid) %>%
        mutate(transseq=max(transseq)) %>%
        ungroup() %>%
        filter(transseq==0) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-trans,-matches("event_"))

# keep data set with information on employment status, wages, and other IVs for merging into df_transition
df_transition_data <- df_sample_2 %>%
        select(-trans,-transseq,-matches("event_"))

df_transition_data

# generate sequence indicator: 4 years before transition and 6 years after transition 
df_sample_events <- data.frame()
event <- c(-4,-3,-2,-1,1,2,3,4,5,6)
for (e in event) {
        df_test <- df_transition %>%
                mutate(eventtime=e)
        df_sample_events <- rbind(df_sample_events,df_test)
        rm(df_test)
}                

df_transition_events <- rbind(df_transition,df_sample_events) %>%
        arrange(country,pid,transseq,eventtime) %>%
        mutate(year = year+eventtime) %>%
        arrange(country,pid,transseq,year) %>%
        select(-trans)

# merge new data with old data
df_multiple_events <- merge(df_transition_events,df_transition_data) %>%
        arrange(country,pid,transseq,year) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-eventtime)

df_multiple_events <- bind_rows(df_multiple_events,df_transition_non)

rm(list=ls(pattern="df_sample"))
rm(list=ls(pattern="df_transition"))

df_multiple_events

# Code first event ----

df_first_event <- df_original

df_first_event <- suppressWarnings(df_first_event %>%
                                           arrange(country, pid, year) %>%
                                           group_by(country, pid) %>%
                                           mutate(event_t_p = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0), # identify treatment
                                                  event_t_p_yes = max(event_t_p),
                                                  event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                  event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                  event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = NA),
                                                  event_t_p_time = ifelse(event_t_p_time < -3, yes = -3,
                                                                          ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                                  event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
                                           ) %>%
                                           ungroup())

df_first_event <- suppressWarnings(df_first_event %>%
                                           arrange(country, pid, year) %>%
                                           group_by(country, pid) %>%
                                           mutate(event_p_t = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = 0),
                                                  event_p_t_yes = max(event_p_t),
                                                  event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                  event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                  event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = NA),
                                                  event_p_t_time = ifelse(event_p_t_time < -3, yes = -3,
                                                                          ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                                  event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
                                           ) %>%
                                           ungroup())


# Code multiple events ----

# temp to perm
df_events_all <- suppressWarnings(df_multiple_events %>%
                                          arrange(country,pidseq,year) %>%
                                          group_by(country,pidseq) %>%
                                          mutate(event_t_p = ifelse(event_t_p_yes == 1 & perm == 1 & dplyr::lag(temp,1) == 1 & row_number()>1 & year == dplyr::lag(year,1)+year_lag, yes = 1, no = 0),
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = eventyear, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = 0),
                                                 event_t_p_time = ifelse(event_t_p_time < -3, yes = -4,
                                                                         ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                                 event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
                                          ) %>%
                                          replace(is.na(.), 0) %>% # ATTN: this makes all missing values zero, not just the NAs from t=1.
                                          ungroup()) 

df_events_all %>% select(pid,pidseq,year,matches("event_t_p"))

# perm to temp
df_events_all <- suppressWarnings(df_events_all %>%
                                          arrange(country,pidseq,year) %>%
                                          group_by(country,pidseq) %>%
                                          mutate(event_p_t = ifelse(event_p_t_yes == 1 & temp == 1 & dplyr::lag(perm,1) == 1 & row_number()>1 & year == dplyr::lag(year,1)+year_lag, yes = 1, no = 0),
                                                 event_p_t_year = ifelse(event_p_t == 1, yes = eventyear, no = NA),
                                                 event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                 event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = 0),
                                                 event_p_t_time = ifelse(event_p_t_time < -3, yes = -4,
                                                                         ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                                 event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
                                          ) %>%
                                          replace(is.na(.), 0) %>% # ATTN: this makes all missing values zero, not just the NAs from t=1.
                                          ungroup())

# Model simulation 1 ----

df_events_all %>% select(country,pid,pidseq,year,matches("event_t_p"))

df_simulation <- df_events_all 


# FE
model_fe_original <- feis(ln_hourly_wage ~ temp | 1,
                          data = data.frame(df_original), 
                          robust = TRUE,
                          id = "pid")


# FEIS
model_feis_original <- feis(ln_hourly_wage ~ temp | year,
                            data = data.frame(df_original), 
                            robust = TRUE,
                            id = "pid")

# FE
model_fe_transformed <- feis(ln_hourly_wage ~ temp | 1,
                   data = data.frame(df_simulation), 
                   robust = TRUE,
                   id = "pidseq")


# FEIS
model_feis_transformed <- feis(ln_hourly_wage ~ temp | year,
                     data = data.frame(df_simulation), 
                     robust = TRUE,
                     id = "pidseq")


# FE + IF
df_simulation$event_t_p_time_pos <- relevel(factor(df_simulation$event_t_p_time_pos), ref = "2")
df_simulation$event_p_t_time_pos <- relevel(factor(df_simulation$event_p_t_time_pos), ref = "2")

model_fe_if <- feis(ln_hourly_wage ~ event_t_p_time_pos + event_p_t_time_pos | 1,
                      data = data.frame(df_simulation), 
                      robust = TRUE,
                      id = "pidseq")

df_first_event$event_t_p_time_pos <- relevel(factor(df_first_event$event_t_p_time_pos), ref = "2")
df_first_event$event_p_t_time_pos <- relevel(factor(df_first_event$event_p_t_time_pos), ref = "2")
model_fe_if_t_p <- feis(ln_hourly_wage ~ event_t_p_time_pos | 1,
                    data = data.frame(df_first_event), 
                    robust = TRUE,
                    id = "pid")

model_fe_if_p_t <- feis(ln_hourly_wage ~ event_p_t_time_pos | 1,
                        data = data.frame(df_first_event), 
                        robust = TRUE,
                        id = "pid")

# Table ----

screenreg(
        list(model_fe_original, model_fe_transformed, model_feis_original, model_feis_transformed, model_fe_if_t_p, model_fe_if_p_t, model_fe_if),
        custom.header = list("FE" = 1:2, "FEIS" = 3:4, "FE + IF" = 5:7),
        custom.model.names = c("Original", "Transformed", "Original", "Transformed", "First", "First", "Multiple"),
        table = FALSE,
        custom.coef.map = list(
                "temp"="Temp",
                "event_t_p_time_pos3"="Event: T-P",
                "event_p_t_time_pos3"="Event: P-T"
                # "event_t_p_time_pos0"="Pre event: T-P (-2)",
                # "event_t_p_time_pos1"="Pre event: T-P (-1)",
                # "event_t_p_time_pos4"="Post event: T-P (+1)",
                # "event_t_p_time_pos5"="Post event: T-P (+2)",
                # "event_t_p_time_pos6"="Post event: T-P (+3)",
                # "event_t_p_time_pos7"="Post event: T-P (+4)",
                # "event_p_t_time_pos0"="Pre event: P-T (-2)",
                # "event_p_t_time_pos1"="Pre event: P-T (-1)",
                # "event_p_t_time_pos4"="Post event: P-T (+1)",
                # "event_p_t_time_pos5"="Post event: P-T (+2)",
                # "event_p_t_time_pos6"="Post event: P-T (+3)",
                # "event_p_t_time_pos7"="Post event: P-T (+4)"
        ),
        include.rsquared = FALSE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
)

texreg(
        list(model_fe_original, model_fe_transformed, model_feis_original, model_feis_transformed, model_fe_if_t_p, model_fe_if_p_t, model_fe_if),
        custom.header = list("FE" = 1:2, "FEIS" = 3:4, "FE + IF" = 5:7),
        custom.model.names = c("Original", "Transformed", "Original", "Transformed", "First", "First", "Multiple"),
        table = FALSE,
        custom.coef.map = list(
                "temp"="Temp",
                "event_t_p_time_pos3"="Event: T $\\rightarrow$ P",
                "event_p_t_time_pos3"="Event: P $\\rightarrow$ T"
                # "event_t_p_time_pos0"="Pre event: T-P (-2)",
                # "event_t_p_time_pos1"="Pre event: T-P (-1)",
                # "event_t_p_time_pos4"="Post event: T-P (+1)",
                # "event_t_p_time_pos5"="Post event: T-P (+2)",
                # "event_t_p_time_pos6"="Post event: T-P (+3)",
                # "event_t_p_time_pos7"="Post event: T-P (+4)",
                # "event_p_t_time_pos0"="Pre event: P-T (-2)",
                # "event_p_t_time_pos1"="Pre event: P-T (-1)",
                # "event_p_t_time_pos4"="Post event: P-T (+1)",
                # "event_p_t_time_pos5"="Post event: P-T (+2)",
                # "event_p_t_time_pos6"="Post event: P-T (+3)",
                # "event_p_t_time_pos7"="Post event: P-T (+4)"
        ),
        booktabs = TRUE, use.packages = FALSE,
        custom.note = paste("%stars. Note: In FE + IF, pre and post event coefficients are not shown."),
        include.rsquared = FALSE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
        file = paste0(tables,"table_compare_transformed_data_paper.tex"),
)

