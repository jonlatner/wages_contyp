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

# Adapt this pathway!
setwd("~/GitHub/wages_contyp/support_files/simulation/")

data_files = "data_files/"
tables = "tables/"
graphs = "graphs/"

# LIBRARY
library(plm)
library(tidyverse)
library(readxl)
library(texreg)
library(feisr)
library(zoo)
library(broom)
library(broomExtra)
library(beepr)

options(scipen = 9999) # disable scientific notation

# Load data -----

id_1 <- data.frame(
        temp = c(1,1,1,0,0,0),
        year = c(1,2,3,4,5,6)
)
id_1$pid = 1

id_2 <- data.frame(
        temp = c(0,0,0,1,1,1),
        year = c(1,2,3,4,5,6)
)
id_2$pid = 2

example_1 <- rbind(id_1,id_2)
example_1$example = "Simulation 1"

example_2 <- rbind(id_1,id_2)
example_2$example = "Simulation 2"

example_3 <- rbind(id_1,id_2)
example_3$example = "Simulation 3"

df_example <- rbind(example_1,example_2,example_3)
rm(id_1,id_2,example_1,example_2,example_3)

# Clean data ----

df_example <- df_example %>%
        mutate(country = "DE",
               perm = ifelse(temp == 0, yes = 1, no = 0)) %>%
        group_by(example,country, pid) %>%
        mutate(year_lag = year - lag(year,1,default = NA)) %>%
        group_by(example,country) %>%
        mutate(year_lag = min(year_lag,na.rm = TRUE)) %>%
        mutate(ln_hourly_wage = ifelse(pid==1, yes = 100, no = 270)) %>%
        ungroup()

df_example_1 <- df_example %>%
        filter(example == "Simulation 1") %>%
        mutate(ln_hourly_wage = ln_hourly_wage + perm*30)

df_example_2 <- df_example %>%
        filter(example == "Simulation 2") %>%
        mutate(
                ln_hourly_wage = ifelse(pid==1, yes = ln_hourly_wage + (year-1)*20 + perm*30, no = ln_hourly_wage),
               # ln_hourly_wage = ifelse(pid==2, yes = ln_hourly_wage + (year-1)*20 + perm*30, no = ln_hourly_wage),
               ln_hourly_wage = ifelse(pid==2, yes = ln_hourly_wage + perm*30, no = ln_hourly_wage),
        )

df_example_3 <- df_example %>%
        filter(example == "Simulation 3") %>%
        mutate(ln_hourly_wage = ifelse(pid==1, yes = 100, no = 300)) %>%
        mutate(ln_hourly_wage = ifelse(pid==1, 
                                       yes = ln_hourly_wage + perm*30,
                                       no = ln_hourly_wage + temp*20))

df_original <- rbind(df_example_1,df_example_2,df_example_3)
rm(df_example_1,df_example_2,df_example_3)

# Graph data ----

df_graph <- df_original %>%
        mutate(emp_status = ifelse(temp==1, yes = "Temporary",
                                   ifelse(perm==1, yes = "Permanent", no = 0))) %>%
        mutate(ID = as.factor(pid))

df_graph$ID <- factor(df_graph$ID, 
                      levels = c("1", "2"), 
                      labels = c("A (Temp to Perm)", "B (Perm to Temp)"))

counterfactual_fe <- df_graph %>%
        group_by(example,ID,temp) %>%
        mutate(ln_hourly_wage = ifelse(temp==1, yes = mean(ln_hourly_wage), no = NA)) %>%
        group_by(example,ID) %>%
        mutate(ln_hourly_wage = ifelse(first(!is.na(ln_hourly_wage)), yes = first(ln_hourly_wage), no = last(ln_hourly_wage))) %>%
        ungroup() %>%
        mutate(counterfactual = "FE")

counterfactual_feis <- df_graph %>%
        group_by(example,ID,temp) %>%
        mutate(ln_hourly_wage = ifelse((example == "Simulation 1" | example == "Simulation 3") & temp==1, yes = mean(ln_hourly_wage), no = NA)) %>%
        group_by(example,ID) %>%
        mutate(ln_hourly_wage = ifelse((example == "Simulation 1" | example == "Simulation 3") & first(!is.na(ln_hourly_wage)), yes = first(ln_hourly_wage), no = last(ln_hourly_wage))) %>%
        ungroup() %>%
        mutate(
                ln_hourly_wage = ifelse(example == "Simulation 2" & pid==1, yes = 100 + (year-1)*20, no = ln_hourly_wage),
                ln_hourly_wage = ifelse(example == "Simulation 2" & pid==2, yes = 270, no = ln_hourly_wage),
                counterfactual = "FEIS",
               )

counterfactual_fe_if <- df_graph %>%
        group_by(example,ID,temp) %>%
        mutate(ln_hourly_wage = ifelse((example == "Simulation 1" | example == "Simulation 3") & temp==1, yes = mean(ln_hourly_wage), no = NA)) %>%
        group_by(example,ID) %>%
        mutate(ln_hourly_wage = ifelse((example == "Simulation 1" | example == "Simulation 3") & first(!is.na(ln_hourly_wage)), yes = first(ln_hourly_wage), no = last(ln_hourly_wage))) %>%
        ungroup() %>%
        mutate(
                ln_hourly_wage = ifelse(example == "Simulation 2" & pid==1, yes = 100 + (year-1)*20, no = ln_hourly_wage),
               ln_hourly_wage = ifelse(example == "Simulation 2" & pid==1 & year > 3, yes = 140, no = ln_hourly_wage),
               ln_hourly_wage = ifelse(example == "Simulation 2" & pid==2, yes = 270, no = ln_hourly_wage),
               ln_hourly_wage = ifelse(example == "Simulation 2" & pid==2 & year > 3, yes = 270, no = ln_hourly_wage),
               counterfactual = "FE + IF",
        )

df_counterfactual <- rbind(counterfactual_fe,counterfactual_feis,counterfactual_fe_if)

df_counterfactual$counterfactual <- factor(df_counterfactual$counterfactual,
                                           levels = c("FE", "FEIS", "FE + IF"))

ggplot() +
        geom_line(data = df_graph, aes(x = year, y = ln_hourly_wage, color = ID)) +
        geom_point(data = df_graph, aes(x = year, y = ln_hourly_wage, shape = emp_status), size = 2.5) +
        facet_wrap(~example,nrow = 1,scales = "fixed") +
        scale_size_manual(values = c(.5,.75,1)) +
        scale_linetype_manual(values = c("longdash","dotted","dashed")) +
        scale_shape_manual(values = c(0,15)) +
        scale_color_manual(values = c("black", "gray60")) +
        scale_y_continuous(breaks =seq(0,400,50), limits = c(0,400)) +
        scale_x_continuous(breaks =seq(1,6,1), limits = c(.5,6.5)) +
        geom_line(data = df_counterfactual, aes(x = year, y = ln_hourly_wage, linetype = counterfactual, color = ID)) +
        theme_bw() +
        xlab("Time") +
        ylab("Income") +
        labs(color = "Individual", shape="Contract type", linetype = "Counterfactual") +
        geom_text(data = df_graph,
                  aes(x = year,
                      y = ln_hourly_wage,
                      label = ln_hourly_wage),
                  show.legend = FALSE,
                  size = 4,
                  vjust = -1) +
        theme(panel.grid.minor = element_blank(),
              legend.margin = margin(0,0,0, unit="cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        )

ggsave(paste0(graphs,"graph_compare_models_simulation_paper.pdf"), height = 6, width = 9, plot = last_plot())

# Transition indicator ----

df_sample_2 <- df_original %>%
        arrange(example,country,pid,year) %>%
        group_by(example,country,pid) %>%
        mutate(event_t_p_yes = ifelse(perm == 1 & lag(temp,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
               event_p_t_yes = ifelse(temp == 1 & lag(perm,1) == 1 & row_number()>1 & year == lag(year,1,NA)+year_lag, yes = 1, no = NA),
        ) %>%
        ungroup() %>%
        mutate(trans = rowSums(select(., contains("event_")), na.rm = TRUE)) %>%
        group_by(country, pid) %>%
        mutate(transseq = cumsum(ifelse(is.na(trans), 0, trans))) %>%
        ungroup()

# create new data set for each transition
df_transition <- df_sample_2 %>%
        select(example,country,pid,year,trans,transseq,matches("event_")) %>%
        filter(trans==1) %>%
        group_by(example,country,pid,transseq) %>%
        filter(row_number()==1) %>%
        mutate(eventtime=0) %>%
        ungroup()

# keep data set for individuals without any transitions
df_transition_non <- df_sample_2 %>%
        group_by(example,country,pid) %>%
        mutate(transseq=max(transseq)) %>%
        ungroup() %>%
        filter(transseq==0) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-trans,-matches("event_"))

# keep data set with information on employment status, wages, and other IVs for merging into df_transition
df_transition_data <- df_sample_2 %>%
        select(-trans,-transseq,-matches("event_"))

# generate sequence indicator: 2 years before transition and 3 years after transition 
df_sample_events <- data.frame()
event <- c(-3,-2,-1,1,2,3,4)
for (e in event) {
        df_test <- df_transition %>%
                mutate(eventtime=e)
        df_sample_events <- rbind(df_sample_events,df_test)
        rm(df_test)
}                

df_transition_events <- rbind(df_transition,df_sample_events) %>%
        arrange(example,country,pid,transseq,eventtime) %>%
        mutate(year = year+eventtime) %>%
        arrange(example,country,pid,transseq,year) %>%
        select(-trans)

# merge new data with old data
df_multiple_events <- merge(df_transition_events,df_transition_data) %>%
        arrange(example,country,pid,transseq,year) %>%
        mutate(pidseq=pid*100+transseq) %>% # new identifier
        select(-eventtime)

df_multiple_events <- bind_rows(df_multiple_events,df_transition_non)

rm(list=ls(pattern="df_sample"))
rm(list=ls(pattern="df_transition"))


# Code events ----

# temp to perm
df_events_all <- suppressWarnings(df_multiple_events %>%
                                          arrange(example,country,pidseq,year) %>%
                                          group_by(example,country,pidseq) %>%
                                          mutate(event_t_p = ifelse(event_t_p_yes == 1 & perm == 1 & dplyr::lag(temp,1) == 1 & row_number()>1 & year == dplyr::lag(year,1)+year_lag, yes = 1, no = 0),
                                                 event_t_p_year = ifelse(event_t_p == 1, yes = year, no = NA),
                                                 event_t_p_year = min(event_t_p_year, na.rm=TRUE),
                                                 event_t_p_time = ifelse(event_t_p_yes == 1, yes = year - event_t_p_year, no = 0),
                                                 event_t_p_time = ifelse(event_t_p_time < -3, yes = -4,
                                                                         ifelse(event_t_p_time > 4, yes = 4, no = event_t_p_time)),
                                                 event_t_p_time_pos = ifelse(event_t_p_yes == 1, yes = event_t_p_time + 3, no = 0),
                                          ) %>%
                                          replace(is.na(.), 0) %>% # ATTN: this makes all missing values zero, not just the NAs from t=1.
                                          ungroup()) 

# perm to temp
df_events_all <- suppressWarnings(df_events_all %>%
                                          arrange(example,country,pidseq,year) %>%
                                          group_by(example,country,pidseq) %>%
                                          mutate(event_p_t = ifelse(event_p_t_yes == 1 & temp == 1 & dplyr::lag(perm,1) == 1 & row_number()>1 & year == dplyr::lag(year,1)+year_lag, yes = 1, no = 0),
                                                 event_p_t_yes = max(event_p_t),
                                                 event_p_t_year = ifelse(event_p_t == 1, yes = year, no = NA),
                                                 event_p_t_year = min(event_p_t_year, na.rm=TRUE),
                                                 event_p_t_time = ifelse(event_p_t_yes == 1, yes = year - event_p_t_year, no = 0),
                                                 event_p_t_time = ifelse(event_p_t_time < -3, yes = -4,
                                                                         ifelse(event_p_t_time > 4, yes = 4, no = event_p_t_time)),
                                                 event_p_t_time_pos = ifelse(event_p_t_yes == 1, yes = event_p_t_time + 3, no = 0),
                                          ) %>%
                                          replace(is.na(.), 0) %>% # ATTN: this makes all missing values zero, not just the NAs from t=1.
                                          ungroup())


# Model simulation 1 ----

df_simulation <- filter(df_events_all, example == "Simulation 1")

df_simulation %>% select(pid,ln_hourly_wage,temp,year,event_p_t_time,event_t_p_time)

# FE
model_fe_1 <- feis(ln_hourly_wage ~ temp | 1,
                 data = data.frame(df_simulation), 
                 robust = TRUE,
                 id = "pidseq")


# FEIS
model_feis_1 <- feis(ln_hourly_wage ~ temp | year,
                   data = data.frame(df_simulation), 
                   robust = TRUE,
                   id = "pidseq")


# FE + IF
df_simulation$event_t_p_time_pos <- relevel(factor(df_simulation$event_t_p_time_pos), ref = "2")
df_simulation$event_p_t_time_pos <- relevel(factor(df_simulation$event_p_t_time_pos), ref = "2")

model_fe_if_1 <- feis(ln_hourly_wage ~ event_t_p_time_pos + event_p_t_time_pos | 1,
                    data = data.frame(df_simulation), 
                    robust = TRUE,
                    id = "pidseq")

# Model simulation 2 ----

df_simulation <- filter(df_events_all, example == "Simulation 2")

df_simulation %>% select(pid,ln_hourly_wage,temp,year,event_p_t_time,event_t_p_time)

# FE
model_fe_2 <- feis(ln_hourly_wage ~ temp | 1,
                 data = data.frame(df_simulation), 
                 robust = TRUE,
                 id = "pidseq")


# FEIS
model_feis_2 <- feis(ln_hourly_wage ~ temp | year,
                   data = data.frame(df_simulation), 
                   robust = TRUE,
                   id = "pidseq")


# FE + IF
df_simulation$event_t_p_time_pos <- relevel(factor(df_simulation$event_t_p_time_pos), ref = "2")
df_simulation$event_p_t_time_pos <- relevel(factor(df_simulation$event_p_t_time_pos), ref = "2")

model_fe_if_2 <- feis(ln_hourly_wage ~ event_t_p_time_pos + event_p_t_time_pos | 1,
                    data = data.frame(df_simulation), 
                    robust = TRUE,
                    id = "pidseq")


# Model simulation 3 ----

df_simulation <- filter(df_events_all, example == "Simulation 3")

df_simulation %>% select(pid,ln_hourly_wage,temp,year,event_p_t_time,event_t_p_time)

# FE
model_fe_3 <- feis(ln_hourly_wage ~ temp | 1,
                   data = data.frame(df_simulation), 
                   robust = TRUE,
                   id = "pidseq")


# FEIS
model_feis_3 <- feis(ln_hourly_wage ~ temp | year,
                     data = data.frame(df_simulation), 
                     robust = TRUE,
                     id = "pidseq")


# FE + IF
df_simulation$event_t_p_time_pos <- relevel(factor(df_simulation$event_t_p_time_pos), ref = "2")
df_simulation$event_p_t_time_pos <- relevel(factor(df_simulation$event_p_t_time_pos), ref = "2")

model_fe_if_3 <- feis(ln_hourly_wage ~ event_t_p_time_pos + event_p_t_time_pos | 1,
                      data = data.frame(df_simulation), 
                      robust = TRUE,
                      id = "pidseq")

# Table ----

# screenreg(
#         list(model_fe, model_feis, model_fe_if),
#         custom.model.names = c("FE", "FEIS", "FE + IF"),
#         table = FALSE,
#         custom.coef.map = list(
#                 "temp"="Temp",
#                 "event_t_p_time_pos3"="Event: T-P",
#                 "event_p_t_time_pos3"="Event: P-T",
#                 "event_t_p_time_pos0"="Pre event: T-P (-2)",
#                 "event_t_p_time_pos1"="Pre event: T-P (-1)",
#                 "event_t_p_time_pos4"="Post event: T-P (+1)",
#                 "event_t_p_time_pos5"="Post event: T-P (+2)",
#                 "event_t_p_time_pos6"="Post event: T-P (+3)",
#                 "event_t_p_time_pos7"="Post event: T-P (+4)",
#                 "event_p_t_time_pos0"="Pre event: P-T (-2)",
#                 "event_p_t_time_pos1"="Pre event: P-T (-1)",
#                 "event_p_t_time_pos4"="Post event: P-T (+1)",
#                 "event_p_t_time_pos5"="Post event: P-T (+2)",
#                 "event_p_t_time_pos6"="Post event: P-T (+3)",
#                 "event_p_t_time_pos7"="Post event: P-T (+4)"
#         ),
#         include.rsquared = FALSE, include.adjrs = FALSE,include.nobs = TRUE,include.rmse=FALSE,include.groups=FALSE,
#         omit.coef = c("year|Intercept"),
# )

texreg(
        list(model_fe_1, model_feis_1, model_fe_if_1, 
             model_fe_2, model_feis_2, model_fe_if_2, 
             model_fe_3, model_feis_3, model_fe_if_3
             ),
        custom.header = list("Simulation 1" = 1:3, 
                             "Simulation 2" = 4:6, 
                             "Simulation 3" = 7:9
        ),
        custom.model.names = c("FE", "FEIS", "FE + DIF", 
                               "FE", "FEIS", "FE + DIF", 
                               "FE", "FEIS", "FE + DIF"
                               ),
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
        caption = "Simulation",caption.above = TRUE,
        booktabs = TRUE, use.packages = FALSE,
        custom.note = paste("%stars. Note: In FE + IF, pre and post event coefficients are not shown."),
        include.rsquared = FALSE, include.adjrs = FALSE,include.nobs = FALSE,include.rmse=FALSE,include.groups=FALSE,
        omit.coef = c("year|Intercept"),
        file = paste0(tables,"table_compare_models_simulation_paper.tex"),
)

beep()
