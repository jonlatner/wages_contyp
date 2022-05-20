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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/SECCOPA/")

data_files = "projects/mobility/data_files/"
tables = "projects/mobility/tables/"
graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(feisr)
library(texreg)
library(dummies)
library(beepr)
library(robumeta) #group.center

options(scipen = 999) # disable scientific notation

# load data -----


df_sample_0 <- readRDS(paste0(data_files,"df_sample_clean.rds"))%>%
        mutate(age_2 = age*age)

# make sample -----

# set.seed(1234)
# 
# df_sample_10_unique <- df_sample_0 %>%
#         select(country,pid) %>%
#         filter(country!="NE-LISS") %>%
#         filter(country!="NE-LSP") %>%
#         group_by(country,pid) %>%
#         filter(row_number()==1) %>%
#         ungroup() %>%
#         sample_frac(.2) %>%
#         mutate(keep=1)
# 
# df_NE <- df_sample_0 %>%
#         select(country,pid) %>%
#         filter(country == "NE-LISS") %>%
#         mutate(keep=1)
# 
# df_NE_LSP <- df_sample_0 %>%
#         select(country,pid) %>%
#         filter(country == "NE-LSP") %>%
#         group_by(country,pid) %>%
#         filter(row_number()==1) %>%
#         ungroup() %>%
#         sample_frac(.25) %>%
#         mutate(keep=1)
# 
# df_sample_10_unique <- rbind(df_sample_10_unique,df_NE,df_NE_LSP)
# 
# df_sample_10 <- merge(df_sample_0,df_sample_10_unique) %>%
#         filter(keep==1)
# 
# df_sample_0 <- df_sample_10
# 
# rm(df_sample_10_unique,df_sample_10,df_NE,df_NE_LSP)

# clean data -----

df_event_1 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,age_2,matches("event_1")) %>%
        rename(event_time_pos = event_1_time_pos)

df_event_2 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,age_2,matches("event_2")) %>%
        rename(event_time_pos = event_2_time_pos)

df_event_3 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,age_2,matches("event_3")) %>%
        rename(event_time_pos = event_3_time_pos)

df_event_4 <- df_sample_0 %>%
        select(country,pid,year,ln_hourly_wage,unmp,temp,perm,age,age_2,matches("event_4")) %>%
        rename(event_time_pos = event_4_time_pos)

# examine some data ----

# df_test <- df_event_2 %>%
#         filter(event_2_yes==1 & country == "NE-LISS") %>%
#         select(country,pid,year,unmp,temp,perm,matches("event"))
# with(df_test,table(event_time_pos))

# df_event_1 %>%
#         filter(event_1_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_1,event_1_time,event_time_pos)
# 
# df_test <- df_event_1 %>%
#         filter(event_1_yes==1 & country == "IT") %>%
#         select(country,pid,year,unmp,temp,perm,event_1,event_1_time,event_time_pos)
# with(df_test,table(event_1_time,event_time_pos))
# 
# df_test %>% filter(pid==248532)

# prepare for output ----

df_model_yhat <- data.frame()

# Variables  ----

iv_vars_annual = c("age + event_time_pos_1 + event_time_pos_2 + event_time_pos_3 + event_time_pos_4 + event_time_pos_5 + event_time_pos_6 + event_time_pos_7 + event_time_pos_8")
iv_vars_biannual = c("age + event_time_pos_1 + event_time_pos_3 + event_time_pos_5 + event_time_pos_7 + event_time_pos_8")

vars_annual = c("ln_hourly_wage", "age", "event_time_pos_1", "event_time_pos_2", "event_time_pos_3", "event_time_pos_4", "event_time_pos_5", "event_time_pos_6", "event_time_pos_7", "event_time_pos_8")
vars_biannual = c("ln_hourly_wage", "age", "event_time_pos_1", "event_time_pos_3", "event_time_pos_5", "event_time_pos_7", "event_time_pos_8")

# Model (Annual countries) ----

country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
event <- c(1:4)
for(c in country) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # create year variables
                df_dummy <- dummy(x = df_country$year,sep = "")
                df_country <- cbind(df_country,df_dummy)
                
                df_dummy <- dummy(x = df_country$event_time_pos,sep = "_")
                df_country <- cbind(df_country,df_dummy)
                
                year <- sort(unique(df_country$year))
                year_vars = c()
                for(y in year) {
                        year_vars <- c(year_vars,paste0("year",as.character(y)))
                }
                remove <- c(year_vars[1]) # drop baseline
                year_vars <- year_vars [! year_vars %in% remove]
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_annual,year_vars)
                
                for (v in vars) {
                        df_country$test <- group.center(df_country[[v]], df_country$pid)
                        df_country$test <- as.numeric(df_country$test)
                        df_country[[v]] <- NULL
                        names(df_country)[names(df_country) == "test"] <- paste0(v)
                }
                
                # create independent variables
                iv_all = c()
                for (v in vars) {
                        iv_all <- paste(iv_all,v,"+")
                }
                iv_all <- sub("..$", "", iv_all)
                
                # model
                df_country <- data.frame(df_country)
                model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_all)),
                            data = df_country)
                
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

# Model (Biannual countries) ----

country <- c("NE-LSP","IT")
event <- c(1:4)
for(c in country) {
        print(paste0("country = ", c))
        for (e in event) {
                print(paste0("event = ", e))
                df_event <- get(paste0("df_event_",e))
                
                df_country <- df_event %>%
                        filter(country == c)
                
                # create year variables
                df_dummy <- dummy(x = df_country$year,sep = "")
                df_country <- cbind(df_country,df_dummy)
                
                df_dummy <- dummy(x = df_country$event_time_pos,sep = "_")
                df_country <- cbind(df_country,df_dummy)
                
                year <- sort(unique(df_country$year))
                year_vars = c()
                for(y in year) {
                        year_vars <- c(year_vars,paste0("year",as.character(y)))
                }
                remove <- c(year_vars[1]) # drop baseline
                year_vars <- year_vars [! year_vars %in% remove]
                year_vars
                
                # Prepare data for transitory exit with individual fixed effects 
                vars <- c(vars_biannual,year_vars)
                
                for (v in vars) {
                        df_country$test <- group.center(df_country[[v]], df_country$pid)
                        df_country$test <- as.numeric(df_country$test)
                        df_country[[v]] <- NULL
                        names(df_country)[names(df_country) == "test"] <- paste0(v)
                }
                
                # create independent variables
                iv_all = c()
                for (v in vars) {
                        iv_all <- paste(iv_all,v,"+")
                }
                iv_all <- sub("..$", "", iv_all)
                
                # model
                df_country <- data.frame(df_country)
                model <- lm(as.formula(paste0("ln_hourly_wage ~ ",iv_all)),
                            data = df_country)
                
                assign(paste0("model_event_",e,"_country_",c),model)
        }
}

# Tables ----

# Event by countries
country <- c("AU","CH","DE","JP","KO","NE-LISS","UK","NE-LSP","IT")
event <- c(1:4)
for (e in event) {
        for(c in country) {
                
        model_event_country <- get(paste0("model_event_",e,"_country_",c))
        assign(paste0("model_",c),model_event_country)

        }
        texreg(list(model_AU,model_CH,model_DE,model_JP,model_KO,`model_NE-LISS`,model_UK,`model_NE-LSP`,model_IT),
               custom.model.names = c("AU", "CH", "DE", "JP", "KO", "NE-LISS", "UK", "NE-LSP","IT"),
               custom.coef.names = c("Age", "Pre event (-2)", "Pre event (-1)", "Event", "Post event (+1)", "Post event (+2)", "Post event (+3)", "Post event (+4)", "Post event ($>=5$)"),
               file = paste0(tables,"table_model_2_output_event_",e,".tex"),
               table = FALSE,
               include.rsquared = TRUE, include.adjrs = FALSE,include.nobs = FALSE,include.rmse=FALSE,include.groups=FALSE,
               omit.coef = c("year|Intercept"))
}

# New data (Annual) ----

df_yhat <- data.frame()

newdata_neg_2 = data.frame(age=0,
                           event_time_pos_1=1,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_neg_1 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=1, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_0 = data.frame(age=0,
                       event_time_pos_1=0,event_time_pos_2=0, # pre event
                       event_time_pos_3=1, # event
                       event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                       year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                       year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_1 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=1,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_2 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=1,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_3 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=1,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_4 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=1,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

# Predict (Annual countries) ----

country <- c("AU","CH","DE","JP","KO","NE-LISS","UK")
event <- c(1:4)
for(c in country) {
        for (e in event) {
        model_event <- get(paste0("model_event_",e,"_country_",c))
        print(c)
        print(e)
        
        yhat_neg_2 <- data.frame(predict(object = model_event,
                                         se.fit = TRUE,
                                         newdata = newdata_neg_2
        )) %>%
                select(fit,se.fit)
        yhat_neg_2$post <- -2
        
        yhat_neg_1 <- data.frame(predict(object = model_event,
                                         se.fit = TRUE,
                                         newdata = newdata_neg_1
        )) %>%
                select(fit,se.fit)
        yhat_neg_1$post <- -1
        
        yhat_0 <- data.frame(predict(object = model_event,
                                     se.fit = TRUE,
                                     newdata = newdata_0
        )) %>%
                select(fit,se.fit)
        yhat_0$post <- 0
        
        yhat_1 <- data.frame(predict(object = model_event,
                                     se.fit = TRUE,
                                     newdata = newdata_pos_1
        )) %>%
                select(fit,se.fit)
        yhat_1$post <- 1
        
        yhat_2 <- data.frame(predict(object = model_event,
                                     se.fit = TRUE,
                                     newdata = newdata_pos_2
        )) %>%
                select(fit,se.fit)
        yhat_2$post <- 2
        
        
        yhat_3 <- data.frame(predict(object = model_event,
                                     se.fit = TRUE,
                                     newdata = newdata_pos_3
        )) %>%
                select(fit,se.fit)
        yhat_3$post <- 3
        
        yhat_4 <- data.frame(predict(object = model_event,
                                     se.fit = TRUE,
                                     newdata = newdata_pos_4
        )) %>%
                select(fit,se.fit)
        yhat_4$post <- 4
        
        yhat <- rbind(yhat_neg_2,yhat_neg_1,yhat_0,yhat_1,yhat_2,yhat_3,yhat_4)        
        yhat$event <- e
        yhat$country <- c
        df_yhat <- rbind(df_yhat,yhat)
        }
}

# New data (Biannual) ----

newdata_neg_2 = data.frame(age=0,
                           event_time_pos_1=1, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_0 = data.frame(age=0,
                       event_time_pos_1=0,event_time_pos_2=0, # pre event
                       event_time_pos_3=1, # event
                       event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                       year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                       year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_2 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=1,event_time_pos_6=0,event_time_pos_7=0,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

newdata_pos_4 = data.frame(age=0,
                           event_time_pos_1=0,event_time_pos_2=0, # pre event
                           event_time_pos_3=0, # event
                           event_time_pos_4=0,event_time_pos_5=0,event_time_pos_6=0,event_time_pos_7=1,event_time_pos_8=0, # post event
                           year2001=0,year2002=0,year2003=0,year2004=0,year2005=0,year2006=0,year2007=0,year2008=0,year2009=0,year2010=0,
                           year2011=0,year2012=0,year2013=0,year2014=0,year2015=0,year2016=0,year2017=0,year2018=0
)

# Predict (Biannual countries) ----

country <- c("NE-LSP","IT")
event <- c(1:4)
for(c in country) {
        for (e in event) {
                model_event <- get(paste0("model_event_",e,"_country_",c))
                print(c)
                print(e)
                
                
                yhat_neg_2 <- data.frame(predict(object = model_event,
                                                 se.fit = TRUE,
                                                 newdata = newdata_neg_2
                )) %>%
                        select(fit,se.fit)
                yhat_neg_2$post <- -2
                
                yhat_0 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_0
                )) %>%
                        select(fit,se.fit)
                yhat_0$post <- 0
                
                yhat_2 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_2
                )) %>%
                        select(fit,se.fit)
                yhat_2$post <- 2
                
                
                yhat_4 <- data.frame(predict(object = model_event,
                                             se.fit = TRUE,
                                             newdata = newdata_pos_4
                )) %>%
                        select(fit,se.fit)
                yhat_4$post <- 4
                
                yhat <- rbind(yhat_neg_2,yhat_0,yhat_2,yhat_4)        
                yhat$event <- e
                yhat$country <- c
                df_yhat <- rbind(df_yhat,yhat)
        }
}

# rm(list=ls(pattern="yhat_"))
# rm(list=ls(pattern="model"))
# rm(list=ls(pattern="newdata"))

# graph predict ----

df_graph <- df_yhat %>%
        filter(event==1 | event == 4)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,4,2,3),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 4 (perm to temp)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_2_output_event_1_4.pdf"), height = 4, width = 8, plot = last_plot())

df_graph <- df_yhat %>%
        filter(event==2 | event == 3)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,4,2,3),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 4 (perm to temp)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_2_output_event_2_3.pdf"), height = 4, width = 8, plot = last_plot())

df_graph <- df_yhat %>%
        filter(post>=0) %>%
        filter(event==2 | event == 3)

df_graph$event <- factor(df_graph$event,
                         levels = c(1,4,2,3),
                         labels = c("Event 1 (temp to perm)",
                                    "Event 4 (perm to temp)",
                                    "Event 2 (unmp to perm)",
                                    "Event 3 (unmp to temp)"))

ggplot(data = df_graph, aes(x = post, y = fit, color = event, group = event)) +
        facet_wrap(. ~ country, scales = "free_y") +
        geom_line(size = 1) +
        # scale_y_continuous(breaks =seq(-50,50,25), limits = c(-75,75)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
        ),
        width=.2) +
        xlab("Years before/after event") +
        ylab("Effects on Pr(LN hourly wage)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_2_output_event_2_3_post_event.pdf"), height = 4, width = 8, plot = last_plot())
