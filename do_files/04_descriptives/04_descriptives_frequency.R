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
setwd("/Users/jonathanlatner/Documents/GitHub/wages_contyp/")

support_files = "support_files/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)

# employment status -----

df_au <- read.csv(file = paste0(support_files, "emp_status_au.csv"))
df_ch <- read.csv(file = paste0(support_files, "emp_status_ch.csv"))
df_de <- read.csv(file = paste0(support_files, "emp_status_de.csv"))
df_it <- read.csv(file = paste0(support_files, "emp_status_it.csv"))
df_jp <- read.csv(file = paste0(support_files, "emp_status_jp.csv"))
df_ko <- read.csv(file = paste0(support_files, "emp_status_ko.csv"))
df_ne <- read.csv(file = paste0(support_files, "emp_status_ne.csv"))
df_uk <- read.csv(file = paste0(support_files, "emp_status_uk.csv"))

df_emp_status <- rbind(df_au,df_de,df_ch,df_it,df_jp,df_ko,df_ne,df_uk)

p<-ggplot(df_emp_status, aes(x=pct, y = lfp, fill=emp)) +
  facet_wrap(~country,scales = "free",nrow=2) +
  geom_bar(stat="identity") +
  # scale_fill_grey()+
  theme_bw()+
  scale_y_discrete(limits = rev(levels(df_emp_status$emp_status)))+
  xlab("")+
  ylab("")+
  theme(legend.position="bottom",legend.title = element_blank())
p

ggsave(plot = p, paste0(graphs,"graph_descriptives_emp_status.pdf"), height = 9, width = 15)


# contract type -----

df_au <- read.csv(file = paste0(support_files, "contyp_au.csv"))
df_ch <- read.csv(file = paste0(support_files, "contyp_ch.csv"))
df_de <- read.csv(file = paste0(support_files, "contyp_de.csv"))
df_it <- read.csv(file = paste0(support_files, "contyp_it.csv"))
df_jp <- read.csv(file = paste0(support_files, "contyp_jp.csv"))
df_ko <- read.csv(file = paste0(support_files, "contyp_ko.csv"))
df_ne <- read.csv(file = paste0(support_files, "contyp_ne.csv"))
df_uk <- read.csv(file = paste0(support_files, "contyp_uk.csv"))

df_emp_status <- rbind(df_au,df_de,df_ch,df_it,df_jp,df_ko,df_ne,df_uk)

p<-ggplot(df_emp_status, aes(x=pct, y = contyp, fill=emp_status)) +
  facet_wrap(~country,scales = "free",nrow=2) +
  geom_bar(stat="identity") +
  # scale_fill_grey()+
  theme_bw()+
  scale_y_discrete(limits = rev(levels(df_emp_status$emp_status)))+
  xlab("")+
  ylab("")+
  theme(legend.position="bottom",legend.title = element_blank())
p

ggsave(plot = p, paste0(graphs,"graph_descriptives_contyp.pdf"), height = 9, width = 15)

with(df_emp_status,table(country,emp_status))


# education -----

df_au <- read.csv(file = paste0(support_files, "edu_au.csv"))
df_ch <- read.csv(file = paste0(support_files, "edu_ch.csv"))
df_de <- read.csv(file = paste0(support_files, "edu_de.csv"))
df_it <- read.csv(file = paste0(support_files, "edu_it.csv"))
df_jp <- read.csv(file = paste0(support_files, "edu_jp.csv"))
df_ko <- read.csv(file = paste0(support_files, "edu_ko.csv"))
df_ne <- read.csv(file = paste0(support_files, "edu_ne.csv"))
df_uk <- read.csv(file = paste0(support_files, "edu_uk.csv"))

df_edu <- rbind(df_au,df_de,df_ch,df_it,df_jp,df_ko,df_ne,df_uk)

df_edu

p<-ggplot(df_edu, aes(x=pct, y = edu, fill=edu_cat)) +
  facet_wrap(~country,scales = "free",nrow=2) +
  geom_bar(stat="identity") +
  # scale_fill_grey()+
  theme_bw()+
  xlab("")+
  ylab("")+
  theme(legend.position="bottom",legend.title = element_blank())
p

ggsave(plot = p, paste0(graphs,"graph_descriptives_education.pdf"), height = 9, width = 15)

with(df_emp_status,table(country,emp_status))
