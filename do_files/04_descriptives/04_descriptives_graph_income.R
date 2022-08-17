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
setwd("~/GitHub/wages_contyp/")

support_files = "support_files/"
data_files = "data_files/"
results = "results/"
graphs = "graphs/"

# LIBRARY
library(tidyverse)
library(car)
library(janitor)
library(readxl)
library(zoo)
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)


options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load sample data -----

df_original_01 <- readRDS(paste0(data_files,"03d_df_sample_cleaned_prepared_first_event_data.rds"))

df_original_01 <- df_original_01 %>%
        filter(country!="NE-LISS")
df_original_01$country <- recode(df_original_01$country, "'NE-LSP'='NE'")

df_original_01$country_name <- recode(df_original_01$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

df_original_02 <- df_original_01 %>%
        filter(unmp == 0) %>%
        group_by(country_name, year) %>%
        summarise(wages = mean(wages) * 12) %>%
        ungroup() %>%
        mutate(source = "Sample (B)")

# load cpi data (adjust to 2010) -----

df_inflation <- read.csv(paste0(support_files, "world_bank/world_bank_cpi.csv"), sep = ",") 

df_inflation <- pivot_longer(df_inflation, 
                                cols = !year,
                                names_to = "country",
                                values_to = "cpi",
                                ) 

head(df_inflation)

# load OECD data -----

df_oecd_01 <- read_xlsx(paste0(support_files,"OECD/OECD_average_annual_wages_modified.xlsx"), skip = 1)
df_oecd_02 <- clean_names(df_oecd_01)
df_oecd_02 <- df_oecd_02 %>%
        rename(country=time,
               series=x2,
               unit=x3) %>%
        filter(row_number()!=1) %>%
        mutate(country = na.locf(country,na.rm = TRUE))

df_oecd_03 <- pivot_longer(df_oecd_02, 
                                cols = starts_with("x"),
                                names_prefix = "x", 
                                names_to = "year", 
                                values_to = "income") 

df_oecd_04 <- df_oecd_03 %>% 
        rename(country_name=country) %>%
        filter(series == "Current prices in NCU") %>%
        select(-series,-unit) %>%
        mutate()

df_oecd_04$country <- recode(df_oecd_04$country_name, "'Australia'='AU'; 'Switzerland'='CH'; 'Germany'='DE'; 'Italy'='IT'; 'Japan'='JP'; 'Korea'='KO'; 'Netherlands'='NE'; 'United Kingdom'='UK'; else=NA")

df_oecd_04 <- df_oecd_04 %>%
        filter(!is.na(country))

df_oecd_merge_01 <- merge(df_oecd_04,df_inflation,by = c("country","year"))
df_oecd_merge_02 <- df_oecd_merge_01 %>%
        mutate(wages=income/(cpi/100)) %>%
        mutate(source = "OECD",
               year = as.numeric(year)) %>%
        select(country_name, year, wages, source)

rm(df_oecd_01,df_oecd_02,df_oecd_03,df_oecd_04,df_inflation)

# Graph  ----

df_graph <- rbind(df_oecd_merge_02,df_original_02)

ggplot(data = df_graph, aes(x = year, y = log(wages), color = source)) +
        facet_wrap(. ~ country_name, nrow = 2, scales = "free_y") +
        geom_line(size = 1) +
        scale_color_grey(start = 0, end = 0.7) +
        xlab("Year") +
        ylab("Annual average wages (LN)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot=last_plot(),paste0(graphs,"graph_descriptives_income.pdf"), height = 4, width = 8)

# Graph (nicer)  ----

df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data = df_graph_1, aes(x = year, y = log(wages), color = source)) +
        facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(9,12)) +
        scale_color_grey(start = 0, end = 0.7) +
        xlab("Year") +
        # ylab("Annual average wages (LN)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.title.x = element_text(hjust = .75),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


df_graph_2 <- df_graph %>%
        filter(country_name == "Japan" | country_name == "Korea")

df_graph_2 <- droplevels(df_graph_2)


p2 <- ggplot(data = df_graph_2, aes(x = year, y = log(wages), color = source)) +
        facet_wrap(. ~ country_name, nrow = 2) +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(15,18)) +
        scale_color_grey(start = 0, end = 0.7) +
        xlab(" ") +
        # ylab("Annual average wages (LN)") +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


y.grob <- textGrob("Annual average wages (LN)", 
                   rot=90)

p3 <- ggarrange(
        p1, p2, nrow = 1, widths = c(3, 1), 
        common.legend = TRUE, legend = "bottom"
)

p4 <- grid.arrange(arrangeGrob(p3, left = y.grob))

ggsave(plot=p4,paste0(graphs,"graph_descriptives_income_better_paper.pdf"), height = 6, width = 9)

