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

data_files = "data_files/"
results = "results/"
graphs = "graphs/"
tables = "tables/"

# LIBRARY
library(tidyverse)
library(car)
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)
library(xtable)

options(scipen = 999) # disable scientific notation

RIGHT = function(x,n){
        substring(x,nchar(x)-n+1)
}

# load data -----

df_original <- readRDS(paste0(results,"df_yhat_multiple_events_unmp.rds"))
df_original <- df_original %>%
        filter(country!="NE-LISS") %>%
        mutate(country = ifelse(country == "NE-LSP", yes = "NE", no = country))

df_original$country_name <- recode(df_original$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")
table(df_original$country)


# Graph event ----

df_graph <- df_original %>%
        filter(term == "event_u_p_time_pos3" | term == "event_u_t_time_pos3") %>%
        mutate(event = ifelse(term == "event_u_p_time_pos3", yes = "u_p",
                              ifelse(term == "event_u_t_time_pos3", yes = "u_t", no = model))) %>%
        select(country_name,term, estimate, std.error, event)

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("U to P", "U to T"))

df_graph %>% mutate(ymin = estimate-1.96*std.error,
                    ymax = estimate+1.96*std.error)

p <- ggplot(data=df_graph, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2, scales = "free_y") +
        geom_point(size=1) +
        theme_bw() +
        # scale_y_continuous(limits = c(-.3,.3)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        # geom_hline(yintercept = 0) +
        xlab("Model/event type") +
  ylab("Pr(LN Hourly Wage)") +
  geom_text(data = df_graph,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 4,
                  vjust = ifelse(df_graph$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(plot = last_plot(), paste0(graphs,"graph_multiple_events_unmp.pdf"), height = 4, width = 8)


# Table event ----

hline_top <- ("\\toprule \n")
hline_bot <- c("\\bottomrule  \n")

columns_header_top <- c("
country & estimate & std.error & ymin & ymax \\\\ \n
\\cmidrule(lr){1-5} \n 
\\\\[-1.8ex]  \n 
")

# AFE - unmp to perm
df_table_afe_t_p <- df_graph %>% 
  filter(term == "event_u_p_time_pos3") %>% 
  select(-term,-event) %>%
  mutate(ymin = estimate-1.96*std.error,
         ymax = estimate+1.96*std.error)

t <- xtable(df_table_afe_t_p, digits = 4)

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"beta_coef_unmp_afe_u_p.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      # format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,8),
        command = c(hline_top,
                    columns_header_top,
                    hline_bot)),
      comment = FALSE
)

# AFE - unmp to temp

df_table_afe_p_t <- df_graph %>% 
  filter(term == "event_u_t_time_pos3") %>% 
  select(-term,-event) %>%
  mutate(ymin = estimate-1.96*std.error,
         ymax = estimate+1.96*std.error)

t <- xtable(df_table_afe_p_t, digits = 4)

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"beta_coef_unmp_afe_u_t.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      # format.args = list(big.mark = ",", decimal.mark = "."),
      hline.after = NULL,
      add.to.row = list(
        pos = list(0,0,8),
        command = c(hline_top,
                    columns_header_top,
                    hline_bot)),
      comment = FALSE
)

# create better graph

df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_point(size=1) +
        theme_bw() +
        scale_y_continuous(limits = c(0,4)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        geom_text(data = df_graph_1,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 4,
                  vjust = ifelse(df_graph_1$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              axis.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

df_graph_2 <- df_graph %>%
        filter(country_name == "Japan" | country_name == "Korea")

df_graph_2 <- droplevels(df_graph_2)

p2 <- ggplot(data=df_graph_2, aes(x=event, y=estimate)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_point(size=1) +
        theme_bw() +
        scale_y_continuous(limits = c(6,10)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        # xlab("Model/event type") +
        # ylab("Estimate") +
        geom_text(data = df_graph_2,
                  aes(x = event,
                      y = ifelse(estimate>0, yes = estimate+1.96*std.error, no = estimate-1.96*std.error),
                      label = sprintf("%.2f",estimate)),
                  show.legend = FALSE,
                  size = 4,
                  vjust = ifelse(df_graph_2$estimate>0, yes = -.5, no = 2.5)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.title = element_blank(),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

#create common x and y labels

y.grob <- textGrob("Pr(LN Hourly Wage)", 
                   # gp=gpar(fontface="bold", col="blue", fontsize=15), 
                   rot=90)

x.grob <- textGrob("Event type", 
                   # gp=gpar(fontface="bold", col="blue", fontsize=15),
                   )

p3 <- plot_grid(p1, p2, rel_widths = c(3, 1))
p4 <- grid.arrange(arrangeGrob(p3, left = y.grob, bottom = x.grob))

ggsave(plot=p4,paste0(graphs,"graph_multiple_events_unmp_better_paper.pdf"), height = 6, width = 9)

# Graph post event ----

# Prepare data 

df_yhat <- df_original %>%
        filter(str_detect(term, '_time')) %>%
        mutate(event = ifelse(str_detect(term, 'event_u_t_time'), yes = "u_t",
                              ifelse(str_detect(term, 'event_u_p_time'), yes = "u_p", no = 0)))

table(df_yhat$term)

df_yhat$post <- as.numeric(as.character(RIGHT(df_yhat$term,1)))

df_yhat <- df_yhat %>%
        mutate(post = post - 3) %>%
        select(estimate,std.error,event,country,post) %>%
        arrange(event,country,post)

# Graph 

df_graph <- df_yhat %>%
        filter(post>=0 & post < 5) %>%
        filter(event=="u_p" | event == "u_t")

df_graph$event <- factor(df_graph$event,
                         levels = c("u_p","u_t"),
                         labels = c("Unmp to Perm",
                                    "Unmp to Temp"))

df_graph$country_name <- recode(df_graph$country, "'AU'='Australia'; 'CH'='Switzerland'; 'DE'='Germany'; 'IT'='Italy'; 'JP'='Japan'; 'KO'='Korea'; 'NE'='Netherlands'; 'UK'='United Kingdom'")

p <- ggplot(data = df_graph, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(. ~ country_name, nrow = 2, scales = "free_y") +
        geom_line(size = 1) +
        # geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        # scale_y_continuous(limits = c(-.3,.3)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years after event") +
  ylab("Pr(LN Hourly Wage)") +
  scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        guides(color=guide_legend(nrow=1,byrow=TRUE)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )
p

ggsave(plot = p, paste0(graphs,"graph_multiple_events_unmp_post_free_scale.pdf"), height = 6, width = 9)

# create better graph

df_graph_1 <- df_graph %>%
        filter(country_name != "Japan") %>%
        filter(country_name != "Korea")

df_graph_1 <- droplevels(df_graph_1)

p1 <- ggplot(data=df_graph_1, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(.~country_name,  nrow = 2) +
        geom_line(size = 1) +
        theme_bw() +
        scale_y_continuous(limits = c(0,4)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab("Years after event") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_text(hjust = .75),
              legend.key.width=unit(2, "cm"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


df_graph_2 <- df_graph %>%
        filter(country_name == "Japan")

df_graph_2 <- droplevels(df_graph_2)

p2 <- ggplot(data=df_graph_2, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_line(size = 1) +
        theme_bw() +
        scale_y_continuous(limits = c(4,8)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab(" ") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin=unit(c(5.5,5.5,0,0), "pt"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

p2

df_graph_3 <- df_graph %>%
        filter(country_name == "Korea")

df_graph_3 <- droplevels(df_graph_3)

p3 <- ggplot(data=df_graph_3, aes(x = post, y = estimate, color = event, group = event)) +
        facet_wrap(~country_name,drop=TRUE, nrow = 2) +
        geom_line(size = 1) +
        theme_bw() +
        scale_y_continuous(limits = c(6,10)) +
        geom_errorbar(aes(ymin = estimate-1.96*std.error,
                          ymax = estimate+1.96*std.error
        ),
        width=.2) +
        xlab(" ") +
        scale_color_grey(start = 0, end = 0.7) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "none",
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              plot.margin=unit(c(-10,5.5,5.5,-5.5), "pt"),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

p3

#create common x and y labels

y.grob <- textGrob("Pr(LN Hourly Wage)", rot=90)


p4 <- ggarrange(
        p2, p3, nrow = 2
)
p4

p5 <- ggarrange(
        p1, p4, nrow = 1, widths = c(3, 1), 
        common.legend = TRUE, legend = "bottom"
)

p5 <- grid.arrange(arrangeGrob(p5, left = y.grob))
p5

ggsave(plot=p5,paste0(graphs,"graph_multiple_events_unmp_post_better_paper.pdf"), height = 6, width = 9)

