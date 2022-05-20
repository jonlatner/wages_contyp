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

graphs = "projects/mobility/graphs/"
results = "projects/mobility/results/"

# LIBRARY
library(tidyverse)
library(plm)
library(stargazer)
library(dummies)
library(robumeta)
library(ggplot2)

options(scipen = 999) # disable scientific notation

## The function to get overlapping strip labels
OverlappingStripLabels = function(plot) {
        
        # Get the ggplot grob
        pg = ggplotGrob(plot)
        
        ### Collect some information about the strips from the plot
        # Get a list of strips
        stripr = lapply(grep("strip-r", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        stript = lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        # Number of strips
        NumberOfStripsr = sum(grepl(pattern = "strip-r", pg$layout$name))
        NumberOfStripst = sum(grepl(pattern = "strip-t", pg$layout$name))
        
        # Number of columns
        NumberOfCols = length(stripr[[1]])
        NumberOfRows = length(stript[[1]])
        
        # Panel spacing
        plot_theme <- function(p) {
                plyr::defaults(p$theme, theme_get())
        }
        PanelSpacing = plot_theme(plot)$panel.spacing
        
        # Map the boundaries of the new strips
        Nlabelr = vector("list", NumberOfCols)
        mapr = vector("list", NumberOfCols)
        for(i in 1:NumberOfCols) {
                
                for(j in 1:NumberOfStripsr) {
                        Nlabelr[[i]][j] = getGrob(grid.force(stripr[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapr[[i]][1] = TRUE
                for(j in 2:NumberOfStripsr) {
                        mapr[[i]][j] = as.character(Nlabelr[[i]][j]) != as.character(Nlabelr[[i]][j-1])#Nlabelr[[i]][j] != Nlabelr[[i]][j-1]
                }
        }
        
        # Map the boundaries of the new strips
        Nlabelt = vector("list", NumberOfRows)
        mapt = vector("list", NumberOfRows)
        for(i in 1:NumberOfRows) {
                
                for(j in 1:NumberOfStripst) {
                        Nlabelt[[i]][j] = getGrob(grid.force(stript[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapt[[i]][1] = TRUE
                for(j in 2:NumberOfStripst) {
                        mapt[[i]][j] = as.character(Nlabelt[[i]][j]) != as.character(Nlabelt[[i]][j-1])#Nlabelt[[i]][j] != Nlabelt[[i]][j-1]
                }
        }
        
        
        ## Construct gtable to contain the new strip
        newStripr  = gtable(heights = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripsr-1), unit(1, "null")), 
                            widths = stripr[[1]]$widths)
        ## Populate the gtable  
        seqTop = list()
        for(i in NumberOfCols:1) {  
                Top = which(mapr[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfCols) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripsr-1))
                newStripr = gtable_add_grob(newStripr, lapply(stripr[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
        }
        
        mapt <- mapt[NumberOfRows:1]
        Nlabelt <- Nlabelt[NumberOfRows:1]
        ## Do the same for top facets
        newStript  = gtable(heights = stript[[1]]$heights,
                            widths = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripst-1), unit(1, "null")))
        seqTop = list()
        for(i in NumberOfRows:1) {  
                Top = which(mapt[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfRows) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripst-1))
                # newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
                newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[(NumberOfRows:1)[i]]]), t = (NumberOfRows:1)[i], l = seqTop[[i]], r = seqBottom)
        }
        
        ## Put the strip into the plot
        # Get the locations of the original strips
        posr = subset(pg$layout, grepl("strip-r", pg$layout$name), t:r)
        post = subset(pg$layout, grepl("strip-t", pg$layout$name), t:r)
        
        ## Use these to position the new strip
        pgNew = gtable_add_grob(pg, newStripr, t = min(posr$t), l = unique(posr$l), b = max(posr$b))
        pgNew = gtable_add_grob(pgNew, newStript, l = min(post$l), r = max(post$r), t=unique(post$t))
        grid.draw(pgNew)
        
        return(pgNew)
}

# load data -----

df_au_wages <- read.csv(paste0(results, "wages_mfx_au.csv")) 
df_ch_wages <- read.csv(paste0(results, "wages_mfx_ch.csv")) 
df_de_wages <- read.csv(paste0(results, "wages_mfx_de.csv")) 
df_uk_wages <- read.csv(paste0(results, "wages_mfx_uk.csv")) 

df_au_prestige <- read.csv(paste0(results, "prestige_mfx_au.csv")) 
df_ch_prestige <- read.csv(paste0(results, "prestige_mfx_ch.csv")) 
df_de_prestige <- read.csv(paste0(results, "prestige_mfx_de.csv")) 
df_uk_prestige <- read.csv(paste0(results, "prestige_mfx_uk.csv")) 

df_wages <- rbind(df_au_wages,df_ch_wages,df_de_wages,df_uk_wages)
df_prestige <- rbind(df_au_prestige,df_ch_prestige,df_de_prestige,df_uk_prestige)

rm(df_au_wages,df_ch_wages,df_de_wages,df_uk_wages,df_au_prestige,df_ch_prestige,df_de_prestige,df_uk_prestige)

# Clean ----

df_prestige$vars <- "Prestige"
df_wages$vars <- "Hourly wage (LN)"

df_graph <- rbind(df_prestige,df_wages)
rm(df_prestige,df_wages)
df_graph$group <- gsub( "_.*$", "", df_graph$category)

df_graph$group <- factor(df_graph$group,
                         levels = c("base", "age", "edu", "female"),
                         labels = c("Baseline", "Age (ref: 35-44)",  "Edu (ref: Secondary)", "(ref: Male)"))

df_graph$category <- factor(df_graph$category,
                        levels = c(
                                "base",
                                "age_cat_1", "age_cat_3",
                                "edu_cat_1", "edu_cat_3", "female"
                        ),
                        labels = c(
                                "",
                                "25-34", "45-54",
                                "< Secondary", "> Secondary",
                                "Female"
                        ))

# Graph ----
library(grid)
library(gtable)
library(plyr)

p <- ggplot(data = df_graph, aes(x = period, y = fit, color = country)) +
        facet_grid(vars ~ group + category, scales = "free_y") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0) +
        # scale_y_continuous(breaks = c(seq(-.5, .5, by = .2)), limits = c(-.5, .5)) +
        geom_errorbar(aes(ymin = fit-1.96*se.fit,
                          ymax = fit+1.96*se.fit
                      ),
                      width=.2) +
        xlab("Years after temporary employment contract") +
        ylab("Average marginal effect") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"analyze.pdf"), height = 5, width = 7)
grid.draw(OverlappingStripLabels(p))
dev.off()

