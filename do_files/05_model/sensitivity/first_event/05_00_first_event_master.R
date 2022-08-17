# Top commands ----

# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/first_event/05a_model_first_event_contyp.R", echo = FALSE)
source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/first_event/05b_model_first_event_unmp.R", echo = FALSE)


