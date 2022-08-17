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

source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/AU_FTC/02_00_AU_FTC_master.R", echo = FALSE)
source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/FEIS/05_00_FEIS_master.R", echo = FALSE)
source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/first_event/05_00_first_event_master.R", echo = FALSE)
source("~/GitHub/wages_contyp/do_files/05_model/sensitivity/UK_FTC/02_00_UK_FTC_master.R", echo = FALSE)


