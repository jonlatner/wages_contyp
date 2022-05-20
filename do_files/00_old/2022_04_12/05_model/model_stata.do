set more off
* Load data
cd  "/Users/jonathanlatner/Google Drive/SECCOPA/projects/mobility/data_files/"    //Adapt this path!

use df_sample_clean.dta, clear

keep if country=="DE"

order country pid year
sort country pid year

***** Declare data to be panel data *****
xtset pid year

* Event - temp to perm
local event_t_p_vars = "age unemployment_rate ib2.event_t_p_time_pos i.year"
qui xtreg hourly_wage `event_t_p_vars', fe robust cluster(pid)
est store event_t_p

* Event - perm to temp
local event_p_t_vars = "age unemployment_rate ib2.event_p_t_time_pos i.year"
qui xtreg hourly_wage `event_p_t_vars', fe robust cluster(pid)
est store event_p_t

* Event - unmp to perm
local event_u_p_vars = "age unemployment_rate ib2.event_u_p_time_pos i.year"
qui xtreg hourly_wage `event_u_p_vars', fe robust cluster(pid)
est store event_u_p

* Event - unmp to temp
local event_u_t_vars = "age unemployment_rate ib2.event_u_t_time_pos i.year"
qui xtreg hourly_wage `event_u_t_vars', fe robust cluster(pid)
est store event_u_t


set more off
#delimit ;
esttab  event_t_p  event_p_t  event_u_p  event_u_t, drop(*.year 0.* 2.* _cons) b se 
rename(
1.event_p_t_time_pos  1.event_t_p_time_pos 1.event_u_p_time_pos  1.event_t_p_time_pos 1.event_u_t_time_pos  1.event_t_p_time_pos
3.event_p_t_time_pos  3.event_t_p_time_pos 3.event_u_p_time_pos  3.event_t_p_time_pos 3.event_u_t_time_pos  3.event_t_p_time_pos
4.event_p_t_time_pos  4.event_t_p_time_pos 4.event_u_p_time_pos  4.event_t_p_time_pos 4.event_u_t_time_pos  4.event_t_p_time_pos
5.event_p_t_time_pos  5.event_t_p_time_pos 5.event_u_p_time_pos  5.event_t_p_time_pos 5.event_u_t_time_pos  5.event_t_p_time_pos
6.event_p_t_time_pos  6.event_t_p_time_pos 6.event_u_p_time_pos  6.event_t_p_time_pos 6.event_u_t_time_pos  6.event_t_p_time_pos
7.event_p_t_time_pos  7.event_t_p_time_pos 7.event_u_p_time_pos  7.event_t_p_time_pos 7.event_u_t_time_pos  7.event_t_p_time_pos
)
varlabels(
1.event_t_p_time_pos "Pre event (-2)"
3.event_t_p_time_pos "Event"
4.event_t_p_time_pos "Post event (+1)"
5.event_t_p_time_pos "Post event (+2)"
6.event_t_p_time_pos "Post event (+3)"
7.event_t_p_time_pos "Post event (+4)"
)
;
