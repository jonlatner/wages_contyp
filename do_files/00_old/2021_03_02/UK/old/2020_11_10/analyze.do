use "/Users/jonathanlatner/Google Drive/SECCOPA/projects/booth_etal_2002/data_files/update/bhps.dta", clear

keep if male == 1

xtset pid year

xtreg ln_wages i.contyp##c.age i.contyp##c.age_2 i.year
margins, at(age==(20(10)50) contyp=(1 2 3))
marginsplot

xtreg prestige i.contyp##c.age i.contyp##c.age_2 i.year
margins, at(age==(20(10)50) contyp=(1 2 3))
marginsplot

xtfeis ln_wages i.contyp age i.year, cluster(pid)

xtfeis ln_wages i.contyp i.year, slope(age) cluster(pid)
