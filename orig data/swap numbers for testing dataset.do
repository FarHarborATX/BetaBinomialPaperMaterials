import excel "G:\Shared drives\_projects\x_fh_special\Methods paper related\Github files\Orig sample data.xlsx", sheet("Sheet1") firstrow case(lower)

sample 50, count

foreach v of varlist no_mostmod yes_mostmod no_larc yes_larc total {
	gen `v'1= `v'*1.1 in 1/25
	replace `v'1= `v'/1.1 if `v'1==.
	replace `v'1 = round(`v'1)
	drop `v'
	rename `v'1 `v'
}

gen rate_mostmod = yes_mostmod/total
gen rate_larc = yes_larc/total

format rate_mostmod rate_larc %9.3f