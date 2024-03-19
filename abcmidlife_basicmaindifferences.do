/*
Project:				AR Perry CBA
Author:					Jorge Luis García (jlgarci@clemson.edu)
Original Date:			March 18, 2024
This file:				basic mean differences, Carolina Abecedarian Project
*/

//options
set more off, permanently
	
global baseline   m_iq_base hrabc_index mwork birthyear
global skills     cognitionfactor positiveind 
global education  hs30 ccol45
global marriage   married45
global income     mon_earnings_year meanearn
global crime      si30_adlt_num_jail soc_arrestednmbr crimefactor
global health     health
global mentalhea  mentalh
global children   nochild onset numchild more5child
global all        $skills $education $marriage $income $crime $health $mentalhea

// reverse crime outcomes for inference
foreach var of varlist $crime {
	replace `var' = -`var'
}

global sex0 male == 0
global sex1 male == 1
global sex2 male !=.

// baseline
matrix all = J(1,9,.)
foreach var of varlist $baseline {
	foreach sex of numlist 0 1 2 { 
		// control mean
		summ `var' if ${sex`sex'} & treat == 0
		matrix m`var'_s`sex' = r(mean)
		
		// mean difference
		reg `var' treat if ${sex`sex'}
		matrix b = e(b)
		matrix md`var'_s`sex' = b[1,1]
		
		// permutation p -value
		ritest treat _b[treat], reps(1000) seed(1): reg `var' treat if ${sex`sex'}
		matrix   p`var'_s`sex' = r(p)
		
		// all in a matrix
		matrix `var'_`sex' = [m`var'_s`sex',md`var'_s`sex',p`var'_s`sex']
	}
	matrix `var' = [`var'_2,`var'_1,`var'_0]
	matrix all = [all \ `var']
}
// outcomes
foreach var of varlist $all {
	foreach sex of numlist 0 1 2 { 
		// control mean
		summ `var' if ${sex`sex'} & treat == 0
		matrix m`var'_s`sex' = r(mean)
		
		// mean difference
		reg `var' treat if ${sex`sex'}
		matrix b = e(b)
		matrix md`var'_s`sex' = b[1,1]
		
		// permutation p -value
		ritest treat _b[treat], reps(1000) seed(1) right: reg `var' treat if ${sex`sex'}
		matrix   p`var'_s`sex' = r(p)
		
		// all in a matrix
		matrix `var'_`sex' = [m`var'_s`sex',md`var'_s`sex',p`var'_s`sex']
	}
	matrix `var' = [`var'_2,`var'_1,`var'_0]
	matrix all = [all \ `var']
}
// fertility
foreach var of varlist $children {
	foreach sex of numlist 0 1 2 { 
		// control mean
		summ `var' if ${sex`sex'} & treat == 0
		matrix m`var'_s`sex' = r(mean)
		
		// mean difference
		reg `var' treat if ${sex`sex'}
		matrix b = e(b)
		matrix md`var'_s`sex' = b[1,1]
		
		// permutation p -value
		ritest treat _b[treat], reps(1000) seed(1): reg `var' treat if ${sex`sex'}
		matrix   p`var'_s`sex' = r(p)
		
		// all in a matrix
		matrix `var'_`sex' = [m`var'_s`sex',md`var'_s`sex',p`var'_s`sex']
	}
	matrix `var' = [`var'_2,`var'_1,`var'_0]
	matrix all = [all \ `var']
}
matrix all = all[2...,1...]
matrix colnames all = pool_c pool_md pool_p male_c male_md male_p female_c female_md female_p 
clear
svmat all, names(col)
mat list all
