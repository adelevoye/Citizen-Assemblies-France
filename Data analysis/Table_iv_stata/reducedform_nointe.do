import delimited "df_clean_table.csv"

* Prep data
encode city, gen(citynum)
encode theme, gen(themenum)
gen citytheme = city*10+theme

* Create interactions vars
gen var_cogni_groupxmean_cogni_group = mean_cogni_group*var_cogni_group
gen var_cogni_tablexmean_cogni_table = var_cogni_table*mean_cogni_table

gen var_trust_groupxmean_trust_group = mean_trust_group*var_trust_group
gen var_trust_tablexmean_trust_table = var_trust_table*mean_trust_table

gen var_know_groupxmean_know_group = mean_know_group*var_know_group
gen var_know_tablexmean_know_table = var_know_table*mean_know_table



* Labels for table

label variable perc_women_table "Percentage women table"

label variable mean_cogni_table "Cognitive level table"

label variable var_cogni_table "Cognitive heterogeneity table"

label variable var_know_tablexmean_know_table "Know heterogeneity X know level"

label variable var_trust_tablexmean_trust_table "Trust heterogeneity x trust level"

label variable var_know_tablexmean_know_table "Know heterogeneity x know level"

label variable var_cogni_tablexmean_cogni_table "Cognitive heterogeneity x cognitive level"

label variable mean_trust_table "Trust level table"

label variable var_trust_table "Trust heterogeneity table"

label variable mean_know_table "Knowledge level table"

label variable var_know_table "Knowledge heterogeneity table"



*********
* Learning
*********


 
xi: regress learning_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 


estimates store model1




* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)









*********
* Opchange
*********
 

xi: regress op_change_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 



estimates store model2



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)








*********
* Satisfaction
*********


xi: regress sat_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 




estimates store model3



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)









*********
* Diverse
*********



xi: regress diverse_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 




estimates store model4



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)








*********
* Delib
*********


xi: regress delib_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 




estimates store model5



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)






*********
* Consensus
*********


xi: regress cons_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 



estimates store model6



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)





*********
* Genint
*********


xi: regress genint_table i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 



estimates store model7



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)










*********
* Grade
*********


xi: regress proposal_grade i.city i.theme perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group, robust cluster(citytheme) 




estimates store model8



* First stage 
* Check with sum stat is better

estadd scalar ffirst = e(widstat)


* Joint f test for heterogeneity vars

testparm perc_women_group var_cogni_group var_trust_group var_know_group mean_cogni_group mean_trust_group mean_know_group 
estadd scalar p_diff = r(p)












*********
* Build table 
*********

* (Need to add first stage)
* (Need to figure out how to deal with both controls and FEs)



esttab model1 model2 model3 model4 model5 model6 model7 model8, se, using "reducedform.tex", stats(N p_diff) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) label nonumber title("Reduced form results, IV analysis") mtitle ("Learning" "Opinion Change" "Satisfaction" "Appreciation diversity" "Deliberation" "Consensus" "General interest" "Policy grade") indicate("FE City + Theme = _Icity* _Itheme*") replace




