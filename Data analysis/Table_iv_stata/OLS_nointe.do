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

* Controls
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"






*********
* OLS learning
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"

            
            

xi: regress learning_table i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model1


* Joint f test for heterogeneity vars

testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)




*********
* OLS opchange
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"



xi: regress op_change_table i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model2


* Joint f test for heterogeneity vars

testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)






*********
* OLS satisfaction
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"


xi: regress sat_table i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model3


* Joint f test for heterogeneity vars

testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)





*********
* OLS delib
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"


xi: regress delib_table i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model5


* Joint f test for heterogeneity vars


testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)






*********
* OLS genint
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"



xi: regress genint_table i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model7


* Joint f test for heterogeneity vars


testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)







*********
* OLS grade
*********
local controls = "pol_interest_table pol_interest_table_v partic_past_table partic_past_table_v conf_trans_table conf_trans_table_v conf_prise_table conf_prise_table_v responsabilite_pol_table responsabilite_pol_table_v conf_prise_pre_table conf_prise_pre_table_v temps_exp_pre_table temps_exp_pre_table_v delib_discuss_pre_pca_table delib_discuss_pre_pca_table_v general_interest_pre_table general_interest_pre_table_v change_avis_pre_pca_table change_avis_pre_pca_table_v consensual_pre_pca_table consensual_pre_pca_table_v delib_diverse_pre_pca_table delib_diverse_pre_pca_table_v"


xi: regress proposal_grade i.city i.theme `controls' perc_women_table var_cogni_table var_trust_table var_know_table mean_cogni_table mean_trust_table mean_know_table, robust cluster(citytheme)

estimates store model8


* Joint f test for heterogeneity vars


testparm var_cogni_table var_trust_table var_know_table 

estadd scalar p_diff = r(p)










*********
* Build table 
*********



esttab model1 model2 model3 model5 model7 model8, se, using "ols.tex", stats(N p_diff) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) label nonumber title("Table level outcomes, OLS analysis") mtitle ("Learning" "Opinion Change" "Satisfaction" "Deliberation" "General interest" "Policy grade") indicate("FE City + Theme = _Icity* _Itheme*" "Controls = pol_interest_table pol_interest_table_var partic_past_table partic_past_table_var conf_trans_table conf_trans_table_var conf_prise_table conf_prise_table_var responsabilite_pol_table responsabilite_pol_table_var conf_prise_pre_table conf_prise_pre_table_var temps_exp_pre_table temps_exp_pre_table_var delib_discuss_pre_pca_table delib_discuss_pre_pca_table_var general_interest_pre_table general_interest_pre_table_var change_avis_pre_pca_table change_avis_pre_pca_table_var consensual_pre_pca_table consensual_pre_pca_table_var delib_diverse_pre_pca_table delib_diverse_pre_pca_table_var") replace


