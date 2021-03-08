import delimited "df_clean_table.csv"

* Prep data
encode city, gen(citynum)
encode theme, gen(themenum)
gen citytheme = city*10+theme

* Create interactions 
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
* Diagno - 1st stage
*********

xi: reg var_cogni_group i.theme i.city var_cogni_table, robust cluster (citytheme)
estimates store stage1


xi: reg var_trust_group i.theme i.city var_trust_table, robust cluster (citytheme)
estimates store stage2

xi: reg var_know_group i.theme i.city var_know_table, robust cluster (citytheme)
estimates store stage3


xi: reg mean_cogni_group i.theme i.city mean_cogni_table, robust cluster (citytheme)
estimates store stage4


xi: reg mean_trust_group i.theme i.city mean_trust_table, robust cluster (citytheme)
estimates store stage5

xi: reg mean_know_group i.theme i.city mean_know_table, robust cluster (citytheme)
estimates store stage6



esttab stage1 stage2 stage3 stage4 stage5 stage6, se, using "first_stage.tex", star(+ 0.10 * 0.05 ** 0.01 *** 0.001) label nonumber title("First stage correlations") mtitle("Cogni hetero" "Trust hetero" "Know hetero" "Cogni level" "Trust level" "Know level") indicate("FE City + Theme = _Icity* _Itheme*") replace


* Interactions


xi: reg var_cogni_groupxmean_cogni_group i.theme i.city var_cogni_tablexmean_cogni_table, robust cluster (citytheme)
estimates store stageinte1

xi: reg  var_trust_groupxmean_trust_group i.theme i.city  var_trust_tablexmean_trust_table, robust cluster (citytheme)
estimates store stageinte2

xi: reg var_know_groupxmean_know_group i.theme i.city var_know_tablexmean_know_table, robust cluster (citytheme)
estimates store stageinte3





esttab stageinte1 stageinte2 stageinte3  using "first_stage_inte.tex", star(+ 0.10 * 0.05 ** 0.01 *** 0.001) label nonumber title("First stage correlations - interactions") mtitle("Cognitive" "Trust" "Knowledge") indicate("FE City + Theme = _Icity* _Itheme*") replace






