rm(list = ls())


library(readr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)
library(psych)
library(patchwork)



df <- read.csv("Data/dataset_clean.csv")


# Clean remaining typos


df$city[df$city == "DIjon"] <- "Dijon"




# Build the independent variables ----------------------------------------------------

###############################
# Average baseline knowledge
################################

# Compute average baseline knowledge across 4 topics for each participant

# Convert vars to numeric
typeof(df_fake$know_pre_etat)

df$know_pre_dem <- as.numeric(as.character(df$know_pre_dem))
df$know_pre_fis <- as.numeric(as.character(df$know_pre_fis))
df$know_pre_eco <- as.numeric(as.character(df$know_pre_eco))
df$know_pre_etat <- as.numeric(as.character(df$know_pre_etat))


df_inter <- df %>%
  select(know_pre_dem,know_pre_fis,know_pre_eco,know_pre_etat)

knowledge_pre <-  rowMeans(df_inter, na.rm = TRUE)

knowledge_pre[is.nan(knowledge_pre)] <- NA

df$knowledge_pre <-  knowledge_pre



###############################
# Average baseline interest
################################

df$int_pre_dem <- as.numeric(as.character(df$int_pre_dem))
df$int_pre_fis <- as.numeric(as.character(df$int_pre_fis))
df$int_pre_eco <- as.numeric(as.character(df$int_pre_eco))
df$int_pre_etat <- as.numeric(as.character(df$int_pre_etat))


df_inter_int <- df %>%
  select(int_pre_dem,int_pre_fis,int_pre_eco,int_pre_etat)


interest_pre <-  rowMeans(df_inter_int, na.rm = TRUE)

interest_pre[is.nan(interest_pre)] <- NA

df$interest_pre = interest_pre





############################################
# Gender homogeneity of the table
############################################
# We will use the percentage of women at one's table as a continuous variable. 

df_table_gender <- df_fake %>%
  group_by (table_code) %>%
  summarise (perc_women_table = mean(gender, na.rm = T))

# For regression results to make sense, need this from 0 to 100
df_table_gender$perc_women_table = (df_table_gender$perc_women_table)*100

# Add this new var in big df

df_fake <- merge (df_fake, df_table_gender)

# Visualize 
hist(df_fake$perc_women_table)



############################################
# Cognitive homogeneity of the table
############################################

# Assign each reported occupation to one of 3 cognitive groups (see paper)

intellectual <- c('cadre', 'fonctionnaire', 'etudiant')
intermediate <- c('intermediaire', 'emp_qualifie', 'ouv_qualifie', 'retraite')
manual <- c('agriculteur', 'emp_non', 'ouv_non', 'sans_emploi', 'artisan')



# Add category to df

df <- df %>%
  mutate (cogni_indiv = case_when (occupation %in% intellectual ~ 'intellectual',
                                        occupation %in% intermediate ~ 'intermediate',
                                        occupation %in% manual ~ 'manual'))

# Add a numerical factor for these categories

df <- df %>%
  mutate (cogni_indiv_code = case_when (occupation %in% intellectual ~ 1,
                                        occupation %in% intermediate ~ 0,
                                        occupation %in% manual ~ -1))



# Get mean and var of cognitive categories at every table
df_status_table_code <- df %>% 
  group_by(table_code) %>% 
  summarize(mean_cogni_table = mean(cogni_indiv_code, na.rm = T),
            var_cogni_table = sqrt(var(cogni_indiv_code, na.rm = T))
 )


# Add table average to df
df <- merge (df, df_status_table_code)


############################################
# Homogeneity in trust in people of the table
############################################

# Use existing variable from answer to entry survey
df_trust_table_code <- df %>% 
  group_by(table_code) %>% 
  summarize(mean_trust_table = mean(confiance_pre, na.rm = T),
            var_trust_table = sqrt(var(confiance_pre, na.rm = T))
  )


df <- merge (df, df_trust_table_code)


# Plot the distributions


plot_trust_baseline <- ggplot (mapping = aes(df$confiance_pre)) +
  geom_bar(fill="mediumpurple4", alpha = .5) +
  theme_bw() +
  xlab ("Individuals") +
  ylab ("Count") 

plot_trust_baseline




plot_trust_baseline_table_lev <- ggplot (mapping = aes(df$mean_trust_table)) +
  geom_histogram(bins = 70, fill="mediumpurple4", alpha = .5) +
  theme_bw() +
  xlab ("Tables, means") +
  ylab ("Count") 

plot_trust_baseline_table_lev


plot_trust_baseline_table_var <- ggplot (mapping = aes(df$var_trust_table)) +
  geom_histogram(bins = 70, fill="mediumpurple4", alpha = .5) +
  theme_bw() +
  xlab ("Tables, standard deviations") +
  ylab ("Count") 

plot_trust_baseline_table_var



plots_baseline_trust <- (plot_trust_baseline | plot_trust_baseline_table_lev | plot_trust_baseline_table_var) 

plots_baseline_trust




############################################
# Homogeneity in initial knowledge
############################################



df_know_table_code <- df %>% 
  group_by(table_code) %>% 
  summarize(mean_know_table = mean(knowledge_pre, na.rm = T),
            var_know_table = sqrt(var(knowledge_pre, na.rm = T))
  )


df <- merge (df, df_know_table_code)


plot_know_baseline <- ggplot (mapping = aes(df$knowledge_pre)) +
  geom_bar(fill="steelblue4", alpha = .5) +
  theme_bw() +
  xlab ("Individuals") +
  ylab ("Count") 

plot_know_baseline




plot_know_baseline_table_lev <- ggplot (mapping = aes(df$mean_know_table)) +
  geom_histogram(bins = 70, fill="steelblue4", alpha = .5) +
  theme_bw() +
  xlab ("Tables, means") +
  ylab ("Count") 

plot_know_baseline_table_lev


plot_know_baseline_table_var <- ggplot (mapping = aes(df$var_know_table)) +
  geom_histogram(bins = 70, fill="steelblue4", alpha = .5) +
  theme_bw() +
  xlab ("Tables, standard deviations") +
  ylab ("Count") 

plot_know_baseline_table_var



plots_baseline_know <- (plot_know_baseline | plot_know_baseline_table_lev | plot_know_baseline_table_var) 

plots_baseline_know




# Build the dependent variables -------------------------------------------

############################################
# Learning summary variable:learning_sum_pca
############################################
# Summary variable substantially = learning gains from participation, using the following survey questions:
#How much do you know on the topic you worked on?} Grade 0-10, we take the difference between the entry and exit surveys
#How much did you learn during the course of this Assembly?} Grade 0-10, exit survey only

# Difference between answers in exit and entry surveys
df <- df %>%
  mutate (knowledge_gain_eco = ifelse (double_answer == 1, NA, know_post_eco-know_pre_eco),
          knowledge_gain_dem = ifelse (double_answer == 1, NA, know_post_dem-know_pre_dem),
          knowledge_gain_fis  = ifelse (double_answer == 1, NA, know_post_fis-know_pre_fis),
          knowledge_gain_etat = ifelse (double_answer == 1, NA, know_post_etat-know_pre_etat)
  )

# Knowledge gain in topic that the participant discussed during the Assembly
df <- df %>%
  mutate (knowledge_gain = ifelse(theme == 'TE', knowledge_gain_eco,
                                  ifelse(theme == 'ESP', knowledge_gain_etat,
                                         ifelse(theme == 'FDP', knowledge_gain_fis,
                                                ifelse(theme == 'DC',knowledge_gain_dem,
                                                       NA)))))
            

# Build summary variable from the 2 questions of interest through PCA
# Train model

learning_sum <- psych::principal(df %>% 
                                 select(knowledge_gain, appris), 
                               nfactors = 2, rotate = "varimax")

# Build summary var

df <- within(df,{
  learning_sum_pca <- predict(learning_sum, data = df %>% 
                                   select(knowledge_gain, appris))[,1]})

df$learning_sum_pca

# Normalize this variable: divide each obs by their sd

df$learning_sum_pca <- scale(df$learning_sum_pca, center = F, scale = T)

# Rescale from - 10 to 10
df$learning_sum_pca <- scales::rescale(df$learning_sum_pca, to = c(-10,10), 
                                             from =range(df$learning_sum_pca, 
                                                         na.rm = TRUE, finite = TRUE))




# Visualize distrib of summary var

plot_learn <- ggplot (mapping = aes(df$learning_sum_pca)) +
  geom_histogram(bins = 70, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Learning") +
  ylab ("Count") 

plot_learn




############################################
# Opinion change summary variable: op_change_sum_pca
############################################
# Summary variable substantially = whether deliberation lead to opinion changes, from 0 to +10. Built from following questions:

# Participants won't/weren't willing to change their minds, even when good arguments were put forward} 
# Grade 0-10, we will only take exit survey result  

#Did the discussions over the weekend led you to change your opinion on any given 
#topic that was discussed?} (yes/no, exit survey). 


# Need num vars
df$change_avis_post <- as.numeric(as.character(df$change_avis_post))
df$change_avis_pre <- as.numeric(as.character(df$change_avis_pre))
df$chgt_avis <- as.numeric(as.character(df$chgt_avis))


df$chgt_avis[df$chgt_avis == 9] <- NA


# Participants were asked how much people were NOT willing to change their minds. Need the inverse
df <- df %>%
  mutate (change_avis_post_pca = 10 - change_avis_post) 


df <- df %>%
  mutate (change_avis_pre_pca = 10 - change_avis_pre) 


# Fit PCA model
op_change_sum <- psych::principal(df %>% 
                                   select(chgt_avis, change_avis_post_pca), 
                                 nfactors = 2, rotate = "varimax")

# Build PCA var

df <- within(df,{
  op_change_sum_pca <- predict(op_change_sum, data = df %>% 
                                select(chgt_avis, change_avis_post_pca))[,1]})



# Normalize and rescale variable

df$op_change_sum_pca <- scale(df$op_change_sum_pca, center = F, scale = T)


# Negative numbers don't mean anything here, need to rescale
df$op_change_sum_pca <- scales::rescale(df$op_change_sum_pca, to = c(0,10), 
                                                 from =range(df$op_change_sum_pca, 
                                                             na.rm = TRUE, finite = TRUE))


# Visualize distribution

plot_op <- ggplot (mapping = aes(df$op_change_sum_pca)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Opinion change") +
  ylab ("Count") 

plot_op



############################################
# Sat levels summary variable
############################################


# Summary variable substantially = whether deliberation led to opinion changes, from 0 to 10
# Question was asked directly in survey
plot_sat <- ggplot (mapping = aes(df$sat_level_gen)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Satisfaction") +
  ylab ("Count") 

plot_sat



############################################
# Deliberative skills 1 summary variable = delib_diverse
############################################
# Substantially: Appreciation for diverse voices and opinions, built from following questions:

# I enjoy discussing with people with different opinions than mine} 
#Grade 0-10,

#Disagreements stimulate discussions and make people communicate more and better} 
#Grade 0-10, 

#I don't like finding myself in situations with a lot of disagreements} 
#Grade 0-10, we will subtract from 10

#I would rather be in groups where people share my opinions} 
#Grade 0-10, we we will subtract from 10


# Inverse vars that need it substantially 
df <- df %>%
  mutate (homogen_pre_pca = 10 - homogen_pre, 
          homogen_post_pca = 10 - homogen_post, 
          desaccord_aime_pre_pca = 10 - desaccord_aime_pre,
          desaccord_aime_post_pca = 10 - desaccord_aime_post
          ) 

# Fit PCA model on answers to entry survey
# Use this model to build 2 PCA vars: one for exit answers, one entry answers 

# Fit PCA model with entry survey 
delib_diverse_sum <- psych::principal(df %>% 
                                    select(discuss_pre, desaccord_aime_pre_pca,
                                           desaccord_stimule_pre, homogen_pre_pca), 
                                  nfactors = 1, rotate = "varimax")




# Extract summary var for pre survey

df <- within(df,{
  delib_diverse_pre_pca <- predict(delib_diverse_sum, data = df %>% 
                                 select(discuss_pre, desaccord_aime_pre_pca,
                                        desaccord_stimule_pre, homogen_pre_pca))[,1]})

# Extrat summary var for post survey
df <- within(df,{
  delib_diverse_post_pca <- predict(delib_diverse_sum, data = df %>% 
                                     select(discuss_post, desaccord_aime_post_pca,
                                            desaccord_stimule_post, homogen_post_pca))[,1]})





# Normalize and Rescale form 0 to 10

df$delib_diverse_pre_pca <- scale(df$delib_diverse_pre_pca, center = F, scale = T)


df$delib_diverse_pre_pca <- scales::rescale(df$delib_diverse_pre_pca, to = c(0,10), 
                                             from =range(df$delib_diverse_pre_pca, 
                                                         na.rm = TRUE, finite = TRUE))

df$delib_diverse_post_pca <- scale(df$delib_diverse_post_pca, center = F, scale = T)


df$delib_diverse_post_pca <- scales::rescale(df$delib_diverse_post_pca, to = c(0,10), 
                                                 from =range(df$delib_diverse_post_pca, 
                                                             na.rm = TRUE, finite = TRUE))




# Visualize variable (exit survey)
plot_diverse <- ggplot (mapping = aes(df$delib_diverse_post_pca)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Apprec. for diversity") +
  ylab ("Count") 

plot_diverse




############################################
# Deliberative skills 2 summary variable = delib_discuss
############################################

# Subtantially, deal  with  participants’ confidence that people are able to lead a productive discussion,
# Listen to each other, be respectful, be open to new arguments. Build from following questions:


# I won't/did not dare saying what I really think, for fear of looking ridiculous} 
# Grade 0-10, we substract from 10 

#Other participants won't/weren't willing to listen to each other, they're only here to defend their point of view} 
# Grade 0-10, we substract from 10 

# Participants will be/were sincere and won't/didn't hide what they really think} 
#Grade 0-10, 

#Most participants will be/were respectful to each other} 
# Grade 0-10, 

#I came here to express specific claims and demands and convince other participants} 
#Grade 0-10, we substract from 10 


# Need the inverse 

df <- df %>%
  mutate (oser_pre_pca = 10 - oser_pre,
          oser_post_pca = 10 - oser_post,
          ecoute_pre_pca = 10 - ecoute_pre,
          ecoute_post_pca = 10 - ecoute_post,
          revendic_pre_pca = 10 - revendic_pre,
          revendic_post_pca = 10 - revendic_post)


# Fit PCA with entry answers
delib_discuss_sum <- psych::principal(df %>% 
                                        select(oser_pre_pca, ecoute_pre_pca,
                                               sincere_pre, respect_pre,
                                               revendic_pre_pca), 
                                      nfactors = 2, rotate = "varimax")



# Extract summary var for entry survey

df <- within(df,{
  delib_discuss_pre_pca <- predict(delib_discuss_sum, data = df %>% 
                                     select(oser_pre_pca, ecoute_pre_pca,
                                            sincere_pre, respect_pre,
                                            revendic_pre_pca))[,1]})


# Extract summary var for exit survey

df <- within(df,{
  delib_discuss_post_pca <- predict(delib_discuss_sum, data = df %>% 
                                     select(oser_post_pca, ecoute_post_pca,
                                            sincere_post, respect_post,
                                            revendic_post_pca))[,1]})



# Normalize and Rescale form 0 to 10

df$delib_discuss_pre_pca <- scale(df$delib_discuss_pre_pca, center = F, scale = T)


df$delib_discuss_pre_pca <- scales::rescale(df$delib_discuss_pre_pca, to = c(0,10), 
                                                 from =range(df$delib_discuss_pre_pca, 
                                                             na.rm = TRUE, finite = TRUE))

                                                                                                                         na.rm = TRUE, finite = TRUE))

df$delib_discuss_post_pca <- scale(df$delib_discuss_post_pca, center = F, scale = T)


df$delib_discuss_post_pca <- scales::rescale(df$delib_discuss_post_pca, to = c(0,10), 
                                                 from =range(df$delib_discuss_post_pca, 
                                                             na.rm = TRUE, finite = TRUE))


# Summary variable substantially = confidence 
# in ability to deliberative nature of discussion, 0-10


plot_delib <- ggplot (mapping = aes(df$delib_discuss_post_pca)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Deliberative quality") +
  ylab ("Count") 

plot_delib




############################################
# Consensual nature variable
############################################

# Substantially: consensual orientation of discussion. Built from following Qs: 

# It will be/was hard, almost impossible, to come up with proposals on which everybody agree} 
# Grade 0-10, we substract from 10 


#People will have/did have very different opinions on topics that will be/were discussed} 
#Grade 0-10, we subtract from 10 


df$diff_consensus_pre  <- as.numeric(df$diff_consensus_pre)
df$diff_consensus_post  <- as.numeric(df$diff_consensus_post)
df$diff_op_pre  <- as.numeric(df$diff_op_pre)
df$diff_op_post_pca  <- as.numeric(df$diff_op_post_pca)


# Need the inverse 

df <- df %>%
  mutate (diff_consensus_pre_pca = 10 - diff_consensus_pre,
          diff_consensus_post_pca = 10 - diff_consensus_post,
          diff_op_pre_pca = 10 - diff_op_pre,
          diff_op_post_pca = 10 - diff_op_post,
  ) 


# Fit PCA model with entry answers
consensual_vars_sum <- psych::principal(df %>% 
                                        select(diff_consensus_pre_pca, diff_op_pre_pca), 
                                      nfactors = 2, rotate = "varimax")



# Extract summary var for entry survey

df <- within(df,{
  consensual_pre_pca <- predict(consensual_vars_sum, data = df %>% 
                                     select(diff_consensus_pre_pca, diff_op_pre_pca))[,1]})

# Extract summary var for exit survey

df <- within(df,{
  consensual_post_pca <- predict(consensual_vars_sum, data = df %>% 
                                       select(diff_consensus_post_pca, diff_op_post_pca))[,1]})





# Normalize and Rescale form 0 to 10


df$consensual_pre_pca <- scales::rescale(df$consensual_pre_pca, to = c(0,10), 
                                                 from =range(df$consensual_pre_pca, 
                                                             na.rm = TRUE, finite = TRUE))





df$consensual_post_pca <- scales::rescale(df$consensual_post_pca, to = c(0,10), 
                                                   from =range(df$consensual_post_pca, 
                                                               na.rm = TRUE, finite = TRUE))


# Summary variable substantially = confidence in ability to lead consensual nature of discussion, 0:-10


plot_cons <- ggplot (mapping = aes(df$consensual_post_pca)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Consensual orientation") +
  ylab ("Count") 

plot_cons



############################################
# General interest orientation variable
############################################
# General interest oritentaion of discussion, 0 to 10 
# Based on: perso_gain




df <- df %>%
  mutate(general_interest_pre = 10 - perso_pre,
         general_interest_post = 10 - perso_post,
         )


plot_genint <- ggplot (mapping = aes(df$general_interest_post)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("General interest") +
  ylab ("Count") 

plot_genint


# Summary plot for distributions of DVs

plots_baselines_dv <- (plot_learn | plot_op | plot_sat) /
  (plot_diverse | plot_delib | plot_cons) /
  (plot_genint) 

plots_baselines_dv





############################################
# Policy proposal grade
############################################
# Need to divide total number of stars received by a policy proposal 
# by nbr of tables in each city (see nbr in report)

# So each table code = a total number of stars / number of tables giving stars out

df <- df %>%
  mutate (policy_grade = case_when(table_code == 'LYO8' ~ 40/14,
                                   table_code == 'LYO10' ~ 29/14,
                                   table_code == 'LYO15' ~ 26/14,
                                   table_code == 'LYO12' ~ 31/14,
                                   table_code == 'LYO3' ~ 38.5/14,
                                   table_code == 'LYO6' ~ 33/14,
                                   table_code == 'LYO11' ~ 33/14,
                                   table_code == 'LYO4' ~ 20/14,
                                   table_code == 'LYO7' ~ 41/14,
                                   table_code == 'LYO9' ~ 33/14,
                                   table_code == 'LYO13' ~ 19.5/14,
                                   table_code == 'LYO14' ~ 33/14,
                                   table_code == 'LYO16' ~ 18/14,
                                   table_code == 'LYO2' ~ 19/14,
                                   table_code == 'AIX1' ~ 24/12,
                                   table_code == 'AIX2' ~ 31/12,
                                   table_code == 'AIX3' ~ 25/12,
                                   table_code == 'AIX5' ~ 32/12,
                                   table_code == 'AIX6' ~ 15/12,
                                   table_code == 'AIX7' ~ 13/12,
                                   table_code == 'AIX9' ~ 21/12,
                                   table_code == 'AIX10' ~ 36/12,
                                   table_code == 'AIX11' ~ 29/12,
                                   table_code == 'AIX13' ~ 22/12,
                                   table_code == 'AIX15' ~ 27/12,
                                   table_code == 'AIX16' ~ 29/12,
                                   table_code == 'AJA16' ~ 22/11,
                                   table_code == 'AJA10' ~ 28/11,
                                   table_code == 'AJA14' ~ 18/11,
                                   table_code == 'AJA3' ~ 29/11,
                                   table_code == 'AJA1' ~ 21/11,
                                   table_code == 'AJA11' ~ 24/11,
                                   table_code == 'AJA12' ~ 32/11,
                                   table_code == 'AJA2' ~ 23/11,
                                   table_code == 'AJA5' ~ 27/11,
                                   table_code == 'AJA6' ~ -99,
                                   table_code == 'AJA7' ~ -99,
                                   table_code == 'DIJ 3' ~ 21/16,
                                   table_code == 'DIJ 5' ~ 31/16,
                                   table_code == 'DIJ 7' ~ 16/16,
                                   table_code == 'DIJ 2' ~ 20/16,
                                   table_code == 'DIJ 4' ~ 19/16,
                                   table_code == 'DIJ 6' ~ 23/16,
                                   table_code == 'DIJ 14' ~ 30/16,
                                   table_code == 'DIJ 15' ~ -99,
                                   table_code == 'DIJ 1' ~ 20/16,
                                   table_code == 'DIJ 9' ~ 30/16,
                                   table_code == 'DIJ 13' ~ 19/16,
                                   table_code == 'DIJ 10' ~ 23/16,
                                   table_code == 'DIJ 11' ~ 24/16,
                                   table_code == 'DIJ 12' ~ 19/16,
                                   table_code == 'LIL1' ~ 22/12,
                                   table_code == 'LIL2' ~ 18/12,
                                   table_code == 'LIL3' ~ 22/12,
                                   table_code == 'LIL5' ~ 28/12,
                                   table_code == 'LIL6' ~ 21/12,
                                   table_code == 'LIL7' ~ 20/12,
                                   table_code == 'LIL9' ~ 25/12,
                                   table_code == 'LIL10' ~ 18/12,
                                   table_code == 'LIL11' ~ 30/12,
                                   table_code == 'LIL13' ~ 19/12,
                                   table_code == 'LIL14' ~ 31/12,
                                   table_code == 'LIL15' ~ 24/12,
                                   table_code == 'MAR 1' ~ 21/12,
                                   table_code == 'MAR 2' ~ 21/12,
                                   table_code == 'MAR 3' ~ 16/12,
                                   table_code == 'MAR 5' ~ 29/12,
                                   table_code == 'MAR 6' ~ -99,
                                   table_code == 'MAR7' ~ 18/12,
                                   table_code == 'MAR9' ~ 26/12,
                                   table_code == 'MAR10' ~ 18/12,
                                   table_code == 'MAR11' ~ 20/12,
                                   table_code == 'MAR13' ~ 22/12,
                                   table_code == 'MAR14' ~ 27/12,
                                   table_code == 'MAR15' ~ 13/12,
                                   table_code == 'NAN1' ~ -99,
                                   table_code == 'NAN2' ~ -99,
                                   table_code == 'NAN3' ~ 29/17,
                                   table_code == 'NAN4' ~ 28/17,
                                   table_code == 'NAN5' ~ 28/17,
                                   table_code == 'NAN6' ~ 30/17,
                                   table_code == 'NAN7' ~ 37/17,
                                   table_code == 'NAN8' ~ -99,
                                   table_code == 'NAN9' ~ -99,
                                   table_code == 'NAN10' ~ -99,
                                   table_code == 'NAN11' ~ -99,
                                   table_code == 'NAN12' ~ -99,
                                   table_code == 'NAN13' ~ -99,
                                   table_code == 'NAN14' ~ -99,
                                   table_code == 'NAN15' ~ -99,
                                   table_code == 'NAN16' ~ -99,
                                   table_code == 'ORL9' ~ 17/12,
                                   table_code == 'ORL11' ~ 25/12,
                                   table_code == 'ORL7' ~ -99,
                                   table_code == 'ORL1' ~ -99,
                                   table_code == 'ORL10' ~ -99,
                                   table_code == 'ORL13' ~ 17/12,
                                   table_code == 'ORL15' ~ 17.5/12,
                                   table_code == 'ORL3' ~ 32/12,
                                   table_code == 'ORL6' ~ -99,
                                   table_code == 'ORL14' ~ -99,
                                   table_code == 'ORL5' ~ -99,
                                   table_code == 'ORL2' ~ -99,
                                   table_code == 'PAR1' ~ 40/18,
                                   table_code == 'par2' ~ 20/18,
                                   table_code == 'PAR3' ~ 25/18,
                                   table_code == 'PAR4' ~ 40/18,
                                   table_code == 'PAR5' ~ 29/18,
                                   table_code == 'par6' ~ 19/18,
                                   table_code == 'PAR7' ~ 30/18,
                                   table_code == 'PAR8' ~ 25/18,
                                   table_code == 'PAR9' ~ 36/18,
                                   table_code == 'PAR10' ~ 45/18,
                                   table_code == 'PAR11' ~ 39/18,
                                   table_code == 'PAR12' ~ 31/18,
                                   table_code == 'PAR13' ~ 35/18,
                                   table_code == 'PAR14' ~ 33/18,
                                   table_code == 'PAR15' ~ 38/18,
                                   table_code == 'PAR16' ~ 11/18,
                                   table_code == 'PAR17' ~ 26/18,
                                   table_code == 'PAR18' ~ 21/18,
                                   table_code == 'REN1' ~ 27/14,
                                   table_code == 'REN2' ~ 20/14,
                                   table_code == 'REN3' ~ 29/14,
                                   table_code == 'REN4' ~ 33/14,
                                   table_code == 'REN5' ~ 31/14,
                                   table_code == 'REN6' ~ 30/14,
                                   table_code == 'REN7' ~ 12/14,
                                   table_code == 'REN8' ~ 28/14,
                                   table_code == 'REN9' ~ 41/14,
                                   table_code == 'REN10' ~ 36/14,
                                   table_code == 'REN11' ~ 41/14,
                                   table_code == 'REN13' ~ 24/14,
                                   table_code == 'REN14' ~ 25/14,
                                   table_code == 'REN15' ~ 32/14,
                                   table_code == 'ROU14' ~ 24/12,
                                   table_code == 'ROU10' ~ 26.5/12,
                                   table_code == 'ROU11' ~ 18/12,
                                   table_code == 'ROU15' ~ 26/12,
                                   table_code == 'ROU2' ~ 21/12,
                                   table_code == 'ROU3' ~ 17/12,
                                   table_code == 'ROU6' ~ 23/12,
                                   table_code == 'ROU9' ~ 25/12,
                                   table_code == 'ROU1' ~ 30/12,
                                   table_code == 'ROU13' ~ 18/12,
                                   table_code == 'ROU5' ~ 25/12,
                                   table_code == 'ROU7' ~ 10/12,
                                   table_code == 'STR1' ~ -99,
                                   table_code == 'STR2' ~ -99,
                                   table_code == 'STR3' ~ 31/16,
                                   table_code == 'STR4' ~ 40/16,
                                   table_code == 'STR5' ~ 16/16,
                                   table_code == 'STR6' ~ 37/16,
                                   table_code == 'STR7' ~ 25/16,
                                   table_code == 'STR8' ~ 20/16,
                                   table_code == 'STR9' ~ -99,
                                   table_code == 'STR10' ~ 30/16,
                                   table_code == 'STR11' ~ 38/16,
                                   table_code == 'STR12' ~ -99,
                                   table_code == 'STR13' ~ -99,
                                   table_code == 'STR14' ~ 15/16,
                                   table_code == 'STR15' ~ 36/16,
                                   table_code == 'STR16' ~ 36/16,
                                   table_code == 'TOU1' ~ 31/16,
                                   table_code == 'TOUw' ~ 32/16,
                                   table_code == 'TOU3' ~ 30/16,
                                   table_code == 'TOU4' ~ 23/16,
                                   table_code == 'TOU5' ~ 28/16,
                                   table_code == 'TOU6' ~ 21/16,
                                   table_code == 'TOU7' ~ 26/16,
                                   table_code == 'TOU8' ~ 16/16,
                                   table_code == 'TOU9' ~ 35/16,
                                   table_code == 'TOU10' ~ 23/16,
                                   table_code == 'TOU11' ~ 22/16,
                                   table_code == 'TOU12' ~ 42/16,
                                   table_code == 'TOU13' ~ 23/16,
                                   table_code == 'TOU14' ~ 21/16,
                                   table_code == 'TOU15' ~ 33/16,
                                   table_code == 'TOU16' ~ 21/16
                                   ))




df <- df %>%
  mutate(proposal_grade = ifelse(policy_grade == -99, NA, policy_grade))





plot_grades <- ggplot (mapping = aes(df$proposal_grade)) +
  geom_histogram(bins = 30, fill="tomato4", alpha = .5) +
  theme_bw() +
  xlab ("Grades, tables policy proposals") +
  ylab ("Count") 

plot_grades




# Handling missing data and factors vars -------------------------------------------


# Need one summary var for rens_pre


df$rens_pre_dem <- as.numeric(as.character(df$rens_pre_dem))
df$rens_pre_eco <- as.numeric(as.character(df$rens_pre_eco))
df$rens_pre_etat <- as.numeric(as.character(df$rens_pre_etat))
df$rens_pre_fis <- as.numeric(as.character(df$rens_pre_fis))
df$concret_pre <- as.numeric(as.character(df$concret_pre))
df$conf_prise_pre <- as.numeric(as.character(df$conf_prise_pre))
df$partic_past <- as.numeric(as.character(df$partic_past))
df$change_avis_gain <- as.numeric(as.character(df$change_avis_gain))
df$concret_gain <- as.numeric(as.character(df$concret_gain))
df$learning_sum_pca <- as.numeric(as.character(df$learning_sum_pca))
df$op_change_sum_pca <- as.numeric(as.character(df$op_change_sum_pca))
df$delib_diverse_pre_pca <- as.numeric(as.character(df$delib_diverse_pre_pca))
df$delib_diverse_post_pca <- as.numeric(as.character(df$delib_diverse_post_pca))
df$delib_discuss_pre_pca <- as.numeric(as.character(df$delib_discuss_pre_pca))
df$delib_discuss_post_pca <- as.numeric(as.character(df$delib_discuss_post_pca))
df$consensual_pre_pca <- as.numeric(as.character(df$consensual_pre_pca))
df$consensual_post_pca <- as.numeric(as.character(df$consensual_post_pca))
df$gender   <- as.factor(df$gender)


# Replace NAs with -99 but create new variable to keep the original as well

df <- df %>%
  mutate(gender_star = ifelse(is.na(gender), -99, gender),
         gender_r = ifelse(is.na(gender), 1, 0),
         var_cogni_table_star = ifelse(is.na(var_cogni_table), -99, var_cogni_table),
         var_cogni_table_r = ifelse(is.na(var_cogni_table), 1, 0),
         mean_cogni_table_star = ifelse(is.na(mean_cogni_table), -99, mean_cogni_table),
         mean_cogni_table_r = ifelse(is.na(mean_cogni_table), 1, 0),
         cogni_indiv_code_star = ifelse(is.na(cogni_indiv_code), -99, cogni_indiv_code),
         cogni_indiv_code_r = ifelse(is.na(cogni_indiv_code), 1, 0),
         mean_trust_table_star = ifelse(is.na(mean_trust_table), -99, mean_trust_table),
         mean_trust_table_r = ifelse(is.na(mean_trust_table), 1, 0),
         var_trust_table_star = ifelse(is.na(var_trust_table), -99, var_trust_table),
         var_trust_table_r = ifelse(is.na(var_trust_table), 1, 0),
         pol_interest_star = ifelse(is.na(pol_interest), -99, pol_interest),
         pol_interest_r = ifelse(is.na(pol_interest), 1, 0),
         partic_past_star = ifelse(is.na(partic_past), -99, partic_past),
         partic_past_r = ifelse(is.na(partic_past), 1, 0),
         responsabilite_pol_star = ifelse(is.na(responsabilite_pol), -99, responsabilite_pol),
         responsabilite_pol_r = ifelse(is.na(responsabilite_pol), 1, 0),
         knowledge_pre_star = ifelse(is.na(knowledge_pre), -99, knowledge_pre),
         knowledge_pre_r = ifelse(is.na(knowledge_pre), 1, 0),
         interest_pre_star = ifelse(is.na(interest_pre), -99, interest_pre),
         interest_pre_r = ifelse(is.na(interest_pre), 1, 0),
         rens_pre_eco_star = ifelse(is.na(rens_pre_eco), -99, rens_pre_eco),
         rens_pre_eco_r = ifelse(is.na(rens_pre_eco), 1, 0),
         rens_pre_dem_star = ifelse(is.na(rens_pre_dem), -99, rens_pre_dem),
         rens_pre_dem_r = ifelse(is.na(rens_pre_dem), 1, 0),
         rens_pre_fis_star = ifelse(is.na(rens_pre_fis), -99, rens_pre_fis),
         rens_pre_fis_r = ifelse(is.na(rens_pre_fis), 1, 0),
         rens_pre_etat_star = ifelse(is.na(rens_pre_etat), -99, rens_pre_etat),
         rens_pre_etat_r = ifelse(is.na(rens_pre_etat), 1, 0),
         conf_trans_pre_star = ifelse(is.na(conf_trans_pre), -99, conf_trans_pre),
         conf_trans_pre_r = ifelse(is.na(conf_trans_pre), 1, 0),
         conf_prise_pre_star = ifelse(is.na(conf_prise_pre), -99, conf_prise_pre),
         conf_prise_pre_r = ifelse(is.na(conf_prise_pre), 1, 0),
         temps_exp_pre_star = ifelse(is.na(temps_exp_pre), -99, temps_exp_pre),
         temps_exp_pre_r = ifelse(is.na(temps_exp_pre), 1, 0),
         delib_discuss_pre_pca_star = ifelse(is.na(delib_discuss_pre_pca), -99, delib_discuss_pre_pca),
         delib_discuss_pre_pca_r = ifelse(is.na(delib_discuss_pre_pca), 1, 0),
         general_interest_pre_star = ifelse(is.na(general_interest_pre), -99, general_interest_pre),
         general_interest_pre_r = ifelse(is.na(general_interest_pre), 1, 0),
         change_avis_pre_pca_star = ifelse(is.na(change_avis_pre_pca), -99, change_avis_pre_pca),
         change_avis_pre_pca_r = ifelse(is.na(change_avis_pre_pca), 1, 0),
         consensual_pre_pca_star = ifelse(is.na(consensual_pre_pca), -99, consensual_pre_pca),
         consensual_pre_pca_r = ifelse(is.na(consensual_pre_pca), 1, 0),
         concret_pre_star = ifelse(is.na(concret_pre), -99, concret_pre),
         concret_pre_r = ifelse(is.na(concret_pre), 1, 0),
         delib_diverse_pre_pca_star = ifelse(is.na(delib_diverse_pre_pca), -99, delib_diverse_pre_pca),
         delib_diverse_pre_pca_r = ifelse(is.na(delib_diverse_pre_pca), 1, 0)
  )


# I want them as numeric of factors

df$delib_discuss_pre_pca_star <- as.numeric(as.character(df$delib_discuss_pre_pca_star))
df$consensual_pre_pca_r <- as.numeric(as.character(df$consensual_pre_pca_r))
df$consensual_pre_pca_star <- as.numeric(as.character(df$consensual_pre_pca_star))
df$consensual_pre_pca_r  <- as.numeric(as.character(df$consensual_pre_pca_r))
df$delib_diverse_pre_pca_star  <- as.numeric(as.character(df$delib_diverse_pre_pca_star))
df$delib_diverse_pre_pca_r   <- as.numeric(as.character(df$delib_diverse_pre_pca_r))


df$gender_r <- as.factor(df$gender_r)
df$var_cogni_table_r <- as.factor(df$var_cogni_table_r)
df$mean_cogni_table_r <- as.factor(df$mean_cogni_table_r)
df$cogni_indiv_code_r <- as.factor(df$cogni_indiv_code_r)
df$mean_trust_table_r <- as.factor(df$mean_trust_table_r)
df$var_trust_table_r <- as.factor(df$var_trust_table_r)
df$pol_interest_r <- as.factor(df$pol_interest_r)
df$partic_past_r <- as.factor(df$partic_past_r)
df$responsabilite_pol_r <- as.factor(df$responsabilite_pol_r)
df$knowledge_pre_r <- as.factor(df$knowledge_pre_r)
df$interest_pre_r <- as.factor(df$interest_pre_r)
df$rens_pre_eco_r <- as.factor(df$rens_pre_eco_r)
df$rens_pre_dem_r <- as.factor(df$rens_pre_dem_r)
df$rens_pre_fis_r <- as.factor(df$rens_pre_fis_r)
df$rens_pre_etat_r <- as.factor(df$rens_pre_etat_r)
df$conf_trans_pre_r <- as.factor(df$conf_trans_pre_r)
df$conf_prise_pre_r <- as.factor(df$conf_prise_pre_r)
df$temps_exp_pre_r <- as.factor(df$temps_exp_pre_r)
df$delib_discuss_pre_pca_r <- as.factor(df$delib_discuss_pre_pca_r)
df$general_interest_pre_r <- as.factor(df$general_interest_pre_r)
df$change_avis_pre_pca_r <- as.factor(df$change_avis_pre_pca_r)
df$consensual_pre_pca_r <- as.factor(df$consensual_pre_pca_r)
df$concret_pre_r <- as.factor(df$concret_pre_r)
df$delib_diverse_pre_pca_r <- as.factor(df$delib_diverse_pre_pca_r)

# Now, clean the categorial vars


df <- df %>%
  mutate(occupation_star = ifelse(is.na(occupation), -99, occupation),
         occupation_r = ifelse(is.na(occupation), 1, 0))

df$occupation_r <- as.factor(df$occupation_r)


# Save dataframe ----------------------------------------------------------


write.csv(df, file = '/df_clean.csv', na="")


# Build the instruments for IV --------------------------------------------

df <- df %>%
  mutate(rens_pre = rens_pre_eco, rens_pre_fis, rens_pre_dem, rens_pre_etat,
         int_pre = int_pre_eco, int_pre_fis, int_pre_dem, int_pre_etat,
         cx_pre = cx_post_eco, cx_pre_fis, cx_pre_dem, cx_pre_etat)

cor_test <- df %>%
  select(knowledge_pre, int_pre, cx_pre, rens_pre) %>%
  drop_na()

cor.test(cor_test$knowledge_pre,cor_test$rens_pre)
  

# Get summary var for the larger groups (1 city, 1 theme = 1 group = our instrument)
df_group <- df %>% 
  group_by(city, theme) %>% 
  summarize(mean_cogni_group = mean(cogni_indiv_code, na.rm = T),
            var_cogni_group = sqrt(var(cogni_indiv_code, na.rm = T)),
            mean_trust_group = mean(confiance_pre, na.rm = T),
            var_trust_group = sqrt(var(confiance_pre, na.rm = T)),
            mean_know_group = mean(knowledge_pre, na.rm = T),
            var_know_group = sqrt(var(knowledge_pre, na.rm = T)),
            mean_int_group = mean(int_pre, na.rm = T),
            var_int_group = sqrt(var(int_pre, na.rm = T)),
            perc_women_group = mean(gender, na.rm = T))
# Add group level instrument to df
df <- merge (df, df_group)




# Table level df ----------------------------------------------------------

# Same cleaning, but at the table (not group) level


df$cogni_indiv_code <- as.numeric(as.character(df$cogni_indiv_code))
df$gender <- as.numeric(as.character(df$gender))




df_table <- df %>%
  group_by(table_code, city, theme) %>%
  summarize(mean_cogni_table = mean(cogni_indiv_code, na.rm = T),
            var_cogni_table = sqrt(var(cogni_indiv_code, na.rm = T)),
            perc_women_table = mean(gender, na.rm = T),
            mean_trust_table = mean(confiance_pre, na.rm = T),
            var_trust_table = sqrt(var(confiance_pre, na.rm = T)),
            mean_know_table = mean(knowledge_pre, na.rm = T),
            var_know_table = sqrt(var(knowledge_pre, na.rm = T)),
            learning_table = mean(learning_sum_pca, na.rm = T),
            op_change_table = mean(op_change_sum_pca, na.rm = T),
            sat_table = mean(sat_level_gen, na.rm = T),
            diverse_table = mean(delib_diverse_post_pca, na.rm = T),
            delib_table = mean(delib_discuss_post_pca, na.rm = T),
            cons_table = mean(consensual_post_pca, na.rm = T),
            genint_table = mean(general_interest_post, na.rm = T),
            pol_interest_table = mean(pol_interest, na.rm = T),
            pol_interest_table_var = sqrt(var(pol_interest, na.rm = T)),
            partic_past_table = mean(partic_past, na.rm = T),
            partic_past_table_var = sqrt(var(partic_past, na.rm = T)),
            conf_trans_table = mean(conf_trans_pre, na.rm = T),
            conf_trans_table_var = sqrt(var(conf_trans_pre, na.rm = T)),
            conf_prise_table = mean(conf_prise_pre, na.rm = T),
            conf_prise_table_var = sqrt(var(conf_prise_pre, na.rm = T)),
            responsabilite_pol_table = mean(responsabilite_pol, na.rm = T),
            responsabilite_pol_table_var = sqrt(var(responsabilite_pol, na.rm = T)),
            conf_prise_pre_table = mean(conf_prise_pre, na.rm = T),
            conf_prise_pre_table_var = sqrt(var(conf_prise_pre, na.rm = T)),
            temps_exp_pre_table = mean(temps_exp_pre, na.rm = T),
            temps_exp_pre_table_var = sqrt(var(temps_exp_pre, na.rm = T)),
            delib_discuss_pre_pca_table = mean(delib_discuss_pre_pca, na.rm = T),
            delib_discuss_pre_pca_table_var = sqrt(var(delib_discuss_pre_pca, na.rm = T)),
            general_interest_pre_table = mean(general_interest_pre, na.rm = T),
            general_interest_pre_table_var = sqrt(var(general_interest_pre, na.rm = T)),
            change_avis_pre_pca_table = mean(change_avis_pre_pca, na.rm = T),
            change_avis_pre_pca_table_var = sqrt(var(change_avis_pre_pca, na.rm = T)),
            consensual_pre_pca_table = mean(consensual_pre_pca, na.rm = T),
            consensual_pre_pca_table_var = sqrt(var(consensual_pre_pca, na.rm = T)),
            delib_diverse_pre_pca_table = mean(delib_diverse_pre_pca, na.rm = T),
            delib_diverse_pre_pca_table_var = sqrt(var(delib_diverse_pre_pca, na.rm = T)),
            rens_pre_table = mean(rens_pre, na.rm = T),
            rens_pre_table_var = sqrt(var(rens_pre, na.rm = T)),
            mean_int_table = mean(int_pre, na.rm = T),
            var_int_table = sqrt(var(int_pre, na.rm = T)),
            cx_pre_table = mean(cx_pre, na.rm = T),
            cx_pre_table_var = sqrt(var(cx_pre, na.rm = T)),
            N = n())



df_table <- df_table %>%
  mutate (policy_grade = case_when(table_code == 'LYO8' ~ 40/14,
                                   table_code == 'LYO10' ~ 29/14,
                                   table_code == 'LYO15' ~ 26/14,
                                   table_code == 'LYO12' ~ 31/14,
                                   table_code == 'LYO3' ~ 38.5/14,
                                   table_code == 'LYO6' ~ 33/14,
                                   table_code == 'LYO11' ~ 33/14,
                                   table_code == 'LYO4' ~ 20/14,
                                   table_code == 'LYO7' ~ 41/14,
                                   table_code == 'LYO9' ~ 33/14,
                                   table_code == 'LYO13' ~ 19.5/14,
                                   table_code == 'LYO14' ~ 33/14,
                                   table_code == 'LYO16' ~ 18/14,
                                   table_code == 'LYO2' ~ 19/14,
                                   table_code == 'AIX1' ~ 24/12,
                                   table_code == 'AIX2' ~ 31/12,
                                   table_code == 'AIX3' ~ 25/12,
                                   table_code == 'AIX5' ~ 32/12,
                                   table_code == 'AIX6' ~ 15/12,
                                   table_code == 'AIX7' ~ 13/12,
                                   table_code == 'AIX9' ~ 21/12,
                                   table_code == 'AIX10' ~ 36/12,
                                   table_code == 'AIX11' ~ 29/12,
                                   table_code == 'AIX13' ~ 22/12,
                                   table_code == 'AIX15' ~ 27/12,
                                   table_code == 'AIX16' ~ 29/12,
                                   table_code == 'AJA16' ~ 22/11,
                                   table_code == 'AJA10' ~ 28/11,
                                   table_code == 'AJA14' ~ 18/11,
                                   table_code == 'AJA3' ~ 29/11,
                                   table_code == 'AJA1' ~ 21/11,
                                   table_code == 'AJA11' ~ 24/11,
                                   table_code == 'AJA12' ~ 32/11,
                                   table_code == 'AJA2' ~ 23/11,
                                   table_code == 'AJA5' ~ 27/11,
                                   table_code == 'AJA6' ~ -99,
                                   table_code == 'AJA7' ~ -99,
                                   table_code == 'DIJ 3' ~ 21/16,
                                   table_code == 'DIJ 5' ~ 31/16,
                                   table_code == 'DIJ 7' ~ 16/16,
                                   table_code == 'DIJ 2' ~ 20/16,
                                   table_code == 'DIJ 4' ~ 19/16,
                                   table_code == 'DIJ 6' ~ 23/16,
                                   table_code == 'DIJ 14' ~ 30/16,
                                   table_code == 'DIJ 15' ~ -99,
                                   table_code == 'DIJ 1' ~ 20/16,
                                   table_code == 'DIJ 9' ~ 30/16,
                                   table_code == 'DIJ 13' ~ 19/16,
                                   table_code == 'DIJ 10' ~ 23/16,
                                   table_code == 'DIJ 11' ~ 24/16,
                                   table_code == 'DIJ 12' ~ 19/16,
                                   table_code == 'LIL1' ~ 22/12,
                                   table_code == 'LIL2' ~ 18/12,
                                   table_code == 'LIL3' ~ 22/12,
                                   table_code == 'LIL5' ~ 28/12,
                                   table_code == 'LIL6' ~ 21/12,
                                   table_code == 'LIL7' ~ 20/12,
                                   table_code == 'LIL9' ~ 25/12,
                                   table_code == 'LIL10' ~ 18/12,
                                   table_code == 'LIL11' ~ 30/12,
                                   table_code == 'LIL13' ~ 19/12,
                                   table_code == 'LIL14' ~ 31/12,
                                   table_code == 'LIL15' ~ 24/12,
                                   table_code == 'MAR 1' ~ 21/12,
                                   table_code == 'MAR 2' ~ 21/12,
                                   table_code == 'MAR 3' ~ 16/12,
                                   table_code == 'MAR 5' ~ 29/12,
                                   table_code == 'MAR 6' ~ -99,
                                   table_code == 'MAR7' ~ 18/12,
                                   table_code == 'MAR9' ~ 26/12,
                                   table_code == 'MAR10' ~ 18/12,
                                   table_code == 'MAR11' ~ 20/12,
                                   table_code == 'MAR13' ~ 22/12,
                                   table_code == 'MAR14' ~ 27/12,
                                   table_code == 'MAR15' ~ 13/12,
                                   table_code == 'NAN1' ~ -99,
                                   table_code == 'NAN2' ~ -99,
                                   table_code == 'NAN3' ~ 29/17,
                                   table_code == 'NAN4' ~ 28/17,
                                   table_code == 'NAN5' ~ 28/17,
                                   table_code == 'NAN6' ~ 30/17,
                                   table_code == 'NAN7' ~ 37/17,
                                   table_code == 'NAN8' ~ -99,
                                   table_code == 'NAN9' ~ -99,
                                   table_code == 'NAN10' ~ -99,
                                   table_code == 'NAN11' ~ -99,
                                   table_code == 'NAN12' ~ -99,
                                   table_code == 'NAN13' ~ -99,
                                   table_code == 'NAN14' ~ -99,
                                   table_code == 'NAN15' ~ -99,
                                   table_code == 'NAN16' ~ -99,
                                   table_code == 'ORL9' ~ 17/12,
                                   table_code == 'ORL11' ~ 25/12,
                                   table_code == 'ORL7' ~ -99,
                                   table_code == 'ORL1' ~ -99,
                                   table_code == 'ORL10' ~ -99,
                                   table_code == 'ORL13' ~ 17/12,
                                   table_code == 'ORL15' ~ 17.5/12,
                                   table_code == 'ORL3' ~ 32/12,
                                   table_code == 'ORL6' ~ -99,
                                   table_code == 'ORL14' ~ -99,
                                   table_code == 'ORL5' ~ -99,
                                   table_code == 'ORL2' ~ -99,
                                   table_code == 'PAR1' ~ 40/18,
                                   table_code == 'par2' ~ 20/18,
                                   table_code == 'PAR3' ~ 25/18,
                                   table_code == 'PAR4' ~ 40/18,
                                   table_code == 'PAR5' ~ 29/18,
                                   table_code == 'par6' ~ 19/18,
                                   table_code == 'PAR7' ~ 30/18,
                                   table_code == 'PAR8' ~ 25/18,
                                   table_code == 'PAR9' ~ 36/18,
                                   table_code == 'PAR10' ~ 45/18,
                                   table_code == 'PAR11' ~ 39/18,
                                   table_code == 'PAR12' ~ 31/18,
                                   table_code == 'PAR13' ~ 35/18,
                                   table_code == 'PAR14' ~ 33/18,
                                   table_code == 'PAR15' ~ 38/18,
                                   table_code == 'PAR16' ~ 11/18,
                                   table_code == 'PAR17' ~ 26/18,
                                   table_code == 'PAR18' ~ 21/18,
                                   table_code == 'REN1' ~ 27/14,
                                   table_code == 'REN2' ~ 20/14,
                                   table_code == 'REN3' ~ 29/14,
                                   table_code == 'REN4' ~ 33/14,
                                   table_code == 'REN5' ~ 31/14,
                                   table_code == 'REN6' ~ 30/14,
                                   table_code == 'REN7' ~ 12/14,
                                   table_code == 'REN8' ~ 28/14,
                                   table_code == 'REN9' ~ 41/14,
                                   table_code == 'REN10' ~ 36/14,
                                   table_code == 'REN11' ~ 41/14,
                                   table_code == 'REN13' ~ 24/14,
                                   table_code == 'REN14' ~ 25/14,
                                   table_code == 'REN15' ~ 32/14,
                                   table_code == 'ROU14' ~ 24/12,
                                   table_code == 'ROU10' ~ 26.5/12,
                                   table_code == 'ROU11' ~ 18/12,
                                   table_code == 'ROU15' ~ 26/12,
                                   table_code == 'ROU2' ~ 21/12,
                                   table_code == 'ROU3' ~ 17/12,
                                   table_code == 'ROU6' ~ 23/12,
                                   table_code == 'ROU9' ~ 25/12,
                                   table_code == 'ROU1' ~ 30/12,
                                   table_code == 'ROU13' ~ 18/12,
                                   table_code == 'ROU5' ~ 25/12,
                                   table_code == 'ROU7' ~ 10/12,
                                   table_code == 'STR1' ~ -99,
                                   table_code == 'STR2' ~ -99,
                                   table_code == 'STR3' ~ 31/16,
                                   table_code == 'STR4' ~ 40/16,
                                   table_code == 'STR5' ~ 16/16,
                                   table_code == 'STR6' ~ 37/16,
                                   table_code == 'STR7' ~ 25/16,
                                   table_code == 'STR8' ~ 20/16,
                                   table_code == 'STR9' ~ -99,
                                   table_code == 'STR10' ~ 30/16,
                                   table_code == 'STR11' ~ 38/16,
                                   table_code == 'STR12' ~ -99,
                                   table_code == 'STR13' ~ -99,
                                   table_code == 'STR14' ~ 15/16,
                                   table_code == 'STR15' ~ 36/16,
                                   table_code == 'STR16' ~ 36/16,
                                   table_code == 'TOU1' ~ 31/16,
                                   table_code == 'TOUw' ~ 32/16,
                                   table_code == 'TOU3' ~ 30/16,
                                   table_code == 'TOU4' ~ 23/16,
                                   table_code == 'TOU5' ~ 28/16,
                                   table_code == 'TOU6' ~ 21/16,
                                   table_code == 'TOU7' ~ 26/16,
                                   table_code == 'TOU8' ~ 16/16,
                                   table_code == 'TOU9' ~ 35/16,
                                   table_code == 'TOU10' ~ 23/16,
                                   table_code == 'TOU11' ~ 22/16,
                                   table_code == 'TOU12' ~ 42/16,
                                   table_code == 'TOU13' ~ 23/16,
                                   table_code == 'TOU14' ~ 21/16,
                                   table_code == 'TOU15' ~ 33/16,
                                   table_code == 'TOU16' ~ 21/16
  ))


df_table <- df_table %>%
  mutate(proposal_grade = ifelse(policy_grade == -99, NA, policy_grade))

# Need city and theme



df_table <- merge (df_table, df_group, by = c('city', 'theme'))





# Save table level df

write.csv(df_table, file = 'df_clean_table.csv', na="")




###########
# Plot distribution of DVs at table level
########

colnames(df_fake_table)

plot_learn_t <- ggplot (mapping = aes(df_table$learning_table)) +
  geom_histogram(bins = 70, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Learning") +
  ylab ("Number of tables") 

plot_learn_t



plot_op_t <- ggplot (mapping = aes(df_table$op_change_table)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Opinion change") +
  ylab ("Number of tables") 

plot_op_t


plot_sat_t <- ggplot (mapping = aes(df_table$sat_table)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Satisfaction") +
  ylab ("Number of tables") 

plot_sat_t


plot_diverse_t <- ggplot (mapping = aes(df_table$diverse_table)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Apprec. for diversity") +
  ylab ("Number of tables") 

plot_diverse_t


plot_delib_t <- ggplot (mapping = aes(df_table$delib_table)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Deliberative quality") +
  ylab ("Number of tables") 

plot_delib_t




plot_cons_t <- ggplot (mapping = aes(df_table$cons_table)) +
  geom_histogram(bins = 60, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("Consensual orientation") +
  ylab ("Number of tables") 

plot_cons_t



plot_genint_t <- ggplot (mapping = aes(df_table$genint_table)) +
  geom_histogram(bins = 30, fill="darkseagreen4", alpha = .5) +
  theme_bw() +
  xlab ("General interest") +
  ylab ("Number of tables") 

plot_genint_t




plots_baselines_dv_t <- (plot_learn_t | plot_op_t | plot_sat_t) /
  (plot_diverse_t | plot_delib_t | plot_cons_t) /
  (plot_genint_t) 

plots_baselines_dv_t





plot_gender_t <- ggplot (mapping = aes(df_table$perc_women_table)) +
  geom_histogram(bins = 30, fill="mediumpurple4", alpha = .5) +
  theme_bw() +
  xlab ("Percentage of women at table") +
  ylab ("Number of tables") 

plot_gender_t




