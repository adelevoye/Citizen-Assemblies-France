rm(list = ls())


library(ggplot2)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(xtable)
library(stargazer)
library(commarobust)
library(randomizr)
library(ggpubr)
library(estimatr)
library(texreg)
library(sandwich)
library(patchwork)



load("/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/df_clean.Rda")


# Gender -----------------------------------------------------

se <- function(x) {sd(x, na.rm = T)/sqrt(length(x))}
df_fake$con

df_fake_gender_means <- df_fake %>%
  group_by (gender) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T)) %>%
  drop_na()


df_fake_gender_ses <- df_fake %>%
  group_by (gender) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))  %>%
  drop_na()


df_fake_gender_means <- df_fake_gender_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')




df_fake_gender_ses <- df_fake_gender_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')


df_fake_gender <- df_fake_gender_means %>% 
  right_join(df_fake_gender_ses, by=c("gender","DV"))

df_fake_gender <- df_fake_gender %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE) %>%
  drop_na()




###########
# Learning
#########

df_fake_gender_learn <- df_fake_gender %>%
  filter(DV == 'learning')

distrib_gender_learn <- ggplot(data=subset(df_fake, !is.na(gender)), aes(learning_sum_pca, 
                                                 color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_learn, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Learning Gains") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())



distrib_gender_learn



ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_learning.pdf',
       distrib_gender_learn)


###########
# Op change
#########


df_fake_gender_opchange <- df_fake_gender %>%
  filter(DV == 'op_change')

distrib_gender_opchange <- ggplot(data=subset(df_fake, !is.na(gender)), aes(op_change_sum_pca, 
                                                                         color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_opchange, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Opinion Change") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())


distrib_gender_opchange


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_opchange.pdf',
       distrib_gender_opchange)



###########
# Sat
#########



df_fake_gender_sat <- df_fake_gender %>%
  filter(DV == 'satisfaction')

distrib_gender_sat <- ggplot(data=subset(df_fake, !is.na(gender)), aes(sat_level_gen, 
                                                                            color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_sat, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Satisfaction level") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())


distrib_gender_sat


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_satisfaction.pdf',
       distrib_gender_sat)




###########
# Diversity 
#########


df_fake_gender_diverse <- df_fake_gender %>%
  filter(DV == 'delib_diverse')

distrib_gender_diverse <- ggplot(data=subset(df_fake, !is.na(gender)), aes(delib_diverse_post_pca, 
                                                                       color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_diverse, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Appreciation for diversity") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())


distrib_gender_diverse


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_diversity.pdf',
       distrib_gender_diverse)




###########
# Dliberation
#########


df_fake_gender_delib <- df_fake_gender %>%
  filter(DV == 'delib_discuss')

distrib_gender_delib <- ggplot(data=subset(df_fake, !is.na(gender)), aes(delib_discuss_post_pca, 
                                                                           color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_delib, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Deliberative nature of discussion") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())


distrib_gender_delib


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_delib.pdf',
       distrib_gender_delib)




###########
# Consensual
#########


df_fake_gender_cons <- df_fake_gender %>%
  filter(DV == 'consensual')

distrib_gender_cons <- ggplot(data=subset(df_fake, !is.na(gender)), aes(consensual_post_pca, 
                                                                         color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_cons, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Consensual orientation of discussion") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "none",
        legend.text=element_blank(), 
        plot.caption = element_blank(),
        legend.title = element_blank())


distrib_gender_cons


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_cons.pdf',
       distrib_gender_cons)





###########
# Genint
#########


df_fake_gender_genint <- df_fake_gender %>%
  filter(DV == 'genint')

distrib_gender_genint <- ggplot(data=subset(df_fake, !is.na(gender)), aes(general_interest_post, 
                                                                        color = as.factor(gender))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_gender_genint, 
             aes(xintercept = group_means,  colour=as.factor(gender)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_gender_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(gender)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_gender_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(gender)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("General interest orientation of discussion") +
  scale_color_manual (values = c("gray1", "gray80"), 
                      labels = c('Men', 'Women')) +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_gender_genint


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/gender_genint.pdf',
       distrib_gender_genint)




###########
# Sum plot
#########


plots_gender_distrib <- (distrib_gender_learn | distrib_gender_opchange | distrib_gender_sat) /
  (distrib_gender_diverse | distrib_gender_delib | distrib_gender_cons) /
  (distrib_gender_genint) 

plots_gender_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_gender_distrib.pdf',
       plots_gender_distrib)



# Age --------------------------------------------------------

table(df_fake$city)

df_fake <- df_fake %>%
  mutate (age = case_when(occupation == 'etudiant' ~ 'youth',
                          occupation == 'retraite' ~ 'retired',
                          city == 'AIX' ~ 'youth'))



df_fake_age_means <- df_fake %>%
  group_by (age) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T))



df_fake_age_means <- df_fake_age_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')

df_fake_age_means <- df_fake_age_means[complete.cases(df_fake_age_means), ]



df_fake_age_ses <- df_fake %>%
  group_by (age) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))

df_fake_age_ses <- df_fake_age_ses[complete.cases(df_fake_age_ses), ]


df_fake_age_ses <- df_fake_age_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')


df_fake_age <- df_fake_age_means %>% 
  right_join(df_fake_age_ses, by=c("age","DV"))

df_fake_age <- df_fake_age %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE)


###########
# Learning
#########



df_fake_age_learn <- df_fake_age %>%
  filter(DV == 'learning')

distrib_age_learn <- ggplot(data=subset(df_fake, !is.na(age)), aes(learning_sum_pca, 
                                                                         color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_learn, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Learning Gains") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_age_learn


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_learning.pdf',
       distrib_age_learn)






###########
# Opchane
#########



df_fake_age_opchange <- df_fake_age %>%
  filter(DV == 'op_change')

distrib_age_opchange <- ggplot(data=subset(df_fake, !is.na(age)), aes(op_change_sum_pca, 
                                                                      color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_opchange, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Opinion Change") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))



distrib_age_opchange


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_opchange.pdf',
       distrib_age_opchange)




###########
# Sat
#########



df_fake_age_sat <- df_fake_age %>%
  filter(DV == 'satisfaction')

distrib_age_sat <- ggplot(data=subset(df_fake, !is.na(age)), aes(sat_level_gen, 
                                                                      color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_sat, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Satisfaction") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))



distrib_age_sat


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_sat.pdf',
       distrib_age_sat)



###########
# Diversity
#########



df_fake_age_diverse <- df_fake_age %>%
  filter(DV == 'delib_diverse')

distrib_age_diverse <- ggplot(data=subset(df_fake, !is.na(age)), aes(delib_diverse_post_pca, 
                                                                 color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_diverse, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Appreciation for diversity") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))



distrib_age_diverse


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_diversity.pdf',
       distrib_age_diverse)




###########
# Delib
#########



df_fake_age_delib <- df_fake_age %>%
  filter(DV == 'delib_discuss')

distrib_age_delib <- ggplot(data=subset(df_fake, !is.na(age)), aes(delib_discuss_post_pca, 
                                                                     color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_delib, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Deliberative quality of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))



distrib_age_delib


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_delib.pdf',
       distrib_age_delib)





###########
# Consensual
#########



df_fake_age_cons <- df_fake_age %>%
  filter(DV == 'consensual')

distrib_age_cons <- ggplot(data=subset(df_fake, !is.na(age)), aes(consensual_post_pca, 
                                                                   color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_cons, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("Consensual orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_age_cons


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_cons.pdf',
       distrib_age_cons)





###########
# Genint
#########



df_fake_age_genint <- df_fake_age %>%
  filter(DV == 'genint')

distrib_age_genint <- ggplot(data=subset(df_fake, !is.na(age)), aes(general_interest_post, 
                                                                  color = as.factor(age))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_age_genint, 
             aes(xintercept = group_means,  colour=as.factor(age)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_age_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(age)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_age_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(age)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c("gray1", "gray80"),
                      labels = c('Retired', 'Youth')) +
  xlab ("General interest orientation of discussion") +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_age_genint


ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/age_genint.pdf',
       distrib_age_genint)




###########
# Sum plot
#########


plots_age_distrib <- (distrib_age_learn | distrib_age_opchange | distrib_age_sat) /
  (distrib_age_diverse | distrib_age_delib | distrib_age_cons) /
  (distrib_age_genint) 

plots_age_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_age_distrib.pdf',
       plots_age_distrib)





# Occupation --------------------------------------------------------------


df_fake_cogni_means <- df_fake %>%
  group_by (cogni_indiv_code) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T))


df_fake_cogni_means <- df_fake_cogni_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')

df_fake_cogni_means <- df_fake_cogni_means[complete.cases(df_fake_cogni_means), ]



df_fake_cogni_ses <- df_fake %>%
  group_by (cogni_indiv_code) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))

df_fake_cogni_ses <- df_fake_cogni_ses[complete.cases(df_fake_cogni_ses), ]


df_fake_cogni_ses <- df_fake_cogni_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')




df_fake_cogni <- df_fake_cogni_means %>% 
  right_join(df_fake_cogni_ses, by=c("cogni_indiv_code","DV"))

df_fake_cogni <- df_fake_cogni %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE)


# Only zoom in on low and high
df_fake_cogni <- df_fake_cogni %>%
  filter(cogni_indiv_code != 0)

# Also need df_fake subset
df_fake_ex_cogni <- df_fake %>%
  filter(cogni_indiv_code != 0)


###########
# Learning
#########



df_fake_cogni_learn <- df_fake_cogni %>%
  filter(DV == 'learning')

distrib_cogni_learn <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                              aes(learning_sum_pca, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_learn, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Learning Gains") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_learn




###########
# Opchange
#########



df_fake_cogni_opchange <- df_fake_cogni %>%
  filter(DV == 'op_change')

distrib_cogni_opchange <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                              aes(op_change_sum_pca, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_opchange, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Opinion Change") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_opchange




###########
# Satisfaction
#########



df_fake_cogni_sat <- df_fake_cogni %>%
  filter(DV == 'satisfaction')

distrib_cogni_sat <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                                 aes(sat_level_gen, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_sat, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Satisfaction") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_sat




###########
# Diversity 
#########



df_fake_cogni_diverse <- df_fake_cogni %>%
  filter(DV == 'delib_diverse')

distrib_cogni_diverse <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                            aes(delib_diverse_post_pca, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_diverse, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Appreciation for diversity") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_diverse




###########
# Deliberation
#########



df_fake_cogni_delib <- df_fake_cogni %>%
  filter(DV == 'delib_discuss')

distrib_cogni_delib <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                                aes(delib_discuss_post_pca, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_delib, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Deliberative quality of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_delib






###########
# Consensual
#########



df_fake_cogni_cons <- df_fake_cogni %>%
  filter(DV == 'consensual')

distrib_cogni_cons <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                              aes(consensual_post_pca, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_cons, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("Consensual orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_cons





###########
# Genint
#########



df_fake_cogni_genint <- df_fake_cogni %>%
  filter(DV == 'genint')

distrib_cogni_genint <- ggplot(data=subset(df_fake_ex_cogni, !is.na(cogni_indiv_code)), 
                             aes(general_interest_post, color = as.factor(cogni_indiv_code))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_cogni_genint, 
             aes(xintercept = group_means,  colour=as.factor(cogni_indiv_code)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_cogni_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_cogni_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(cogni_indiv_code)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Manual', 'Intellectual')) +
  xlab ("General interest orientation of discussion") +
  ylab ("Density") +
  labs(caption="Lines represent group means and 95% confidence intervals") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_cogni_genint





###########
# Sum plot
#########


plots_cogni_distrib <- (distrib_cogni_learn | distrib_cogni_opchange | distrib_cogni_sat) /
  (distrib_cogni_diverse | distrib_cogni_delib | distrib_cogni_cons) /
  (distrib_cogni_genint) 

plots_cogni_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_cogni_distrib.pdf',
       plots_cogni_distrib)




# Previous public life experience -------------------------------------------

table(df_fake$partic_past)



df_fake <- df_fake %>%
  mutate (public_exp = case_when(partic_past == 1 ~ 'Zero experience',
                                 partic_past == 2 ~ 'Some or frequent experience',
                                 partic_past == 3 ~ 'Some or frequent experience',
                                 partic_past == 4 ~ 'Some or frequent experience',
                                 partic_past == 5 ~ 'NA',
                                 partic_past == 6 ~ 'NA')) 
                                 



df_fake_public_means <- df_fake %>%
  group_by (public_exp) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T)) %>%
  drop_na() %>%
  filter(public_exp != 'NA')



df_fake_public_means <- df_fake_public_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')

df_fake_public_means <- df_fake_public_means[complete.cases(df_fake_public_means), ]



df_fake_public_ses <- df_fake %>%
  group_by (public_exp) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))

df_fake_public_ses <- df_fake_public_ses[complete.cases(df_fake_public_ses), ]


df_fake_public_ses <- df_fake_public_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')





df_fake_public <- df_fake_public_means %>% 
  right_join(df_fake_public_ses, by=c("public_exp","DV"))

df_fake_public <- df_fake_public %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE) %>%
  drop_na()



# Also need df_fake subset
df_fake_ex_expe <- df_fake %>%
  filter(partic_past == 1|partic_past == 2 | partic_past == 3 | partic_past == 4)


###########
# Learning
#########



df_fake_exp_learn <- df_fake_public %>%
  filter(DV == 'learning')

distrib_exp_learn <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                              aes(learning_sum_pca, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_learn, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("Learning Gains") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_learn



###########
# Opchange
#########



df_fake_exp_opchange <- df_fake_public %>%
  filter(DV == 'op_change')

distrib_exp_opchange <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                            aes(op_change_sum_pca, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_opchange, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("Opinion Change") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_opchange




###########
# Satisfaction
#########



df_fake_exp_sat <- df_fake_public %>%
  filter(DV == 'satisfaction')

distrib_exp_sat <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                               aes(sat_level_gen, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_sat, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("Satisfaction") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_sat




###########
# Diversity
#########



df_fake_exp_diverse <- df_fake_public %>%
  filter(DV == 'delib_diverse')

distrib_exp_diverse <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                          aes(delib_diverse_post_pca, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_diverse, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  theme_bw() +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  xlab ("Appreciation for diversity") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_diverse



###########
# Deliberation
#########



df_fake_exp_delib <- df_fake_public %>%
  filter(DV == 'delib_discuss')

distrib_exp_delib <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                              aes(delib_discuss_post_pca, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_delib, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("Deliberative nature of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_delib

###########
# Consensual
#########




df_fake_exp_cons <- df_fake_public %>%
  filter(DV == 'consensual')

distrib_exp_cons <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                            aes(consensual_post_pca, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_cons, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("Consensual orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_cons



###########
# Genint
#########




df_fake_exp_genint <- df_fake_public %>%
  filter(DV == 'genint')

distrib_exp_genint <- ggplot(data=subset(df_fake_ex_expe, !is.na(public_exp)), 
                           aes(general_interest_post, color = as.factor(public_exp))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_exp_genint, 
             aes(xintercept = group_means,  colour=as.factor(public_exp)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_exp_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_exp_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(public_exp)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('Some or frequent experience', 'No previous experience')) +
  theme_bw() +
  xlab ("General interest orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_exp_genint




###########
# Sum plot
#########


plots_exp_distrib <- (distrib_exp_learn | distrib_exp_opchange | distrib_exp_sat) /
  (distrib_exp_diverse | distrib_exp_delib | distrib_exp_cons) /
  (distrib_exp_genint) 

plots_exp_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_exp_distrib.pdf',
       plots_exp_distrib)




# Baseline interest in politics -------------------------------------------

hist(df_fake$pol_interest)

df_fake <- df_fake %>%
  mutate (politics = ifelse(pol_interest %in% c(0:6.5), 'Low interest','High interest'))

table(df_fake$politics)


df_fake_politics_means <- df_fake %>%
  group_by (politics) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T))




df_fake_politics_means <- df_fake_politics_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')

df_fake_politics_means <- df_fake_politics_means[complete.cases(df_fake_politics_means), ]



df_fake_politics_ses <- df_fake %>%
  group_by (politics) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))

df_fake_politics_ses <- df_fake_politics_ses[complete.cases(df_fake_politics_ses), ]


df_fake_politics_ses <- df_fake_politics_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')





df_fake_politics <- df_fake_politics_means %>% 
  right_join(df_fake_politics_ses, by=c("politics","DV"))

df_fake_politics <- df_fake_politics %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE)



###########
# Learning
#########



df_fake_int_learn <- df_fake_politics %>%
  filter(DV == 'learning')

distrib_int_learn <- ggplot(data=subset(df_fake), 
                            aes(learning_sum_pca, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_learn, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Learning Gains") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_learn



###########
# Opchange
#########



df_fake_int_opchange <- df_fake_politics %>%
  filter(DV == 'op_change')

distrib_int_opchange <- ggplot(data=subset(df_fake, !is.na(politics)), 
                               aes(op_change_sum_pca, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_opchange, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Opinion Change") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_opchange




###########
# Satisfaction
#########



df_fake_int_sat <- df_fake_politics %>%
  filter(DV == 'satisfaction')

distrib_int_sat <- ggplot(data=subset(df_fake, !is.na(politics)), 
                          aes(sat_level_gen, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_sat, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Satisfaction") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_sat




###########
# Diversity
#########



df_fake_int_diverse <- df_fake_politics %>%
  filter(DV == 'delib_diverse')

distrib_int_diverse <- ggplot(data=subset(df_fake, !is.na(politics)), 
                              aes(delib_diverse_post_pca, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_diverse, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Appreciation for diversity") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_diverse



###########
# Deliberation
#########



df_fake_int_delib <- df_fake_politics %>%
  filter(DV == 'delib_discuss')

distrib_int_delib <- ggplot(data=subset(df_fake, !is.na(politics)), 
                            aes(delib_discuss_post_pca, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_delib, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Deliberative nature of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_delib

###########
# Consensual
#########




df_fake_int_cons <- df_fake_politics %>%
  filter(DV == 'consensual')

distrib_int_cons <- ggplot(data=subset(df_fake, !is.na(politics)), 
                           aes(consensual_post_pca, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_cons, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("Consensual orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_cons



###########
# Genint
#########




df_fake_int_genint <- df_fake_politics %>%
  filter(DV == 'genint')

distrib_int_genint <- ggplot(data=subset(df_fake, !is.na(politics)), 
                             aes(general_interest_post, color = as.factor(politics))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_int_genint, 
             aes(xintercept = group_means,  colour=as.factor(politics)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_int_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(politics)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_int_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(politics)),
             linetype="dashed", size=.2) +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High interest', 'Low interest')) +
  theme_bw() +
  xlab ("General interest orientation of discussion") +
  ylab ("Density") +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_int_genint




###########
# Sum plot
#########


plots_int_distrib <- (distrib_int_learn | distrib_int_opchange | distrib_int_sat) /
  (distrib_int_diverse | distrib_int_delib | distrib_int_cons) /
  (distrib_int_genint) 

plots_int_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_int_distrib.pdf',
       plots_int_distrib)








# Baseline knowledge -------------------------------------------

hist(df_fake$knowledge_pre)

df_fake <- df_fake %>%
  mutate (knowledge = ifelse(knowledge_pre %in% c(0:5), 'Low knowledge','High knowledge'))



df_fake_knowledge_means <- df_fake %>%
  group_by (knowledge) %>%
  summarize (learning = mean(learning_sum_pca, na.rm = T),
             op_change = mean(op_change_sum_pca, na.rm = T),
             satisfaction = mean(sat_level_gen, na.rm = T),
             delib_diverse = mean (delib_diverse_post_pca, na.rm = T),
             delib_discuss = mean (delib_discuss_post_pca, na.rm = T),
             consensual = mean (consensual_post_pca, na.rm = T),
             genint = mean (general_interest_post, na.rm = T))




df_fake_knowledge_means <- df_fake_knowledge_means %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'group_means')

df_fake_knowledge_means <- df_fake_knowledge_means[complete.cases(df_fake_knowledge_means), ]



df_fake_knowledge_ses <- df_fake %>%
  group_by (knowledge) %>%
  summarize (learning = se(learning_sum_pca),
             op_change = se(op_change_sum_pca),
             satisfaction = se(sat_level_gen),
             delib_diverse = se (delib_diverse_post_pca),
             delib_discuss = se (delib_discuss_post_pca),
             consensual = se (consensual_post_pca),
             genint = se (general_interest_post))

df_fake_knowledge_ses <- df_fake_knowledge_ses[complete.cases(df_fake_knowledge_ses), ]


df_fake_knowledge_ses <- df_fake_knowledge_ses %>%
  gather('learning', 'op_change', 'satisfaction', 'delib_diverse', 
         'delib_discuss', 'consensual', 'genint', key = 'DV', value = 'SE')





df_fake_knowledge <- df_fake_knowledge_means %>% 
  right_join(df_fake_knowledge_ses, by=c("knowledge","DV"))

df_fake_knowledge <- df_fake_knowledge %>%
  mutate (ci_lower = group_means - 1.96 * SE,
          ci_upper = group_means + 1.96 * SE)



###########
# Learning
#########



df_fake_know_learn <- df_fake_knowledge %>%
  filter(DV == 'learning')

distrib_know_learn <- ggplot(data=df_fake, 
                            aes(learning_sum_pca, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_learn, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_learn, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_learn, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Learning Gains") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_learn



###########
# Opchange
#########



df_fake_know_opchange <- df_fake_knowledge %>%
  filter(DV == 'op_change')

distrib_know_opchange <- ggplot(data=df_fake, 
                               aes(op_change_sum_pca, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_opchange, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_opchange, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_opchange, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Opinion Change") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_opchange




###########
# Satisfaction
#########



df_fake_know_sat <- df_fake_knowledge %>%
  filter(DV == 'satisfaction')

distrib_know_sat <- ggplot(data=df_fake, 
                          aes(sat_level_gen, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_sat, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_sat, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_sat, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Satisfaction") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_sat




###########
# Diversity
#########



df_fake_know_diverse <- df_fake_knowledge %>%
  filter(DV == 'delib_diverse')

distrib_know_diverse <- ggplot(data=df_fake, 
                              aes(delib_diverse_post_pca, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_diverse, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_diverse, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_diverse, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Appreciation for diversity") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_diverse



###########
# Deliberation
#########



df_fake_know_delib <- df_fake_knowledge %>%
  filter(DV == 'delib_discuss')

distrib_know_delib <- ggplot(data=df_fake, 
                            aes(delib_discuss_post_pca, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_delib, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_delib, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_delib, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Deliberative nature of discussion") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_delib

###########
# Consensual
#########




df_fake_know_cons <- df_fake_knowledge %>%
  filter(DV == 'consensual')

distrib_know_cons <- ggplot(data=df_fake, 
                           aes(consensual_post_pca, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_cons, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_cons, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_cons, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("Consensual orientation of discussion") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "none",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_cons



###########
# Genint
#########




df_fake_know_genint <- df_fake_knowledge %>%
  filter(DV == 'genint')

distrib_know_genint <- ggplot(data=df_fake, 
                             aes(general_interest_post, color = as.factor(knowledge))) +
  geom_density(alpha = .2) +
  geom_vline(data=df_fake_know_genint, 
             aes(xintercept = group_means,  colour=as.factor(knowledge)),
             linetype="solid", size=.5) + 
  geom_vline(data=df_fake_know_genint, 
             aes(xintercept = ci_lower,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) + 
  geom_vline(data=df_fake_know_genint, 
             aes(xintercept = ci_upper,  colour=as.factor(knowledge)),
             linetype="dashed", size=.2) +
  theme_bw() +
  xlab ("General interest orientation of discussion") +
  ylab ("Density") +
  scale_color_manual (values = c('gray1', 'gray80'),
                      labels = c('High knowledge', 'Low knowledge')) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=10,hjust = 0), 
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, face = "bold"))


distrib_know_genint




###########
# Sum plot
#########


plots_know_distrib <- (distrib_know_learn | distrib_know_opchange | distrib_know_sat) /
  (distrib_know_diverse | distrib_know_delib | distrib_know_cons) /
  (distrib_know_genint) 

plots_know_distrib

ggsave('/Volumes/GoogleDrive/My Drive/Grand Débat projet/French Regional Citizens Assemblies 2019/Article avec Angele Delevoye/Data analysis/Real analysis/Plots_distrib_indiv/plots_know_distrib.pdf',
       plots_know_distrib)










