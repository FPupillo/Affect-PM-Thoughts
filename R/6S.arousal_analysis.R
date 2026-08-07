#------------------------------------------------------------------------------#
# analysis of arousal
#------------------------------------------------------------------------------#
variables_to_reshape_ar_diff<-c("arousal_bef_after_ind","arousal_bef_after_PM" )

# with the valence - difference score
long_df_arousal_diff <- melt(df_wide, id.vars = variables_to_keep,
                             measure.vars = variables_to_reshape_ar_diff)

names(long_df_arousal_diff)[which(names(long_df_arousal_diff)=="variable")]<-
  "arousal_diff_measure"

names(long_df_arousal_diff)[which(names(long_df_arousal_diff)=="value")]<-
  "arousal_diff"
#------------------------------------------------------------------------------#

# only taking the difference between the baseline and the valence after the induction
arousal_aft_IND<-
  long_df_arousal_diff[long_df_arousal_diff$arousal_diff_measure=="arousal_bef_after_ind",]


#------------------------------------------------------------------------------#
# excluse participants with low MOCA
MOCAexcl<-long_df_valence%>%
  group_by(participant) %>%
  slice(1) %>%
  filter(MOCA<26)
#------------------------------------------------------------------------------#

# how many participants after exclusion?
kable(long_df_valence %>%
        group_by(participant) %>%
        slice(1) %>%
        filter(MOCA>=26 | is.na(MOCA)) %>%
        group_by( agegroup)%>%
        tally())

#------------------------------------------------------------------------------#
# plot the raw data
#------------------------------------------------------------------------------#
print(
  # plot to check if there are condition differences
  ggplot(long_df_arousal,aes(x=cond,y=arousal, fill = cond,
                        colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                     adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = arousal, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = arousal, fill = cond),
                 outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_grid(arousal_measure~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("Arousal change after induction")+
    xlab("")
)

# analyze arousal at baseline
# center age group
long_df_arousal$agegroup.c<-ifelse(long_df_arousal$agegroup=="YA", -0.5, 0.5)
# use sum contrasts
long_df_arousal$cond<-as.factor(long_df_arousal$cond)
contrasts(long_df_arousal$cond)<-contr.Sum(levels(long_df_arousal$cond))

mod_arousal_baseline<-lmer(arousal~cond*agegroup+
                    (1|participant), data =long_df_arousal[long_df_arousal$arousal_measure=="baseline_arousal",] )

summary(mod_arousal_baseline)
Anova(mod_arousal_baseline, type = 3, test.statistic = "F")
eta_squared(mod_arousal_baseline, partial=T,alternative = "two.sided" )


#------------------------------------------------------------------------------#
# create 2 difference measures
# we can create two measures - one subtracting the baseline from the after induction'
# one subtracting the after induction from the after PM
#------------------------------------------------------------------------------#
# first, create a dataset with the valence after induction, with the before subtractedaft
aft_bef_ar<-arousal_aft_IND

# rename the variable with arousal "arousal_aft_ind"
aft_bef_ar$arousal_aft_ind_min_base<-aft_bef_ar$arousal_diff

print(
  # plot to check if there are condition differences
  ggplot(aft_bef_ar,aes(x=cond,y=arousal_aft_ind_min_base, fill = cond,
                     colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                     adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = arousal_aft_ind_min_base, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = arousal_aft_ind_min_base, fill = cond),
                 outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_wrap(.~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("Arousal change after induction")+
    xlab("")
)

# analyze
mod_arousal_change<-lmer(arousal_aft_ind_min_base~cond*agegroup+
                             (1|participant), data =aft_bef_ar )

summary(mod_arousal_change)
anova(mod_arousal_change)


#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# create the difference between after the PM and after the ind
# get the dataset with the raw arousal measures
long_df<-long_df_arousal

#
# select only arousal after the induction
long_df_aft_ind<-long_df[long_df$arousal_measure == "arousal_after_ind",]

# rename the variable
long_df_aft_ind$arousal_aft_ind<-long_df_aft_ind$arousal

# select after PM, only participant, valence, and Pm
long_df_aft_PM<-long_df[long_df$arousal_measure == "arousal_after_PM", 
                        c("participant","arousal",  "cond")]

names(long_df_aft_PM)[2]<-"arousal_after_PM"

# merge
long_df_ar_merged<-merge(long_df_aft_ind, long_df_aft_PM, by = c("participant", "cond"))

# now get the baseline
long_df_baseline<-long_df[long_df$arousal_measure=="baseline_arousal" , c("participant","arousal",  "cond")]

names(long_df_baseline)[2]<-"arousal_baseline"

# now we want to subtract the valence after ind from the valence after PM
long_df_ar_merged$aft_PM_minus_after_ind<-long_df_ar_merged$arousal_after_PM-long_df_ar_merged$arousal_aft_ind

# now merge baseline
long_df_ar_merged<-merge(long_df_ar_merged, long_df_baseline, by =c("participant", "cond"))

# now create the after induction minus the baseline
long_df_ar_merged$arousal_aftIND_min_bef<-long_df_ar_merged$arousal_aft_ind-long_df_ar_merged$arousal_baseline

# first check if this so-obtained arousal after induction minus baseline is the 
# same as the one previously calculated
# sort by participant and condition both datasets
arousal_aft_IND <- arousal_aft_IND %>%
  arrange(participant, cond) 

long_df_ar_merged <- long_df_ar_merged %>%
  arrange(participant, cond) 

# now plot the correlation
plot(arousal_aft_IND$arousal_diff, long_df_ar_merged$arousal_aftIND_min_bef,
     type = "l")

# reorder age
long_df_ar_merged$agegroup<-factor(long_df_ar_merged$agegroup, levels = c("YA", "OA"))

# check the number of participants
kable(long_df_merged %>%
        group_by(participant) %>%
        slice(1) %>%
        group_by( agegroup)%>%
        tally())

# plot to check if there are condition differences
print(
  ggplot(long_df_ar_merged,aes(x=cond,y=arousal_aftIND_min_bef, fill = cond,
                            colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                     adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = arousal_aftIND_min_bef, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = arousal_aftIND_min_bef, fill = cond),
                 outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_wrap(.~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("arousal change after induction")+
    xlab("")
)

print(
  ggplot(long_df_ar_merged,aes(x=cond,y=aft_PM_minus_after_ind, fill = cond,
                            colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                     adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = aft_PM_minus_after_ind, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = aft_PM_minus_after_ind, fill = cond),
                 outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_wrap(.~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("Arousal change after PM - after the induction")+
    xlab("")
)

# baseline
print(
  ggplot(long_df_ar_merged,aes(x=cond,y=arousal_baseline, fill = cond,
                            colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                     adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = arousal_baseline, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = arousal_baseline, fill = cond)
                 ,outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_wrap(.~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("Arousal baseline")+
    xlab("")
)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Merge all in a long dataset
# standardize the coefficients
long_df_ar_merged <- long_df_ar_merged %>%
  mutate(aft_PM_minus_after_ind.s = as.numeric(scale(aft_PM_minus_after_ind)),
         arousal_aftIND_min_bef.s = as.numeric(scale(arousal_aftIND_min_bef))) %>%
  ungroup()


long_prep<-long_df_ar_merged[,c(  "participant" , "cond", "arousal_aftIND_min_bef.s", 
                               "aft_PM_minus_after_ind.s" , "agegroup" )]

library(tidyr)


# df is your wide table (participant, cond, arousal_after_PM, aft_PM_minus_after_ind, arousal_baseline)
long_df_ar_split <- long_prep %>%
  pivot_longer(
    cols = c(arousal_aftIND_min_bef.s, aft_PM_minus_after_ind.s),
    names_to  = "arousal_measure",
    values_to = "arousal_value",
    values_drop_na = TRUE
  ) %>%
  # (optional) nicer labels & ordering for the measure factor
  mutate(
    arousal_measure = dplyr::recode(arousal_measure,
                                    arousal_aftIND_min_bef.s    = "post_induction",
                                    aft_PM_minus_after_ind.s    = "post_task"
    ),
    arousal_measure = factor(arousal_measure,
                             levels = c("post_induction", "post_task"))
  )

# Result columns: participant, cond, valence_measure, valence_value, plus any other original columns
long_df_ar_split$arousal_value<-as.numeric(long_df_ar_split$arousal_value)

# check again the participants
kable(long_df_ar_split %>%
        group_by(participant) %>%
        slice(1) %>%
        group_by( agegroup)%>%
        tally())


# plot
print(
  ggplot(long_df_ar_split,aes(x=cond,y=arousal_value, fill = cond,
                                   colour = cond))+
    geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE, alpha = .4)+
    geom_point(aes(x = cond, y = arousal_value, fill = cond),
               position = position_jitter(width = .05), size = 1, shape = 20)+
    geom_boxplot(aes(x = (cond), y = arousal_value, fill = cond),outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
    facet_grid(arousal_measure~agegroup)+
    theme_classic()+
    
    theme(legend.position = "none")+
    params+
    ylab("Arousal")+
    xlab("")+params
)
#------------------------------------------------------------------------------#
# analysis of arousal
#------------------------------------------------------------------------------#

# Group data by Condition, calculate mean and SD
summary_table_arousal <-   long_df_ar_split %>%
  group_by(arousal_measure, cond, agegroup) %>%
  summarize(
    Mean = mean(arousal_value, na.rm = TRUE),
    SD = sd(arousal_value, na.rm = TRUE),
    .groups = 'drop'  # To avoid message about grouping
  )


summary_table_arousal


# analyze# analyzelong_df_baseline
long_df_ar_split$agegroup.c<-ifelse(long_df_ar_split$agegroup=="YA", -0.5, 0.5)

mod_arousal<-lmer(arousal_value~agegroup*arousal_measure*cond +(1|participant), 
                  data = long_df_ar_split)


summary(mod_arousal)
tab_model(mod_arousal)
Anova(mod_arousal, type=3 )

eta_squared(mod_arousal, partial=T,alternative = "two.sided" )

emm_mean_ar<-emmeans(mod_arousal, ~ arousal_measure*agegroup | cond )

# 2) Omnibus tests within each arousal_measure (does cond matter? any agxcond interaction?)
joint_tests(emm_mean_ar, by = "arousal_measure")


# 3) Pairwise contrasts of cond within each agegroup and measure 
pairs(emm_mean_ar,
      by     = c( "agegroup", "cond"),
      adjust = "bonferroni")


# now break down the valence by condition interaction
emm_mean2_ar<-emmeans(mod_arousal, ~ agegroup | arousal_measure+cond )


pairs(emm_mean2_ar,
      
      adjust = "bonferroni")

print(emm_mean2_ar)

print(
  emmip(mod_arousal, agegroup ~ cond | arousal_measure, CIs = TRUE) +
    theme_minimal() + labs(y = "Estimated arousal", x = "Condition", color = "Age group")+
    # scale_color_manual(values = c(YA = "darkorange", OA = "darkgreen")) +
    scale_color_manual(values = okabe_ito,
                       labels = c(YA = "YA", OA = "OA")) +
    geom_hline(aes(yintercept = 0), color = "black")  +
    
    #scale_fill_manual(values = c(YA = "darkorange", OA = "darkgreen")) +  # CI ribbons
    scale_fill_manual(values = okabe_ito,
                      labels = c(YA = "YA", OA = "OA")) +  # CI ribbons
    
    theme_classic()+params+
    facet_wrap(
      ~ arousal_measure,
      labeller = lab_valence
    )
)

ggsave("Write_up/figures/arousal_all_contrasts.png", width = 12, height = 6, dpi = 300)
