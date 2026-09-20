#------------------------------------------------------------------------------#
# Analyze Valence data
#------------------------------------------------------------------------------#

source("RainCloudPlots-master/tutorial_R/R_rainclouds.R")
source("RainCloudPlots-master/tutorial_R/summarySE.R")

# load the dfs
#long_df_valence<-read.csv("group_data/long_df_valence_diff.csv")

# only taking the difference between the baseline and the valence after the induction
valence_aft_IND<-
  long_df_valence_diff[long_df_valence_diff$valence_diff_measure=="valence_bef_after_ind",]


#df_wide<-read.csv("group_data/df_wide.csv")

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
# create 2 difference measures
# we can create two measures - one subtracting the baseline from the after induction'
# one subtracting the after induction from the after PM
#------------------------------------------------------------------------------#
# first, create a dataset with the valence after induction, with the before subtractedaft
aft_bef<-valence_aft_IND

# rename the variable with valence "valence_aft_ind"
aft_bef$valence_aft_ind_min_base<-aft_bef$valence_diff

print(
# plot to check if there are condition differences
ggplot(aft_bef,aes(x=cond,y=valence_aft_ind_min_base, fill = cond,
                   colour = cond))+
  geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                  adjust =2, trim = FALSE, alpha = .4)+
  geom_point(aes(x = cond, y = valence_aft_ind_min_base, fill = cond),
             position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = (cond), y = valence_aft_ind_min_base, fill = cond),
               outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
  facet_wrap(.~agegroup)+
  theme_classic()+
  
  theme(legend.position = "none")+
  #params+
  ylab("Valence change after induction")+
  xlab("")
)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# create the difference between after the PM and after the ind
# get the dataset with the raw valence measures
long_df<-long_df_valence

#
# select only valence after the induction
long_df_aft_ind<-long_df[long_df$valence_measure == "valence_after_ind",]

# rename the variable
long_df_aft_ind$valence_aft_ind<-long_df_aft_ind$valence

# select after PM, only participant, valence, and Pm
long_df_aft_PM<-long_df[long_df$valence_measure == "valence_after_PM", 
                        c("participant","valence",  "cond")]

names(long_df_aft_PM)[2]<-"valence_after_PM"

# merge
long_df_merged<-merge(long_df_aft_ind, long_df_aft_PM, by = c("participant", "cond"))

# now get the baseling
long_df_baseline<-long_df[long_df$valence_measure=="baseline_valence" , c("participant","valence",  "cond")]

names(long_df_baseline)[2]<-"valence_baseline"

# now we want to subtract the valence after ind from the valence after PM
long_df_merged$aft_PM_minus_after_ind<-long_df_merged$valence_after_PM-long_df_merged$valence_aft_ind

# now merge baseling
long_df_merged<-merge(long_df_merged, long_df_baseline, by =c("participant", "cond"))

# now create the after induction minus the baseline
long_df_merged$valence_aftIND_min_bef<-long_df_merged$valence_aft_ind-long_df_merged$valence_baseline

# first check if this so-obtained valence after induction minus baseline is the 
# same as the one previously calculated
# sort by participant and condition both datasets
valence_aft_IND <- valence_aft_IND %>%
  arrange(participant, cond) 

long_df_merged <- long_df_merged %>%
  arrange(participant, cond) 

# now plot the correlation
#plot(valence_aft_IND$valence_diff, long_df_merged$valence_aftIND_min_bef,
#     type = "l")

# reorder age
long_df_merged$agegroup<-factor(long_df_merged$agegroup, levels = c("YA", "OA"))

# check the number of participants
kable(long_df_merged %>%
        group_by(participant) %>%
        slice(1) %>%
        group_by( agegroup)%>%
        tally())

# plot to check if there are condition differences
print(
ggplot(long_df_merged,aes(x=cond,y=valence_aftIND_min_bef, fill = cond,
                          colour = cond))+
  geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                   adjust =2, trim = FALSE, alpha = .4)+
  geom_point(aes(x = cond, y = valence_aftIND_min_bef, fill = cond),
             position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = (cond), y = valence_aftIND_min_bef, fill = cond),
               outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
  facet_wrap(.~agegroup)+
  theme_classic()+
  
  theme(legend.position = "none")+
  #params+
  ylab("Valence valence_after_ind")+
  xlab("")
)

print(
ggplot(long_df_merged,aes(x=cond,y=aft_PM_minus_after_ind, fill = cond,
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
 # params+
  ylab("Valence change after PM - after the induction")+
  xlab("")
)

# baseline
print(
ggplot(long_df_merged,aes(x=cond,y=valence_baseline, fill = cond,
                          colour = cond))+
  geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),
                   adjust =2, trim = FALSE, alpha = .4)+
  geom_point(aes(x = cond, y = valence_baseline, fill = cond),
             position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = (cond), y = valence_baseline, fill = cond)
               ,outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
  facet_wrap(.~agegroup)+
  theme_classic()+
  
  theme(legend.position = "none")+
  #params+
  ylab("Valence baseline")+
  xlab("")
)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Merge all in a long dataset
# standardize the coefficients
long_df_merged <- long_df_merged %>%
  mutate(aft_PM_minus_after_ind.s = as.numeric(scale(aft_PM_minus_after_ind)),
         valence_aftIND_min_bef.s = as.numeric(scale(valence_aftIND_min_bef))) %>%
  ungroup()


long_prep<-long_df_merged[,c(  "participant" , "cond", "valence_aftIND_min_bef.s", 
                               "aft_PM_minus_after_ind.s" , "agegroup" )]

library(tidyr)


# df is your wide table (participant, cond, valence_after_PM, aft_PM_minus_after_ind, valence_baseline)
long_df_valence_split <- long_prep %>%
  pivot_longer(
    cols = c(valence_aftIND_min_bef.s, aft_PM_minus_after_ind.s),
    names_to  = "valence_measure",
    values_to = "valence_value",
    values_drop_na = TRUE
  ) %>%
  # (optional) nicer labels & ordering for the measure factor
  mutate(
    valence_measure = dplyr::recode(valence_measure,
                                    valence_aftIND_min_bef.s    = "post_induction",
                                    aft_PM_minus_after_ind.s    = "post_task"
    ),
    valence_measure = factor(valence_measure,
                             levels = c("post_induction", "post_task"))
  )

# Result columns: participant, cond, valence_measure, valence_value, plus any other original columns
long_df_valence_split$valence_value<-as.numeric(long_df_valence_split$valence_value)

# check again the participants
kable(long_df_valence_split %>%
        group_by(participant) %>%
        slice(1) %>%
        group_by( agegroup)%>%
        tally())


# plot
print(
ggplot(long_df_valence_split,aes(x=cond,y=valence_value, fill = cond,
                           colour = cond))+
  geom_flat_violin(aes(fill = cond), position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE, alpha = .4)+
  geom_point(aes(x = cond, y = valence_value, fill = cond),
             position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = (cond), y = valence_value, fill = cond),outlier.shape = NA, alpha = 0.3, width = .1,colour = "black") +
  facet_grid(valence_measure~agegroup)+
  theme_classic()+
  
  theme(legend.position = "none")+
  #params+
  ylab("Valence")+
  xlab("")
)
