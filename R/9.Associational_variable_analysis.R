#------------------------------------------------------------------------------#
# associational variable analysis
#------------------------------------------------------------------------------#
library(parameters)

# valence after the induction on pm, without considering intrusive thoughts
valence_PM<-lmer(PM_task_lenient_av_log~  
                   valence_aftIND_min_bef.s+
                   aft_PM_minus_after_ind.s + 
                   DS.c+Mill_Hill.c+order_c+
                   (1+order_c|participant), 
                 data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(valence_PM)

anova(valence_PM)

standardize_parameters(valence_PM)

# association between thoughts and valence
mod_thouhgts_task_paths<-lmer(c_tcaq~+aft_PM_minus_after_ind.s+
                                valence_aftIND_min_bef.s+
                                DS.c+Mill_Hill.c+agegroup.c+order_c+
                          (1+order_c|participant), 
                        long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

summary(mod_thouhgts_task_paths)
standardize_parameters(mod_thouhgts_task_paths)



# now thought alone
PM_thoughts<-lmer(PM_task_lenient_av_log~  
                            DS.c+Mill_Hill.c+order_c+ 
                            c_tcaq+
                            (1+order_c|participant), 
                          data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(PM_thoughts)
anova(PM_thoughts)
standardize_parameters(PM_thoughts)


standardize_parameters(PM_thoughts)



long_df_merged$valence_aftIND_agegroup<-long_df_merged$valence_aftIND_min_bef.s*long_df_merged$agegroup.c
long_df_merged$valence_aftPM_agegroup<-long_df_merged$aft_PM_minus_after_ind.s*long_df_merged$agegroup.c

associational_variable_analysis <- paste( '
  level: 1
    PM_task_lenient_av_log ~ c*valence_aftIND_min_bef.s + DS.c+Mill_Hill.c+agegroup.c+order_c+valence_aftIND_agegroup+valence_aftPM_agegroup
    PM_task_lenient_av_log ~ c2*aft_PM_minus_after_ind.s+DS.c+Mill_Hill.c+agegroup.c+order_c+valence_aftIND_agegroup+valence_aftPM_agegroup
    c_tcaq ~ a*valence_aftIND_min_bef.s+DS.c+Mill_Hill.c+agegroup.c+order_c+valence_aftIND_agegroup
   c_tcaq ~ a2*aft_PM_minus_after_ind.s+DS.c+Mill_Hill.c+agegroup.c+order_c+valence_aftPM_agegroup

    PM_task_lenient_av_log ~ b*c_tcaq +DS.c+Mill_Hill.c+agegroup.c+order_c

  level: 2   # random intercepts only
    # estimate the variance at level two 
    PM_task_lenient_av_log ~~ PM_task_lenient_av_log
    c_tcaq~~ c_tcaq
      #valence_aft_ind ~~ valence_aft_ind

    # (optional) valence_aft_ind ~~ c_tcaq + PM_task_lenient_av
    # (optional) aft_PM_minus_after_ind ~~ c_tcaq + PM_task_lenient_av

  # within-level indirects and totals
    a2b := a2*b; 
    ab := a*b;

  total_valence := c + ab 
  total_valence2 := c2 + a2b 


'
)

fit_assoc_path <- sem(model = associational_variable_analysis,
                     data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], cluster = "participant",
                     estimator = "MLR",
                     missing = "ML")

print(
  summary(fit_assoc_path, std=T, fit.measures = T)
)

#------------------------------------------------------------------------------#

