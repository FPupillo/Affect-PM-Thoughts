#------------------------------------------------------------------------------#
# associational variable analysis
#------------------------------------------------------------------------------#
library(parameters)

# association between thoughts and valence
mod_thouhgts_task_paths<-lmer(c_tcaq~+aft_PM_minus_after_ind.s+
                                valence_aftIND_min_bef.s+
                                DS.c+Mill_Hill.c+agegroup.c+order_c+
                          (1+order_c|participant), 
                        long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

summary(mod_thouhgts_task_paths)
standardize_parameters(mod_thouhgts_task_paths)

# valence after the induction on pm
reg_aft_ind<-lmer(PM_task_lenient_av_log~  
            valence_aftIND_min_bef.s+
              aft_PM_minus_after_ind.s + 
              DS.c+Mill_Hill.c+order_c+
              (1+order_c|participant), 
          data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(reg_aft_ind)

anova(reg_aft_ind)

standardize_parameters(reg_aft_ind)

# now add thoughts
reg_aft_ind_thoughts<-lmer(PM_task_lenient_av_log~  
                            valence_aftIND_min_bef.s+aft_PM_minus_after_ind.s + 
                            DS.c+Mill_Hill.c+order_c+ 
                            c_tcaq+
                            (1+order_c|participant), 
                          data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(reg_aft_ind_thoughts)
standardize_parameters(reg_aft_ind_thoughts)

# thoughts alone on PM
PM_thoughts<-lmer(PM_task_lenient_av_log~  
                    DS.c+Mill_Hill.c+order_c+ 
                             c_tcaq+(1|participant), 
                           data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(PM_thoughts)
standardize_parameters(PM_thoughts)

# now the two variables mood
reg_aft_ind_aft_PM<-lmer(valence_aftIND_min_bef.s~aft_PM_minus_after_ind.s +
                           DS.c+Mill_Hill.c+order_c+ 
                           (1|participant), 
                           data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(reg_aft_ind_aft_PM)

standardize_parameters(reg_aft_ind_aft_PM)


long_df_merged$valence_aftIND_agegroup<-long_df_merged$valence_aftIND_min_bef.s*long_df_merged$agegroup.c
long_df_merged$valence_aftPM_agegroup<-long_df_merged$aft_PM_minus_after_ind.s*long_df_merged$agegroup.c

model_aft_ind_intr <- paste( '
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

fit_assoc_path <- sem(model = model_aft_ind_intr,
                     data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], cluster = "participant",
                     estimator = "MLR",
                     missing = "ML")

print(
  summary(fit_assoc_path, std=T, fit.measures = T)
)

#------------------------------------------------------------------------------#

# now add all the control variables and age group
# add the interactions with agegroup
long_df_merged$valence_aftIND_agegroup<-long_df_merged$valence_aftIND_min_bef.s*long_df_merged$agegroup.c
long_df_merged$valence_aftPM_agegroup<-long_df_merged$aft_PM_minus_after_ind.s*long_df_merged$agegroup.c
long_df_merged$tcaq_c_agegroup<-long_df_merged$c_tcaq.s*long_df_merged$agegroup.c
# add also order and 

model_all <- paste( '
  level: 1
    PM_task_lenient_av_log ~ c*valence_aftIND_min_bef.s+d1*agegroup.c+f1*DS+h1*order_c+l1*c_tcaq
    PM_task_lenient_av_log ~ c2*aft_PM_minus_after_ind.s+d2*agegroup.c+f2*DS+h2*order_c+l2*c_tcaq
    c_tcaq ~ a*valence_aftIND_min_bef.s+e1*agegroup.c+g1*DS+i1*order_c
   c_tcaq ~ a2*aft_PM_minus_after_ind.s+e2*agegroup.c+g2*DS+i2*order_c

    PM_task_lenient_av_log ~ b*c_tcaq 

  level: 2   # random intercepts only
    # estimate the variance at level two 
    PM_task_lenient_av_log ~~ PM_task_lenient_av_log
    c_tcaq~~ c_tcaq
      #valence_aft_ind ~~ valence_aft_ind

    # (optional) valence_aft_ind ~~ c_tcaq + PM_task_lenient_av
    # (optional) aft_PM_minus_after_ind ~~ c_tcaq + PM_task_lenient_av

  # within-level indirects and totals
  ab := a*b; 
    a2b := a2*b; 

  total_valence := c + ab 
  total_valence2 := c2 + a2b 


'
)

fit_after_ind_all <- sem(model = model_all,
                     data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], cluster = "participant",
                     estimator = "MLR",
                     missing = "ML")

print(
  summary(fit_after_ind_all, std=T, fit.measures=TRUE)
)
