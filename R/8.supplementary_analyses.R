#------------------------------------------------------------------------------#
# Supplementary Splines analysis
#------------------------------------------------------------------------------#
  # Spline analysis2 - exclude the zero
  
  # now we want to know whether it is positive or negative side that drives the effect
  long_df_merged$post_ind_pos_spline_nozero<- 
    ifelse(long_df_merged$valence_aftIND_min_bef==0,NA, 
           long_df_merged$post_ind_pos_spline )
  
  long_df_merged$post_ind_neg_spline_nozero<- 
    ifelse(long_df_merged$valence_aftIND_min_bef==0,NA,
           long_df_merged$post_ind_neg_spline )
  
  long_df_merged$post_PM_pos_spline_no_zero<- 
    ifelse(long_df_merged$aft_PM_minus_after_ind==0,
           NA, long_df_merged$post_PM_pos_spline)
  long_df_merged$post_PM_neg_spline_no_zero<- 
    ifelse(long_df_merged$aft_PM_minus_after_ind==0,
           NA, long_df_merged$post_PM_neg_spline )
  
  # scale the predictors
  long_df_merged$post_ind_pos_spline_nozero.s<-scale(long_df_merged$post_ind_pos_spline_nozero)
  long_df_merged$post_ind_neg_spline_nozero.s<-scale(long_df_merged$post_ind_neg_spline_nozero)
  long_df_merged$post_PM_pos_spline_no_zero.s<-scale(long_df_merged$post_PM_pos_spline_no_zero)
  long_df_merged$post_PM_neg_spline_no_zero.s<-scale(long_df_merged$post_PM_neg_spline_no_zero)
  
  mod_splines_nozero<-lmer(PM_task_lenient_av_log~
                             agegroup.c*order_c+DS.c+Mill_Hill.c+
                             
                             post_ind_pos_spline_nozero.s+post_ind_neg_spline_nozero.s+
                             + agegroup.c+
                             post_PM_pos_spline_no_zero.s+post_PM_neg_spline_no_zero.s+
                             (1|participant), 
                           data = long_df_merged[ (is.na(long_df_merged$MOCA)|
                                                     long_df_merged$MOCA>=26) ,],
                           control =  lmerControl(optimizer = "bobyqa", 
                                                  optCtrl = list(maxfun = 100000)))
  tab_model(mod_splines_nozero)
  
  anova(mod_splines_nozero)
  
  
  
  # -----------------------------------------------------------------------------#
  # run the regression with arousal change
  long_df_ar_merged$PM_task_lenient_av_log<-
    car::logit(long_df_ar_merged$PM_task_lenient_av, percents = F, adjust = minimum )
  
  reg_ar<-lmer(PM_task_lenient_av_log~ aft_PM_minus_after_ind*agegroup.c + 
                 arousal_aftIND_min_bef*agegroup.c+(1|participant), 
               data = long_df_ar_merged[ (is.na(long_df_ar_merged$MOCA)|
                                            long_df_ar_merged$MOCA>=26),])
  
  tab_model(reg_ar)
  
  (
    anova(reg_ar)
  )
  
  long_df_ar_merged$aft_PM_minus_after_ind_ar.s<-long_df_ar_merged$aft_PM_minus_after_ind.s
  
  # merge the main df with arousal and run the analysis controlling for arousal
  long_df_merged_val_ar<-merge(long_df_merged,
                               long_df_ar_merged[, c(
                                 "participant", "cond",
                                 "aft_PM_minus_after_ind_ar.s",
                                 "arousal_aftIND_min_bef.s")],
                               by = c("participant", "cond"))
  
  reg_ar<-lmer(PM_task_lenient_av_log~ aft_PM_minus_after_ind_ar.s*agegroup.c + 
                 arousal_aftIND_min_bef.s*agegroup.c+(1|participant), 
               data = long_df_merged_val_ar[ (is.na(long_df_merged_val_ar$MOCA)|
                                                long_df_merged_val_ar$MOCA>=26),])
  
  tab_model(reg_ar)
  
  
  reg.cont.s<-lmer(PM_task_lenient_av_log~aft_PM_minus_after_ind_ar.s+ 
                     arousal_aftIND_min_bef.s+
                     aft_PM_minus_after_ind.s*agegroup.c + 
                     valence_aftIND_min_bef.s*agegroup.c+(1|participant), 
                   data = long_df_merged_val_ar[ (is.na(long_df_merged_val_ar$MOCA)|
                                                    long_df_merged_val_ar$MOCA>=26),])
  
  tab_model(reg.cont.s, 
            file = "regression controlling for arousal.html"
  )
  