#------------------------------------------------------------------------------#
# mediation analyeses 
#------------------------------------------------------------------------------#



# after the induction change
model_aft_ind_intr <- paste( '
  level: 1
    PM_task_lenient_av_log ~ c*valence_aftIND_min_bef.s
    c_tcaq ~ a*valence_aftIND_min_bef.s
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
  total_valence := c + ab 

'
)

fit_after_ind <- sem(model = model_aft_ind_intr,
                     data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], cluster = "participant",
                     estimator = "MLR",
                     missing = "ML")

print(
summary(fit_after_ind, std=T)
)
#pe_med1 <- parameterEstimates(model_aft_ind_intr, standardized = TRUE, ci = TRUE)

#------------------------------------------------------------------------------#
# now after PM minus after induction
# with TCAQ
model_aft_pm <- paste( '
  level: 1
    PM_task_lenient_av ~ c*aft_PM_minus_after_ind.s
    c_tcaq ~ a*aft_PM_minus_after_ind.s
    PM_task_lenient_av ~ b*c_tcaq  

  level: 2   # random intercepts only
    # estimate the variance at level two 
    PM_task_lenient_av ~~ PM_task_lenient_av
    c_tcaq~~ c_tcaq
      aft_PM_minus_after_ind.s ~~ aft_PM_minus_after_ind.s


    # (optional) valence_aft_ind ~~ c_tcaq + PM_task_lenient_av
    # (optional) aft_PM_minus_after_ind ~~ c_tcaq + PM_task_lenient_av

  # within-level indirects and totals
  ab := a*b; 
  total_valence := c + ab 

'
)


fit_after_PM <- sem(model = model_aft_pm, data = 
                      long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], cluster = "participant", 
                    standardized = T)
print(
summary(fit_after_PM, std=T)
)

