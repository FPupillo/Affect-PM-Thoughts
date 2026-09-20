#------------------------------------------------------------------------------#
# PM main analysis
#------------------------------------------------------------------------------#

#instead of applying the previous custom-made transformation, use the logit
# function in the car package, using the lowest non-zero proportion in the "adjust" section
# get the minimum non-zero proportion
minimum<-min(long_df_merged$PM_task_lenient_av[long_df_merged$PM_task_lenient_av!=0])

long_df_merged$PM_task_lenient_av_log<-car::logit(long_df_merged$PM_task_lenient_av, percents = F, adjust = minimum )

long_df_merged$agegroup.c<-ifelse(long_df_merged$agegroup=="YA",-0.5, 0.5 )

#------------------------------------------------------------------------------#
# Include the order of the PM task and its interaction with age group (1st, 2nd, or third block-day) 
# affected PM

long_df_merged$order<-as.numeric(long_df_merged$order)

# center order
long_df_merged$order_c<-ifelse(long_df_merged$order==1, -1, 
                               ifelse(long_df_merged$order==2, 0, 
                                      1))

# Center also Digit symbol (processing speed)
long_df_merged$DS.c<-scale(long_df_merged$DS)

# Center also Mill Hill (Crystallized intelligence)
long_df_merged$Mill_Hill.c<-scale(long_df_merged$Mill_Hill)

# regression with all the control variables
reg.s<-lmer(PM_task_lenient_av_log~order_c*agegroup.c+DS.c+Mill_Hill.c+aft_PM_minus_after_ind.s*agegroup.c + 
              valence_aftIND_min_bef.s*agegroup.c+(1+order_c|participant), 
            data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])
summary(reg.s)

tab_model(reg.s, file = "main_regression_table.html")


anova(reg.s)


vif(reg.s)

eta_squared(reg.s, partial = T, alternative = "two.sided")

# check the interaction between age and DS
age_DS<-lmer(PM_task_lenient_av_log~agegroup*DS.c+(1|participant), 
             data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

summary(age_DS)


interact_plot(age_DS,
              
              pred = DS.c,
              
              modx = agegroup,
              
              interval = TRUE,
              legend.main = "Age group"
)+
  labs(x = "Fluid intelligence (centered)", 
       y = "PM performance")+
  theme_classic()+
  labs(colour = "Age group")

# without centering
reg_order<-lmer(PM_task_lenient_av_log~ Mill_Hill.c+DS.c+order_c*agegroup.c+aft_PM_minus_after_ind*agegroup.c + 
                  valence_aftIND_min_bef*agegroup.c+(1|participant), 
                data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])


Anova(reg_order, type =2)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# reshap to plot

long_df_merged_all_long<- long_df_merged %>%
  pivot_longer(
    cols = c(,valence_aftIND_min_bef, aft_PM_minus_after_ind),
    names_to = "valence_diff_measure",
    values_to = "valence_change",
    values_drop_na = F
  )%>%
  # (optional) nicer labels & ordering for the measure factor
  mutate(
    valence_diff_measure = dplyr::recode(valence_diff_measure,
                                         valence_aftIND_min_bef  = "post_induction",
                                         aft_PM_minus_after_ind    = "post_task",
    ),
    valence_diff_measure = factor(valence_diff_measure,
                                  levels = c ("post_induction", "post_task"))
  )

# reorder level
long_df_merged_all_long$valence_diff_measure<-factor(long_df_merged_all_long$valence_diff_measure, 
                                                     levels =  c("post_induction", "post_task"))


# 1) Predicted curve for post-induction (hold post-task at 0)
pred_T2T1 <- ggpredict(
  reg_order,
  terms = c("valence_aftIND_min_bef", "agegroup.c"),
  condition = c(aft_PM_minus_after_ind.s = 0)   # set the other predictor to 0 (its natural reference)
) %>%
  as_tibble() %>%
  mutate(measure = "post_induction",
         x = x)   # keep x for clarity

# 2) Predicted curve for post-task (hold post-induction at 0)
pred_T3T2 <- ggpredict(
  reg_order,
  terms = c("aft_PM_minus_after_ind", "agegroup.c"),
  condition = c(valence_aftIND_min_bef = 0)
) %>%
  as_tibble() %>%
  mutate(measure = "post_task",
         x = x)

# 3) Combine and plot
pred_all <- bind_rows(pred_T2T1, pred_T3T2)

print(
  ggplot(pred_all, aes(x = x, y = predicted, colour = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
                alpha = 0.15, colour = NA) +
    geom_line(size = 1) +
    facet_wrap(~ measure, nrow = 1, labeller = lab_valence) +
    labs(x = "Valence", y = "Predicted PM accuracy", colour = "Age group", fill = "Age group") +
    scale_color_manual(values = c("-0.5" = "#0072B2",  # blue
                                  "0.5" = "#D55E00"),  # vermillion, 
                       labels = c("-0.5" = "YA", "0.5" = "OA")) +
    #scale_fill_manual(values = c(YA = "darkorange", OA = "darkgreen")) +  # CI ribbons
    scale_fill_manual(values = c("-0.5" = "#0072B2",  # blue
                                 "0.5" = "#D55E00"),  # vermillion, 
                      labels = c("-0.5" = "YA", "0.5" = "OA"))  +  # CI ribbons
    geom_vline(xintercept = 0)+
    theme_classic()+
    theme(legend.position = "bottom")+
    params
  
)

ggsave("Write_up/figures/predicted_valence_aftIND_aftPM_PM.png", width = 12, height = 8, dpi = 300)

#------------------------------------------------------------------------------#
# Spline analysis

# now we want to know whether it is positive or negative side that drives the effect
long_df_merged$post_ind_pos_spline<-
  ifelse(long_df_merged$valence_aftIND_min_bef>0,
         long_df_merged$valence_aftIND_min_bef, 0 )

long_df_merged$post_ind_neg_spline<- 
  ifelse(long_df_merged$valence_aftIND_min_bef<0,
         long_df_merged$valence_aftIND_min_bef, 0 )

long_df_merged$post_PM_pos_spline<- 
  ifelse(long_df_merged$aft_PM_minus_after_ind>0,
         long_df_merged$aft_PM_minus_after_ind, 0 )

long_df_merged$post_PM_neg_spline<- 
  ifelse(long_df_merged$aft_PM_minus_after_ind<0,
         long_df_merged$aft_PM_minus_after_ind, 0 )

# scale the predictors
long_df_merged$post_ind_pos_spline.s<-scale(long_df_merged$post_ind_pos_spline)
long_df_merged$post_ind_neg_spline.s<-scale(long_df_merged$post_ind_neg_spline)
long_df_merged$post_PM_pos_spline.s<-scale(long_df_merged$post_PM_pos_spline)
long_df_merged$post_PM_neg_spline.s<-scale(long_df_merged$post_PM_neg_spline)

mod_splines<-lmer(PM_task_lenient_av_log~
                    agegroup.c*order_c+DS.c+Mill_Hill.c+
                    post_ind_pos_spline+post_ind_neg_spline+
                    + agegroup.c+
                    post_PM_pos_spline+post_PM_neg_spline+
                    (1|participant), 
                  data = long_df_merged[ (is.na(long_df_merged$MOCA)|
                                            long_df_merged$MOCA>=26) ,],
                  control =  lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 100000)))

tab_model(mod_splines)

vif(mod_splines)


mod_splines.s<-lmer(PM_task_lenient_av_log~
                      agegroup.c*order_c+DS.c+Mill_Hill.c+
                      
                    post_ind_pos_spline.s+post_ind_neg_spline.s+
                    + agegroup.c+
                    post_PM_pos_spline.s+post_PM_neg_spline.s+
                    (1|participant), 
                  data = long_df_merged[ (is.na(long_df_merged$MOCA)|
                                            long_df_merged$MOCA>=26) ,],
                  control =  lmerControl(optimizer = "bobyqa", optCtrl =
                                           list(maxfun = 100000)))

print(summary(mod_splines.s))
tab_model(mod_splines.s)

vif(mod_splines)

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
