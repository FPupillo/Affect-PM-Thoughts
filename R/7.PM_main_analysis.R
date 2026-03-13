#------------------------------------------------------------------------------#
# PM main analysis
#------------------------------------------------------------------------------#

#instead of applying the previous custom-made transformation, use the logit
# function in the car package, using the lowest non-zero proportion in the "adjust" section
# get the minimum non-zero proportion
minimum<-min(long_df_merged$PM_task_lenient_av[long_df_merged$PM_task_lenient_av!=0])

long_df_merged$PM_task_lenient_av_log<-car::logit(long_df_merged$PM_task_lenient_av, percents = F, adjust = minimum )

long_df_merged$agegroup.c<-ifelse(long_df_merged$agegroup=="YA",-0.5, 0.5 )

# run the multiple regression
reg<-lmer(PM_task_lenient_av_log~ aft_PM_minus_after_ind*agegroup.c + 
            valence_aftIND_min_bef*agegroup.c+(1|participant), 
          data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])

(
anova(reg)
)

eta_squared(reg, partial = T, alternative = "two.sided")

(
summary(reg)
)
tab_model(reg)

vif(reg)

# standardize the coefficients
long_df_merged <- long_df_merged %>%
  mutate(aft_PM_minus_after_ind.s = as.numeric(scale(aft_PM_minus_after_ind)),
         valence_aftIND_min_bef.s = as.numeric(scale(valence_aftIND_min_bef))) %>%
  ungroup()

# analyze the standardized scores
reg.s<-lmer(PM_task_lenient_av_log~ aft_PM_minus_after_ind.s*agegroup.c + 
              valence_aftIND_min_bef.s*agegroup.c+(1|participant), 
            data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26),])


print(
anova(reg.s)
)

print(
eta_squared(reg.s, partial = T, alternative = "two.sided")
)
tab_model(reg.s)

print(summary(reg.s))
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

# do a plot of the predicted values

# 1) Predicted curve for post-induction (hold post-task at 0)
pred_T2T1 <- ggpredict(
  reg,
  terms = c("valence_aftIND_min_bef", "agegroup.c"),
  condition = c(aft_PM_minus_after_ind = 0)   # set the other predictor to 0 (its natural reference)
) %>%
  as_tibble() %>%
  mutate(measure = "post_induction",
         x = x)   # keep x for clarity

# 2) Predicted curve for post-task (hold post-induction at 0)
pred_T3T2 <- ggpredict(
  reg,
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
  theme_classic()+
  params

)

ggsave("Write_up/figures/predicted_valence_aftIND_aftPM_PM.png", width = 12, height = 6, dpi = 300)

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Spline analysis

# now we want to know whether it is positive or negative side that drives the effect
long_df_merged$post_ind_pos_spline<- ifelse(long_df_merged$valence_aftIND_min_bef>0,long_df_merged$valence_aftIND_min_bef, 0 )
long_df_merged$post_ind_neg_spline<- ifelse(long_df_merged$valence_aftIND_min_bef<0,long_df_merged$valence_aftIND_min_bef, 0 )

long_df_merged$post_PM_pos_spline<- ifelse(long_df_merged$aft_PM_minus_after_ind>0,long_df_merged$aft_PM_minus_after_ind, 0 )
long_df_merged$post_PM_neg_spline<- ifelse(long_df_merged$aft_PM_minus_after_ind<0,long_df_merged$aft_PM_minus_after_ind, 0 )

# scale the predictors
long_df_merged$post_ind_pos_spline.s<-scale(long_df_merged$post_ind_pos_spline)
long_df_merged$post_ind_neg_spline.s<-scale(long_df_merged$post_ind_neg_spline)
long_df_merged$post_PM_pos_spline.s<-scale(long_df_merged$post_PM_pos_spline)
long_df_merged$post_PM_neg_spline.s<-scale(long_df_merged$post_PM_neg_spline)

mod_splines<-lmer(PM_task_lenient_av_log~
                    post_ind_pos_spline+post_ind_neg_spline+
                    + agegroup.c+
                    post_PM_pos_spline+post_PM_neg_spline+
                    (1|participant), 
                  data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,],
                  control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

tab_model(mod_splines)

vif(mod_splines)


mod_splines.s<-lmer(PM_task_lenient_av_log~
                    post_ind_pos_spline.s+post_ind_neg_spline.s+
                    + agegroup.c+
                    post_PM_pos_spline.s+post_PM_neg_spline.s+
                    (1|participant), 
                  data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,],
                  control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

print(summary(mod_splines.s))
tab_model(mod_splines.s)

vif(mod_splines)

#------------------------------------------------------------------------------#
# Alternative to splines
dat <- long_df_merged %>%
  mutate(side_aftIND = factor(if_else(valence_aftIND_min_bef > 0, "negative_side_aftIND", "positive_side_aftIND")),
         side_aftPM = factor(if_else(aft_PM_minus_after_ind > 0, "negative_side_aftPM", "positive_side_aftPM"))
         )

mod_side<-lmer(PM_task_lenient_av_log~
                 side_aftIND*valence_aftIND_min_bef
                    + agegroup.c+
                    (1|participant), 
                  data = dat[ (is.na(dat$MOCA)|dat$MOCA>=26) ,],
                  control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(mod_side)
emmeans::emtrends(mod_side, ~ side_aftIND, var = c("valence_aftIND_min_bef"))  # slope on each side

emtrends(reg, specs = ~1, var = "valence_aftIND_min_bef", at = list(valence = seq(0.05, max(long_df_merged$valence_aftIND_min_bef, na.rm=TRUE), length.out=5)))


dat <- long_df_merged %>%
  mutate(
    val_posIND = pmax(0, valence_aftIND_min_bef),   # captures the >0 (more positve) side
    val_negIND = pmax(0, -valence_aftIND_min_bef),   # captures the <0 (more negative) side, as positive magnitude
    val_posPM = pmax(0, aft_PM_minus_after_ind),   # captures the >0 (more positve) side
    val_negPM = pmax(0, -aft_PM_minus_after_ind)   # captures the <0 (more negative) side, as positive magnitude
      )



mod_side<-lmer(PM_task_lenient_av_log~
                 val_posIND+val_negIND+val_posPM+val_negPM+agegroup.c+
              
                 (1|participant), 
               data = dat[ (is.na(dat$MOCA)|dat$MOCA>=26) ,],
               control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(mod_side)

anova(mod_side)
vif(mod_side)
