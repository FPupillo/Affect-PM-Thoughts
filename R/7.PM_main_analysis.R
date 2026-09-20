#------------------------------------------------------------------------------#
# PM main analysis
#------------------------------------------------------------------------------#

# use the logit
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
    theme(legend.position = "bottom")
  
)

PM_valence_plot<-
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
  


ggsave( "Write_up/figures/predicted_valence_aftIND_aftPM_PM.png", PM_valence_plot, 
       width = 12, height = 8, dpi = 300)



#