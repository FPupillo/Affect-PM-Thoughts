#------------------------------------------------------------------------------#
# analysis of valence
#------------------------------------------------------------------------------#

# Group data by Condition, calculate mean and SD
summary_table_valence <-   long_df_valence_split %>%
  group_by(valence_measure, cond, agegroup) %>%
  summarize(
    Mean = mean(valence_value, na.rm = TRUE),
    SD = sd(valence_value, na.rm = TRUE),
    .groups = 'drop'  # To avoid message about grouping
  )


summary_table_valence


# analyze# analyzelong_df_baseline
long_df_valence_split$agegroup.c<-ifelse(long_df_valence_split$agegroup=="YA", -0.5, 0.5)

mod_valence<-lmer(valence_value~agegroup*valence_measure*cond +(1|participant), 
                  data = long_df_valence_split)


summary(mod_valence)
tab_model(mod_valence)
anova(mod_valence )

eta_squared(mod_valence, partial=T,alternative = "two.sided" )

emm_mean<-emmeans(mod_valence, ~ valence_measure*agegroup | cond )

# 2) Omnibus tests within each valence_measure (does cond matter? any agxcond interaction?)
joint_tests(emm_mean, by = "valence_measure")


# 3) Pairwise contrasts of cond within each agegroup and measure (your main request)
pairs(emm_mean,
      by     = c( "cond", "valence_measure"),
      adjust = "bonferroni")


# now break down the valence by condition interaction
emm_mean2<-emmeans(mod_valence, ~ cond | valence_measure )


pairs(emm_mean2,
      
      adjust = "bonferroni")
print(
emmip(mod_valence, agegroup ~ cond | valence_measure, CIs = TRUE) +
  theme_minimal() + labs(y = "Estimated valence", x = "Condition", color = "Age group")+
  # scale_color_manual(values = c(YA = "darkorange", OA = "darkgreen")) +
  scale_color_manual(values = okabe_ito,
                     labels = c(YA = "YA", OA = "OA")) +
  geom_hline(aes(yintercept = 0), color = "black")  +
  
  #scale_fill_manual(values = c(YA = "darkorange", OA = "darkgreen")) +  # CI ribbons
  scale_fill_manual(values = okabe_ito,
                    labels = c(YA = "YA", OA = "OA")) +  # CI ribbons
  
  theme_classic()+params+
  facet_wrap(
    ~ valence_measure,
    labeller = lab_valence
  )
)

ggsave("Write_up/figures/valence_all_contrasts.png", width = 12, height = 6, dpi = 300)
