#

# plot after PM
p1<-ggplot(long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], 
       aes( x=tcaq_g, y=  PM_task_lenient_av, colour = agegroup))+
  ylab("PM Performance")+
  xlab("General intrusive thoughts")+

  scale_fill_manual( values =  okabe_ito)+
  scale_color_manual(
    values = okabe_ito)+
  geom_smooth(method="lm",formula=y~x, se=T)+
  params+
  labs( color = "Age group"  )+ 
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  
  # specify that we want different colours for different participants
  # add the summary line with geom_smooth
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  params


# plot after PM
p2<-ggplot(long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], 
       aes( x=c_tcaq, y=PM_task_lenient_av, colour = agegroup))+
  geom_smooth(method="lm",formula=y~x, se=T)+
  ylab("PM Performance")+
  xlab("Intrusive Thoughts During Task")+

  scale_fill_manual( values =  okabe_ito)+
    scale_color_manual(
    values = okabe_ito)+
  params+
  labs( color = "Age group"  )+ 
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  
  # specify that we want different colours for different participants
  # add the summary line with geom_smooth
  theme_classic()+
  params


plot_thoughts<-ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom",
                  labels = c("a)", "b)"), 
                  font.label = list(size = 24, face = "bold"), 
                  label.x = -0.02, label.y = 0.98)

# combined the two
ggsave("Write_up/figures/task-related_general_intr_PM.png",plot_thoughts, width = 14, height = 8, dpi = 300)


# plot after PM
p1<-ggplot(long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], 
           aes( x=tcaq_g, y=  PM_task_lenient_av, colour = agegroup))+
  ylab("PM Performance")+
  xlab("General intrusive thoughts")+
  
  scale_fill_manual( values =  okabe_ito)+
  scale_color_manual(
    values = okabe_ito)+
  geom_smooth(method="lm",formula=y~x, se=T)+
  labs( color = "Age group"  )+ 

  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))


# plot after PM
p2<-ggplot(long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,], 
           aes( x=c_tcaq, y=PM_task_lenient_av, colour = agegroup))+
  geom_smooth(method="lm",formula=y~x, se=T)+
  ylab("PM Performance")+
  xlab("Intrusive Thoughts During Task")+
  
  scale_fill_manual( values =  okabe_ito)+
  scale_color_manual(
    values = okabe_ito)+
  labs( color = "Age group"  )+ 
  theme_classic()


ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom",
                                 labels = c("a)", "b)"), 
                                 font.label = list(size = 24, face = "bold"), 
                                 label.x = -0.02, label.y = 0.98)


long_df_merged_all_long$valence_diff_measure<-factor(long_df_merged_all_long$valence_diff_measure, 
                                                     levels =  c("post_induction", "post_task"))


# now th eeffect of task-related intrusive thoughts on PM
print(
ggplot(long_df_merged_all_long[ (is.na(long_df_merged_all_long$MOCA)|long_df_merged_all_long$MOCA>=26),], 
       aes( x=valence_change, y=c_tcaq, colour = agegroup))+
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  
  # add the summary line with geom_smooth
  geom_smooth(method="lm",formula=y~x, se=T)+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  labs( color = "Age Group"  )+ 
  facet_grid(.~valence_diff_measure, labeller = lab_valence)  +
  ylab("Task-related Intrusive Thoughts")+
  xlab("Valence")+
  scale_color_manual(
    values = okabe_ito)+
  scale_fill_manual( values =  okabe_ito)+
  theme(legend.position = "bottom")
  
)

plot_valence_thoughts<-ggplot(long_df_merged_all_long[ (is.na(long_df_merged_all_long$MOCA)|long_df_merged_all_long$MOCA>=26),], 
       aes( x=valence_change, y=c_tcaq, colour = agegroup))+
  # add the "smooth" line, which the regression method ('l,')
  # and trasparent (0.5)
  
  # add the summary line with geom_smooth
  geom_smooth(method="lm",formula=y~x, se=T)+
  theme(strip.text.x = element_text(size = 13))+
  theme_classic()+
  theme(panel.spacing = unit(1, "lines"))+
  labs( color = "Age Group"  )+ 
  facet_grid(.~valence_diff_measure, labeller = lab_valence)  +
  ylab("Task-related Intrusive Thoughts")+
  xlab("Valence")+
  scale_color_manual(
    values = okabe_ito)+
  scale_fill_manual( values =  okabe_ito)+
  theme(legend.position = "bottom")


ggsave("Write_up/figures/valence_Age_thoughts.png",plot_valence_thoughts,
       width = 14, height = 8, dpi = 300)


mod__valence_thougths<-lmer(c_tcaq~DS.c*agegroup.c+Mill_Hill.c+order_c+
                              valence_aftIND_min_bef.s*agegroup.c+ aft_PM_minus_after_ind.s*agegroup.c+
                              
                              (1|participant), 
                            data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,],
                            control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
print(
summary(mod__valence_thougths)
)
anova(mod__valence_thougths, type = 3, ddf = "Kenward-Roger")

eta_squared(mod__valence_thougths, partial = T, altenative = "two.sided")

#------------------------------------------------------------------------------#

# some analyses
#------------------------------------------------------------------------------#
# age difference in general intrusive thoughts
# summarize across parts
long_df_part<-long_df_merged %>%
  group_by(participant) %>%
  slice_sample(n = 1) %>%
  ungroup()

mod_thouhgts_gen<-lm(tcaq_g~agegroup.c, 
                     long_df_part[ (is.na(long_df_part$MOCA)|long_df_part$MOCA>=26) ,])

summary(mod_thouhgts_gen)

anova(mod_thouhgts_gen)

# summarize
long_df_part %>%
  group_by(agegroup) %>%
  dplyr::summarize(mean_tcaq_g = mean(tcaq_g, na.rm = T), 
                   sd_tcaq_g = (sd(tcaq_g, na.rm=T))) %>%
  mutate(across(c(mean_tcaq_g, sd_tcaq_g), ~ round(.x, 2)))

mean(long_df_part$tcaq_g[long_df_part$agegroup=="YA"], na.rm=T)
sd(long_df_part$tcaq_g[long_df_part$agegroup=="YA"], na.rm=T)

mean(long_df_part$tcaq_g[long_df_part$agegroup=="OA"], na.rm=T)
sd(long_df_part$tcaq_g[long_df_part$agegroup=="OA"], na.rm=T)

# is this significant?fsdf
mod_thouhgts_gen_task<-lmer(c_tcaq~tcaq_g*agegroup.c+
                              (1|participant), 
                            long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

summary(mod_thouhgts_gen_task)

anova(mod_thouhgts_gen_task)

# are intrusive thoughts predicted by mood canges and age?


# is this significant?fsdf
mod_thouhgts_task<-lmer(c_tcaq~valence_aftIND_min_bef*agegroup.c+aft_PM_minus_after_ind*agegroup.c+
                          (1|participant), 
                        long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

summary(mod_thouhgts_task)
tab_model(mod_thouhgts_task)


anova(mod_thouhgts_task)

eta_squared(mod_thouhgts_task, partial = T, altenative = "two.sided")
#------------------------------------------------------------------------------#
# did generatl and task-specific thoughts were influenced by cognitive ability?
mod_thouhgts_gen_DS<-lm(tcaq_g~agegroup.c*DS, 
                     long_df_part[ (is.na(long_df_part$MOCA)|long_df_part$MOCA>=26) ,])

summary(mod_thouhgts_gen_DS)
Anova(mod_thouhgts_gen_DS, type=2)

# center DS
long_df_merged$DS.c<-scale(long_df_merged$DS)
mod_thouhgts_task_DS<-lmer(c_tcaq~DS.c*agegroup+
                          (1|participant), 
                        long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

summary(mod_thouhgts_task_DS)
vif(mod_thouhgts_task_DS)
tab_model(mod_thouhgts_task_DS)

anova(mod_thouhgts_task_DS, type=3)


interact_plot(mod_thouhgts_task_DS,
           
              pred = DS.c,
      
              modx = agegroup,
       
              interval = TRUE,
              legend.main = "Age group"
              )+
  labs(x = "Fluid intelligence (centered)", 
       y = " Intrusive thoughts during task")+
  theme_classic()+
  labs(colour = "Age group")

eta_squared(mod_thouhgts_task_DS, partial = T, altenative = "two.sided")
#------------------------------------------------------------------------------#

# try a three way interaction with age, tcaq_g, and tcaq_c

# first, center tcaq_g
long_df_merged$tcaq_g.s<-scale(long_df_merged$tcaq_g)

# and c_tcaq
long_df_merged$c_tcaq.s<-scale(long_df_merged$c_tcaq)


mod_thouhgts_gen_PM_int<-lmer(PM_task_lenient_av_log~tcaq_g.s*agegroup.c*c_tcaq.s+DS*agegroup.c+
                            (1|participant), 
                          long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,])

tab_model(mod_thouhgts_gen_PM_int)

print(
  summary(mod_thouhgts_gen_PM)
)
anova(mod_thouhgts_gen_PM, type =3)

eta_squared(mod_thouhgts_gen_PM, partial = T, altenative = "two.sided")

# what if we control for DS?

mod_thouhgts_gen_DS<-lm(tcaq_g~DS*agegroup.c, 
                     long_df_part[ (is.na(long_df_part$MOCA)|long_df_part$MOCA>=26) ,])

summary(mod_thouhgts_gen_DS)


Anova(mod_thouhgts_gen_DS, type = 3)

mod__valence_DS<-lmer(valence_aftIND_min_bef.s~cond*DS+
                              (1|participant), 
                            data = long_df_merged[ (is.na(long_df_merged$MOCA)|long_df_merged$MOCA>=26) ,],
                            control =  lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mod__valence_DS)

anova(mod__valence_DS)
