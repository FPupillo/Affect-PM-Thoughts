#------------------------------------------------------------------------------#
# analysis of valence - save the plot
#------------------------------------------------------------------------------#


valence_plot<-emmip(mod_valence, agegroup ~ cond | valence_measure, CIs = TRUE) +
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

ggsave(filename =  "Write_up/figures/valence_all_contrasts.png", 
       plot = valence_plot, width = 14, height = 8, dpi = 300)
