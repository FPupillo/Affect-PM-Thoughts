#------------------------------------------------------------------------------#
# Reshape Valence data
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# reshape the dataframe to have valence and arousal repeated
variables_to_keep<-c("participant" ,"cond" , "order", "MOCA" ,                
                     "c_tcaq" ,"tcaq_g", "agegroup" ,"health", 
                     "OT_task_only" ,  "DS" , "Mill_Hill", "education_y",
                     "OT_task_PM" , "PM_cost", "PM_task_av" , 
                     "PM_task_lenient_av","recog_task_av"
                    )

variables_to_reshape_val<-c("baseline_valence", "valence_after_ind", 
                            "valence_after_PM"  )

# for  the valence with baseline subtracted
variables_to_reshape_val_diff<-
  c("valence_bef_after_ind","valence_bef_after_PM" )
variables_to_reshape_ar<- 
  c("baseline_arousal","arousal_after_ind" ,"arousal_after_PM" )

long_df_valence <- melt(df_wide, id.vars = variables_to_keep,
                        measure.vars = variables_to_reshape_val)

# check
check_1<- long_df_valence %>%
  group_by(participant) %>%
  tally()


names(long_df_valence)[which(names(long_df_valence)=="variable")]<-
  "valence_measure"

names(long_df_valence)[which(names(long_df_valence)=="value")]<-
  "valence"

# with the valence - difference score
long_df_valence_diff <- melt(df_wide, id.vars = variables_to_keep,
                             measure.vars = variables_to_reshape_val_diff)

check_2<- long_df_valence_diff %>%
  group_by(participant) %>%
  tally()

names(long_df_valence_diff)[which(names(long_df_valence_diff)=="variable")]<-
  "valence_diff_measure"

names(long_df_valence_diff)[which(names(long_df_valence_diff)=="value")]<-
  "valence_diff"

# with arousal
long_df_arousal<-melt(df_wide, id.vars = variables_to_keep,
                      measure.vars = variables_to_reshape_ar)

names(long_df_arousal)[which(names(long_df_arousal)=="variable")]<-
  "arousal_measure"

names(long_df_arousal)[which(names(long_df_arousal)=="value")]<-
  "arousal"

# merge the three variables
all_df<-merge(long_df_valence, 
              long_df_valence_diff[, c("participant", "valence_diff_measure", 
                                       "valence_diff")], by = "participant")

check_3<-all_df %>%
  group_by(participant) %>%
  tally()

all_df<-merge(long_df_valence, 
              long_df_arousal[, c("participant", "arousal_measure", "arousal")],
              by = "participant")


# write the df
write.csv(long_df_valence, "group_data/long_df_valence.csv", row.names = F)
write.csv(long_df_valence_diff, "group_data/long_df_valence_diff.csv", row.names = F)
#------------------------------------------------------------------------------#


