#------------------------------------------------------------------------------#
# extract the files with the valence and PM data and save them to a file
#
#------------------------------------------------------------------------------#

# get the spss_data 
spss_data<-read.csv("PM final data.csv"  )


# get the file with the order of the conditions
cond_young<-read_excel("Age, Mood and PM Project Participant Code (Young Adults).xlsx")
cond_old<-read_excel("Age, Mood and PM Project Participant Code (Older Adults).xlsx")

# in both files, assign the subject where there are NAs
files<-c("cond_young", "cond_old")

# initialize a file for storing the conditions
cond_file<-vector()
for (file in files){
  
  # get the file
  c_file<-get(file)
  
  # get the header
  if (file == "cond_young"){
    agegr<-"YA"
    head_row<-which(c_file$`Age, Mood and PM Project Participant Code (Young Adults)`=="Subject")
  } else {
    agegr<-"OA"
    head_row<-which(c_file$`Age, Mood and PM Project Participant Code (Older Adults)`=="Subject")
  }
  
  names(c_file)<-as.character(c_file[head_row,])
  
  # delete the previous ones
  c_file<-c_file[((head_row+1):nrow(c_file)),1:5]
  
  # how many participants
  parts<-unique(c_file$Subject)
  # delete NA
  parts<-parts[!is.na(parts)]
  
  # loop through rows
  for (n in 1:nrow(c_file)){
    
    #get the row
    if(!is.na(c_file$Subject[n])){ # if this is not a missing case
      if (agegr=="OA"){ # we need to add 100 to OA
        c_file$Subject[n]<-as.numeric(c_file$Subject[n]) +100
        c_subject<-as.numeric(c_file$Subject[n])+100
      } 
      c_file$Subject[n]<-as.numeric(c_file$Subject[n])
      c_subject<-c_file$Subject[n]
    }else{ # if this is a missing case, assign the previous subject
      if (agegr=="OA"){ # we need to add 100 to OA
        c_file$Subject[n]<-as.numeric(c_subject)+100
      }
      c_file$Subject[n]<-c_subject
    }
    
    # if the agegroup is OA, we need to add 100 to participant number
  }
  
  c_file$agegr<-agegr
  
  cond_file<-rbind(cond_file, c_file)
  
  
}

# save this 
write.csv(cond_file, "group_data/conditions.csv")

#  okaay, now look again across particiopants and get the conditions
part_cond<-vector()

parts<-unique(cond_file$Subject)

for (p in parts){
  
  # subset the df
  c_df<-cond_file[cond_file$Subject==as.numeric(p),]
  
  c_order<-c_df$`Mood Induced`
  
  # get the conditions
  part_cond<-rbind(part_cond, c(as.numeric(p), c_order))
  
}

# convert as a dataframe
part_cond<-as.data.frame(part_cond)

# assign names
names(part_cond)<-c("participant", "cond1", "cond2", "cond3")

# Get the data
#Import the data from the folders

# Name of the conditions
conditions<-c("Negative", "Positive", "Neutral")

# create a list
df_long<-vector()
df_wide<-vector()

# initialize a df for the tcaq
# count for participant -  so that we extract part info (HADS, ERQ) only once
p_count<-0

for (cond in conditions){ # loop across conditions
  
  # get the current path to the raw data
  c_path_raw<-paste0("raw_data/", cond)
  
  # get the current participant
  curr_participants<-list.files(c_path_raw)
  
  for (n in 1:length(curr_participants)){ # loop across participants within 
    
    # within a condition
    file_name<-paste0(c_path_raw,"/", curr_participants[n])
    
    c_participant<-curr_participants[n]
    
    curr_files<-read.csv(file_name)
    
    # select the variables of interest
    VoI<- c("word", "corrAnsw", "trial_type","cues", "slider_mood.response",
            "slider_arousal.response", "key_resp_ongoing.corr",
            "key_resp_ongoing.rt",  "key_resp_PM.rt",
            "key_resp_PM.keys","key_resp_PM.corr","key_resp_recognition.corr" ,
            "participant" ,"session" )
    
    curr_files<-curr_files[, VoI]
    
    # save the files in the clean_files folder
    # participant
    
    #participant<-curr_files$participant[1]
    
    participant<-as.numeric(strsplit(c_participant, "_")[[1]][1])
    
    
    # generate the name first
    #name<-paste0(curr_files$participant[1], "_", cond, ".csv")
    
    name<-paste0(participant, "_", cond, ".csv")
    
    write.csv(curr_files, paste0("clean_data/", name), row.names = F)
    
    # take the baseline mood
    # we are averaging the first two SAMs
    curr_files$slider_mood.response<-as.numeric(curr_files$slider_mood.response)  
    
    baseline_valence<-
      mean(curr_files$slider_mood.response[!is.na(curr_files$slider_mood.response)][1:2])
    
    curr_files$slider_arousal.response<-as.numeric(curr_files$slider_arousal.response)
    
    # same for arousal
    baseline_arousal<-
      mean(curr_files$slider_arousal.response[!is.na(curr_files$slider_arousal.response)][1:2])
    
    # the SAM after the mood induction: it is the third
    valence_after_ind<-
      curr_files$slider_mood.response[!is.na(curr_files$slider_mood.response)][3]
    
    # arousal after the mood induction
    arousal_after_ind<-
      curr_files$slider_arousal.response[!is.na(curr_files$slider_arousal.response)][3]
    
    # the SAM after PM: it is the fourth
    valence_after_PM<-curr_files$slider_mood.response[!is.na(curr_files$slider_mood.response)][4]
    
    # arousal after PM
    arousal_after_PM<-curr_files$slider_arousal.response[!is.na(curr_files$slider_arousal.response)][4]
    
    #--------------------------------------------------------------------------#
    # Reverse the valence
    baseline_valence<-101-baseline_valence
    valence_after_ind<-101-valence_after_ind
    valence_after_PM<-101-valence_after_PM
    
    #--------------------------------------------------------------------------#
    
    # create bef_after_induction - difference score
    valence_bef_after_ind<-valence_after_ind-baseline_valence
    arousal_bef_after_ind<-arousal_after_ind-baseline_arousal
    
    # create bef_after_PM
    valence_bef_after_PM<-valence_after_PM-baseline_valence
    arousal_bef_after_PM<-arousal_after_PM-baseline_arousal
    
    # ongloing task only
    #ongoing_task_only<-mean(curr_files$key_resp_ongoing.corr, na.rm=T)
    
    # OT task only
    OT_task_only<-mean(curr_files$key_resp_ongoing.corr, na.rm=T)
    
    # OT task PM
    OT_task_PM<-mean(curr_files$key_resp_PM.corr[curr_files$trial_type!="PM cue"], na.rm=T)
    
    PM_cost<-OT_task_PM-OT_task_only
    
    # now reaction times
    OT_task_only_rt<-mean(extract_rt(curr_files$key_resp_ongoing.rt), na.rm=T)
    
    # now reactin times for the OT under PM
    OT_task_PM_rt<-mean(extract_rt(curr_files$key_resp_PM.rt[curr_files$trial_type!= "PM cue" ]))
    
    PM_cost_rt<-OT_task_PM_rt-OT_task_only_rt
    
    # PM task:averaged
    PM_task_av<-mean(curr_files$key_resp_PM.corr[curr_files$trial_type=="PM cue"])
    
    # recognition, average
    recog_task_av<-mean(curr_files$key_resp_recognition.corr, na.rm=T)
    
    # now get the lenient PM performance
    curr_files$PM.lenient<-NA
    
    for (i in 1:nrow(curr_files)){
      if (curr_files$trial_type[i]=="PM cue"){
        
        # get the key pressed - get rid off the square brackets
        key_pressed<- substr(curr_files$key_resp_PM.keys[i], 2,
                             nchar(curr_files$key_resp_PM.keys[i])-1)
        
        if (length(grep('h', key_pressed))>0){ # if they pressed an h 
          
          curr_files$PM.lenient[i]<-1
          
        } else {
          
          curr_files$PM.lenient[i]<-0
          
        }
        
      }
    }
    
    PM_task_lenient_av<-mean(curr_files$PM.lenient, na.rm = T)
    
    # create the condition variable
    curr_files$condition<-cond
    
    # get age group
    agegroup<-as.numeric(spss_data$AgeGroup[spss_data$Nb==participant])
    
    # get the MOCA
    MOCA<-spss_data$MOCA[spss_data$Nb==participant]
    
    # get thought control ability questionnaire (tcaq)
    # we have three questionnaires, one for each condition
    # we also need to reverse some of the  items' scores
    
    reverse_score<-function(scoring, n){
      #--------------------------------------#
      # function which reverse the scoring
      # of an item
      #  INPUT: scoring - scoring on that item
      #         n - number of levels of the scales
      #  OUTPUT: the score reversed
      #--------------------------------------#
      
      reversed<-(n+1)-scoring
      return(reversed)
      
    }
    
    # which ones are the items to reverse?
    # there are two thoughts questionnaires - one general and one after the tasks.
    # the reversal items are also different
    to_reverse_general<-c(4,7,9, 11, 14, 20, 24, 25)
    
    to_reverse_task<-c(3,6,8,9,12,16,19)
    
    # for the general
    tcaq_g_names<- grep(pattern = paste0("TCAQ_G"), names(spss_data))
    
    # get those
    tcaq_g<-spss_data[spss_data$Nb==participant, tcaq_g_names]
    
    # reverse
    for (name in 1:length(tcaq_g)){
      
      if (any(name==to_reverse_general)){
        
        tcaq_g[name]<-as.numeric(reverse_score(tcaq_g[name], 5))
        
      }
      
    }
    
    tcaq_g<-sum(tcaq_g)
    
    # for the task one,  run through all the items three times
    for (t in 1:3){
      
      # subset the tcaq questionnaires
      tcaq_names<- grep(pattern = paste0("TCAQ_T",t), names(spss_data))
      
      # get those
      tcaq<-spss_data[spss_data$Nb==participant, tcaq_names]
      for (name in 1:length(tcaq)){
        
        if (any(name==to_reverse_task)){
          
          tcaq[name]<-as.numeric(reverse_score(tcaq[name], 5))
          
        }
        
      }
      
      c_cond<-part_cond[part_cond$participant==participant, paste0("cond", t)]
      
      # assign the sum to the correct condition
      assign(paste0("tcaq_", c_cond), sum(tcaq))
      
      # assign the all tcaq
      assign(paste0("tcaq.all_", c_cond), tcaq)
      
    }
    
    # the current tcaq is the one of the current condition
    if(cond == "Neutral"){
      c_tcaq<-get(paste0("tcaq_", "Neu"))
    } else {    c_tcaq<-get(paste0("tcaq_", cond))}
    

    #--------------------------------------------------------------------------#
    # get health and years of education
    health<-spss_data$Health[spss_data$Nb == participant]
    
    education_y<-spss_data$EduY[spss_data$Nb == participant]
    
    age<-spss_data$Age[spss_data$Nb == participant]
    
    gender<-spss_data$Gender[spss_data$Nb==participant]

    # get Digit Symbol and Mill_Hill
    # get the digit symbol
    DS<-sum(spss_data$FluidI[spss_data$Nb==participant])
    
    # get Mill Hill
    Mill_Hill<-sum(spss_data$CrystallizedI[spss_data$Nb==participant])
    
    
    # create the wide dataset
    curr_wide<-as.data.frame(cbind(participant, age, cond,  MOCA, health, education_y,gender, 
                                   tcaq_g, c_tcaq, agegroup, 
                                   DS, Mill_Hill, health, education_y,gender, 
                                   OT_task_only,  OT_task_PM, 
                                   OT_task_only_rt,  OT_task_PM_rt,
                                   PM_cost, PM_cost_rt,
                                   PM_task_av, PM_task_lenient_av,
                                   recog_task_av,
                                   baseline_valence, baseline_arousal, 
                                   valence_after_ind, arousal_after_ind,
                                   valence_after_PM, arousal_after_PM, valence_bef_after_ind, 
                                   arousal_bef_after_ind, valence_bef_after_PM, arousal_bef_after_PM
                                  ))
    
    # add the tcaq
    # get all the tcaq variables
    # get the t
    # get the currenct tcaq_all
    if(cond == "Neutral"){
      c_tcaq.all<-get(paste0("tcaq.all_", "Neu"))
    } else {    c_tcaq.all<-get(paste0("tcaq.all_", cond))}
    
    
    t<-substr(names(c_tcaq.all)[1], 7,7)
    
    for (var in names(c_tcaq.all)){
      
      # strip the tcaq names from the time point and assign the affect condition
      new_name <- gsub(paste0("TCAQ_T", t,"\\."), "TCAQ_", var)
      
      curr_wide[[new_name]]<-c_tcaq.all[[var]]
      
    }    
    
    df_wide<-rbind(df_wide, curr_wide )
    
    # create the PM long
    PM_long<-curr_files[curr_files$trial_type=="PM cue",]
    recog_long<-curr_files[!is.na(curr_files$key_resp_recognition.corr),]
    
    # valence
    PM_long$baseline_valence<-baseline_valence
    PM_long$baseline_arousa<-baseline_arousal
    PM_long$valence_after_ind<-valence_after_ind
    PM_long$arousal_after_ind<-arousal_after_ind
    PM_long$valence_after_PM<-valence_after_PM
    PM_long$arousal_after_PM<-arousal_after_PM
    PM_long$valence_bef_after_ind<-valence_bef_after_ind
    PM_long$arousal_bef_after_ind<-arousal_bef_after_ind
    PM_long$arousal_bef_after_PM<-arousal_bef_after_PM
    PM_long$valence_bef_after_PM<-valence_bef_after_PM
    
    
    # create a variable called recognition
    PM_long$recognition<-NA
    for (n in 1:nrow(PM_long)){
      
      # get the word
      word<-PM_long$word[n]
      PM_long$recognition[n]<-(curr_files$key_resp_recognition.corr[curr_files$word==word])[3]
      
    }
    
    PM_long$agegroup<-agegroup
    
    PM_long$cond<-cond
    
    PM_long$OT_task_only<-OT_task_only
    PM_long$OT_task_PM<-OT_task_PM
    PM_long$OT_task_only_rt<-OT_task_only_rt
    PM_long$OT_task_PM_rt<-OT_task_PM_rt
    PM_long$PM_cost<-PM_cost
    PM_long$PM_cost<-PM_cost_rt
    PM_long$c_tcaq<-c_tcaq
    PM_long$tcaq_g<-tcaq_g
    PM_long$MOCA<-MOCA
 
    # now we can bind them
    df_long<-rbind(df_long, PM_long)
    
  }
  
}

# check that we have the same observation by participant
obs_by_part<-df_wide %>%
  group_by(participant)%>%
  tally()

# this function showed that if we assign the number 103 to the file 103
# in the negative condition (which had 100 as participant variable)
# we end up with an additional condition for participant 103. 
# that participant is probably 101

# write them
write.csv(df_wide, "group_data/df_wide.csv", row.names = F)

write.csv(df_long, "group_data/df_long.csv", row.names = F)
