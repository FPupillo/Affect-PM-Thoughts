logit_transform<-function(df, var, n ){
  #----------------------------------------------------------------------------#
  # Function that log-transform the data
  # input:  df - dataset in which the variable is
  #         var - variable to be log transformed
  #         n = sample size
  #         
  # output: veriable log-transformed with the same name 
  #         and the "_log" at the end
  #----------------------------------------------------------------------------#
  
  # first, prepare for logit by adding or subtracting a costant
  
  # initialize the variable in the dataset
  df[[paste0(var, "_rev")]]<-NA

  for (i in 1:nrow(df)){
    if(!is.na( df[[var]][i] == 0)){
      if (df[[var]][i] == 0){

        # add a costant if zero
        df[[paste0(var, "_rev")]][i]<- 1/ (2*n)

        # subtract a constant if 1
      }else if (df[[var]][i] == 1){

        df[[paste0(var, "_rev")]][i]<- 1 - 1/ (2*n)

      } else {

        df[[paste0(var, "_rev")]][i]<-df[[var]][i]

      }

    }
  }
  
  #DataN3<-prepare_for_logit(DataN3, "PM_ACC_mean", 0.05)
  
  df[[paste0(var, "_log")]]<-log( (df[[paste0(var, "_rev")]]) /(1- (df[[paste0(var, "_rev")]])))
  
  return(df)
  
}