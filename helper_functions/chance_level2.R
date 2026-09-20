# calculate chance level
chance_level2<-function(N1, N2, n_iterations, prob){
  #----------------------------------------------------------------------------#
  # this function returns the cutoff value below which the performance on the 
  # ongoing task can be considered at chance, by using permutation test
  #     INPUT: N1 - numbers of hits (similar to 2 items before)
  #            N2 - number of no hit (non similar)
  #            n_iterations: number of iteration
  #            prob - probability
  #     Output: chance level
  #----------------------------------------------------------------------------#
  
  # How many trials in total?
  n_trials = N1 + N2
  
  # Get trial sequence
  trial_seq <- c(rep(1, N1), rep(0, N2))
  
  trial_seq<-sample(c(0, 1), n_trials, prob = c(0.66,0.33), replace = T)
  
  
  # Initialize d' dataframe
  SDT_out <- data.frame(participant=c(1:n_iterations),
                        accuracy=rep(NA,n_iterations))
  
  # I have not found a more elegant way, so I'm looping through n_iterations to get the indeces.
  c=1
  for(cSub in 1:n_iterations){
    
    # Create random distribution
    sim_responses <- sample(c(0, 1), n_trials, prob = c(0.5, 0.5), replace = T)
    
    # Get numbers for each response type
    accuracy <- sum((trial_seq==1 & sim_responses==1) |
                      (trial_seq==0 & sim_responses==0) )/n_trials
    
    
    # Store dprime
    SDT_out$accuracy[c] <- accuracy
    c=c+1
  }
  
  
  # Sort
  sorted_a <- SDT_out %>% 
    arrange(SDT_out$accuracy)
  
  # get the threshold
  thres <- sorted_a$accuracy[n_iterations * prob]
  
  # Print critical value
  print(paste("Critical value for chance performance:", thres))
  
  return(thres)
  
}
