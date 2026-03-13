# calculate chance level
chance_level<-function(N){
  #----------------------------------------------------------------------------#
  # this function returns the cutoff value below which the performance on the 
  # ongoing task can be considered at chance
  #     INPUT: N - number of trials
  #     Output: chance level
  #----------------------------------------------------------------------------#
  
  # Probability of success by chance
  p <- 0.66
  
  # Significance level
  alpha <- 0.05
  
  # Perform binomial test
  binom_test <- binom.test(sum(sample(c(0,1), N, replace=TRUE)), N, p = 0.5,
                           alternative = "two.sided", conf.level = 1 - alpha)
  
  # Get critical value
  critical_value <- binom_test$conf.int[1]
  
  # Print critical value
  print(paste("Critical value for chance performance:", critical_value))
  
  return(critical_value)
  
}
