extract_rt <- function(x) {
  # Replace brackets and convert to numeric
  x_clean <- gsub("\\[|\\]", "", x)      # Remove brackets
  x_num <- as.numeric(x_clean)           # Convert to numeric (NAs for empty or invalid)
  
  # get rid of Nas
  x_num<-x_num[complete.cases(x_num)]
  
  return(x_num)
}