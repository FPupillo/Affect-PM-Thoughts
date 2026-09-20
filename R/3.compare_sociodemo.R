#------------------------------------------------------------------------------#
# Compare measures betwen age groups
#------------------------------------------------------------------------------#

mod_health<-lm(health~agegroup, data = single_part)
summary(mod_health)

mod_educat<-lm(education_y~agegroup, data = single_part)
summary(mod_educat)

### mill hill
mill_hill<-lm(Mill_Hill~agegroup, data = single_part)
summary(mill_hill)

# digit symbol
digit_symbol<-lm(DS~agegroup, data = single_part)
summary(digit_symbol)

# create a contingency table to check whther the distribution of gender is equal
contingency_table_gender<-table(single_part[, c("agegroup", "gender")])

# perform chi square test on gender
chi_square_test <- chisq.test(contingency_table_gender)
chi_square_test
#------------------------------------------------------------------------------#
