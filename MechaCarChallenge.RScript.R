#####################################
# Linear Regression to Predict MPG  #
#####################################

install.packages("tidyverse")
install.packages("dplyr")

# read open input file: 
mechacar_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

num_rows <- 1:nrow(mechacar_df)

# generate multiple linear regression model to determine the p-value 
# and r-squared value for the model
summary(lm( mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_df)) 

###############################################
# Create Visualizations for the Trip Analysis #
###############################################
# read open input file: 
suspend_df <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
library(tidyverse)

# get mean, median, variance and standard deviation (no grouping)
total_summary <- suspend_df %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep') #create summary table

# mean, median, variance and standard deviation (group by Manufacturing Lot)
lot_summary <- suspend_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep') #create summary table

################################
# T-Tests on Suspension Coils  #
################################

# given the population's mean is 1,500 PSI, we test to see if the PSI 
# is statistically different across all lots: 
PSI_all_lots <- t.test(suspend_df$PSI, mu=1500)

# given the population's mean is 1500 PSI, we test to see if the PSI 
# is statistically different FOR EACH lot: 
PSI_lot1 <- t.test(subset(suspend_df,Manufacturing_Lot=="Lot1")$PSI, mu=1500)
print(PSI_lot1)

PSI_lot2 <- t.test(subset(suspend_df,Manufacturing_Lot=="Lot2")$PSI, mu=1500)
print(PSI_lot2)

PSI_lot3 <- t.test(subset(suspend_df,Manufacturing_Lot=="Lot3")$PSI, mu=1500)
print(PSI_lot3)