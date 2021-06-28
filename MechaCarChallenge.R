### linear regression for mpg

# loading dplyr package
library(dplyr)

# import and reading MechaCar_mpg.csv file as a df
mecha_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(mecha_df)

# perform linear regression, passing in all six variables
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_df)

# determining pvalue and r-squared values for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mecha_df))


### summary statistics for suspension coils

#import and reading Suspension_Coil.csv as a table
suspension_table <- read.csv(file='suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# create total_summary off of PSI column
total_summary <- suspension_table %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# create lot_summary using group_by
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

### T-Tests on suspension coils

# use t.test() to determine if the PSI across all manufacturing lots is statistically different from the population mean
t.test(suspension_table$PSI,mu=1500)

# use t.test to determine if Lot1 is statistically different from the mean
t.test(subset(suspension_table, Manufacturing_Lot == 'Lot1')$PSI, mu = 1500)

# use t.test to determine if Lot2 is statistically different from the mean
t.test(subset(suspension_table, Manufacturing_Lot == 'Lot2')$PSI, mu = 1500)

# use t.test to determine if Lot3 is statistically different from the mean
t.test(subset(suspension_table, Manufacturing_Lot == 'Lot3')$PSI, mu = 1500)