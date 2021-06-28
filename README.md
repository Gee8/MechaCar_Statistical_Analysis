# MechaCar_Statistical_Analysis

## Linear Regression to Predict MPG

<img width="400" alt="regressionSummary" src="Images\regressionSummary.png">

Shown above is the summary statistics for our linear regression model using all six variables.

- Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
    - The `vehicle_weight`, `spoilder_angle`, and `AWD` variables had the highest p-values and provided the most significant contributions.
- Is the slope of the linear model considered to be zero? Why or why not?
    - The slope of the linear model is not zero, since all the coefficients for each variable is non-zero. For example, the coefficient for `ground_clearance` is 3.55.
- Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
    - With a multiple R-squared value of 0.7149, we can confidently say that our linear model predicts the mpg of the MechaCar prototypes effectively.

## Summary Statistics on Suspension Coils

Shown below is the `total_summary` and `lot_summary`, respectively.

<img width="400" alt="total_summary" src="Images\totalSummary.png"><img width="400" alt="lot_summary" src="Images\lotSummary.png">

- The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
    - As shown in the `total_summary`, the overall variance is 62.3 PSI, which is within the allowed 100 PSI. However, when we look at our `lot_summary`, we see that Lot3 has a variance of 170.29 PSI, which is far above the 100 PSI limit.

## T-Tests on Suspension Coils

Shown below are the T-Test results for our `suspension_table`

<img width="400" alt="tTests" src="Images\tTests.png">

- We conducted four T-Tests on our `suspension_table`. We tested to see if our data is has statistically different means than a the population mean of 1500. For our first T-Test, we saught to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1500. With a p-value of 0.06, we can determine that there is no statistically significant difference between all of the manufacturing lots and the population mean. For our next three T-Tests, we compared each of the three lots to the population mean. Our p-values for lots 1,2 and 3 were 1, 0.6, and 0.04. From this, we can determine that there is only a statistically significant difference between the means of lot 3 and the population.

## Study Design: MechaCar vs Competition

- For an additional analysis, consumers would likely be interested on how they can percieve purchasing a MechaCar as an investment. For a lot of people, their vehicle is one of the most expensive things they own. We should gather information about the value of a MechaCar over time, and how much of its value it retains over time. For this, we need data for the price of a MechaCar and their competition over time. Using linear regression, we can find the slope of our data to get the cars depreciation per year. Using this, our null hypothesis is that the MechaCar will retain its value more than its competetors. We can accept or reject the null hypothesis by comparing the slopes retrieved for the linear regression of each the MechaCar and its competetors.


