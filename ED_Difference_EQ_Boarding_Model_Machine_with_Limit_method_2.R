# Example code supplied for illustrative purposes only. The regression equations below were built
# using 2 years of data from our level 1 trauma academic medical center. They will not work out of the
# box for your institution.
# 2019-2020 Edward G. Brown MS, Patricia K. Howard PhD RN, Daniel Moore MD
# For more information please read the paper SIMULATING EMERGENCY DEPARTMENT BOARDING USING A DIFFERENCE EQUATION 
# at https://doi.org/10.1101/2020.03.19.20039040

bfunc <- function() {

  days <- 30
  lower <- 200
  upper <- 300
  
  test_limit <- 290
  threshold <- 40
  
  xbar.rand <- rep(NA,days)
  xbar.star <- rep(NA,days)
  xbar.start <- rep(NA,days)
  xbar.stop <- rep(NA,days)
  xbar.forty <- rep(40,days)
  xbar.thirty <- rep(30,days)
  
  start_int <- 22.44721952
  start_slope <- 0.179980129
  stop_int <- 32.5802325885976
  stop_slope <- 0.492633658985415
  
  for (i in 1:days)
    xbar.rand[[i]] <-sample(lower:upper, 1)
    
  for (i in 1:days)
    xbar.start[[i]] <-ceiling(start_int + (xbar.rand[[i]] * start_slope))

    for (i in 1:days)
    xbar.stop[[i]] <- ceiling(stop_int +(xbar.start[[i]] * stop_slope))
  
# Why 12? In our observed data, the starting day balance of a day (selected at random) was 12. This starts our iteration.
# Patients boarding at Start of Day was 25, Patients start boarding during the day was 57, Patients stop boarding was 70.  
  xbar.star[[1]] = 12
  
  for (i in 1:days)

    xbar.star[[i+1]] = ifelse(ceiling((xbar.star[i] + xbar.start[[i]]) - xbar.stop[[i]]) < 0, 0,
                              
                              
                              ifelse(ceiling((xbar.star[i] + xbar.start[[i]]) - xbar.stop[[i]]) >= threshold,
                                     
                                     ifelse(ceiling((xbar.star[i] + (start_int + (test_limit * start_slope))   ) - (stop_int +((start_int + (test_limit * start_slope)) * stop_slope))     ) < 0,0
                                            , ceiling((xbar.star[i] + (start_int + (test_limit * start_slope))   ) - (stop_int +((start_int + (test_limit * start_slope)) * stop_slope))     ) )
                                     
                                     ,ceiling((xbar.star[i] + xbar.start[[i]]) - xbar.stop[[i]])
                              )
    )
  
    
    
        
over_threshold <- length(xbar.star[xbar.star >= threshold])/days

return(over_threshold)
} # end of function

reps <- 10000
bresults <- rep(NA,reps)

for (i in 1:reps)
  bresults[[i]] <- bfunc()

sd(bresults)
summary(bresults)

# percentile method for 95% CI
quantile(bresults, 0.025)
quantile(bresults, 0.975)

hist(bresults, col="lightgreen")
abline(v=quantile(bresults, 0.025),col="blue", lwd=2)
abline(v=quantile(bresults, 0.975),col="blue", lwd=2)
abline(v=median(bresults),col="red", lwd=2)

# For a non-parametric CI around the median please see this repository:
# https://github.com/edwardbrown/Median_Confidence_Interval
# The function will output the achieved CL.
# Note: Numerous ties (especially around the median) may lead to an exceedingly narrow CI.
# This will likely occur more/less depending on the function arguments used and number of bootstrap replications.
# Your results may vary.
