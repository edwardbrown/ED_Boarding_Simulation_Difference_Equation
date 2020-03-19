# Example code supplied for illustrative purposes only. The regression equations below were built
# using 2 years of data from our level 1 trauma academic medical center. They will not work out of the
# box for your institution.
# 2019-2020 Edward G. Brown MS, Patricia K. Howard PhD RN, Daniel Moore MD

bfunc <- function() {

  days <- 30
  lower <- 200
  upper <- 300
  
  threshold <- 40

  xbar.rand <- rep(NA,days)
  xbar.star <- rep(NA,days)
  xbar.start <- rep(NA,days)
  xbar.stop <- rep(NA,days)

  start_int <- 22.44721952
  start_slope <- 0.179980129
  stop_int <- 32.5802325885976
  stop_slope <- 0.492633658985415
  
    
for (i in 1:days)
  xbar.rand[[i]] <-sample(lower:upper, 1)

for (i in 1:days)
  xbar.start[[i]] <- start_int + (xbar.rand[[i]] * start_slope)


for (i in 1:days)
  xbar.stop[[i]] <- stop_int +(xbar.start[[i]] * stop_slope)


xbar.star[[1]] = 12

for (i in 1:days)
  xbar.star[[i+1]] = ifelse(ceiling((xbar.star[i] + xbar.start[[i]]) - xbar.stop[[i]]) < 0, 0, ceiling((xbar.star[i] + xbar.start[[i]]) - xbar.stop[[i]]) )
 

over_threshold <- length(xbar.star[xbar.star >= threshold])/days

return(over_threshold)
} # end of function


reps <- 10000
bresults <- rep(NA,reps)


for (i in 1:reps)
  bresults[[i]] <- bfunc()



sd(bresults)
summary(bresults)
hist(bresults)
quantile(bresults, 0.025)
quantile(bresults, 0.975)


