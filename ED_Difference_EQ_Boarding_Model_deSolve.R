# Example code supplied for illustrative purposes only. This version uses
# deSolve and only simulates for 30 days (can be adjusted) with no intervention.
# 2019-2020 Edward G. Brown MS, Patricia K. Howard PhD RN, Daniel Moore MD
# For more information please read the paper SIMULATING EMERGENCY DEPARTMENT BOARDING USING A DIFFERENCE EQUATION 
# at https://doi.org/10.1101/2020.03.19.20039040


library(deSolve)

boarding <- function(t, state, parameters) {
  with(as.list(c(state)), {
    # start boarding, b this is db below [57]
    # stop boarding, c this is dc below [70]
    # (a + b) - c, day balance and also boarders at start of day, da below [12]
    
    db <-  (22.44721952 + (0.179980129 * (sample(200:300,1, replace=TRUE) ))) # start boarding
    dc <-  (32.5802325885976 + (0.492633658985415 * b)) # stop boarding
    da <-  ifelse ( ((a + b) - c) < 0, 0, ((a + b) - c) ) # day balance
    list(c(db, dc, da))
  })
}

# initial state and control for number and interval of iterations
state      <- c(b = 57, c = 70, a = 12)
times      <- seq(0, 30, by = 1)

out <- ode(y = state, times = times, func = boarding, parms = NULL, method="iteration")

matplot.deSolve(out) #generates a plot

# uncomment to display the day the day balance.
# out[,4]