#Function to run simulation
run_simulation <- function() {
  wishes <- 0
  last_five_star <- 0
  is_guaranteed_featured <- FALSE
  
  while(TRUE) {
    wishes <- wishes + 1
    last_five_star <- last_five_star + 1
    
    if (last_five_star >= 90 || runif(1) < 0.006) { 
      if (is_guaranteed_featured || runif(1) < 0.5) {
        #Featured 5-star
        return(wishes)
      } else {
        #Standard Banner 5-star
        is_guaranteed_featured <- TRUE
      }
      last_five_star <- 0 #Reset the pity counter for a 5-star
    }
  }
}

#Running the simulation multiple times
set.seed(111)
num_simulations <- 10000
simulation_results <- replicate(num_simulations, run_simulation())

#Analyzing the results
mean_wishes <- mean(simulation_results)
hist(simulation_results, breaks = 50, main = "Histogram of Wishes for 
     Featured 5-Star Character")

plot(ecdf(simulation_results), main = 'Cumulative Distribution Function', 
     xlab = 'Number of Wishes', ylab = 'Cumulative Probability', 
     verticals = TRUE, do.points = FALSE, col = 'red')



