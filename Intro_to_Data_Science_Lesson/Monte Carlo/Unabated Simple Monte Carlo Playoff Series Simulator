# Function to convert probability to American odds
prob_to_american_odds <- function(prob) {
  if (prob >= 0.5) {
    # Favorite odds
    return(-round((prob / (1 - prob)) * 100))
  } else {
    # Underdog odds
    return(round(((1 - prob) / prob) * 100))
  }
}

monte_carlo_best_of_7 <- function() {
  # Prompts for user input
  favorite <- readline(prompt = "Enter the series favorite team: ")
  underdog <- readline(prompt = "Enter the series underdog team: ")
  favorite_home_prob <- as.numeric(readline(prompt = paste0("Enter ", favorite, "'s home win probability as a decimal: ")))
  underdog_home_prob <- as.numeric(readline(prompt = paste0("Enter ", underdog, "'s home win probability as a decimal: ")))
  favorite_wins <- as.integer(readline(prompt = paste0("Enter the number of games ", favorite, " has won to date: ")))
  underdog_wins <- as.integer(readline(prompt = paste0("Enter the number of games ", underdog, " has won to date: ")))
  
  # Series format (2-2-1-1-1) starting with the favorite's home court
  home_court_order <- c(favorite, favorite, underdog, underdog, favorite, underdog, favorite)
  
  # Define the simulation function
  simulate_series <- function(favorite_wins, underdog_wins) {
    for (game in home_court_order) {
      if (game == favorite) {
        win <- runif(1) < favorite_home_prob
        if (win) favorite_wins <- favorite_wins + 1 else underdog_wins <- underdog_wins + 1
      } else {
        win <- runif(1) < underdog_home_prob
        if (win) underdog_wins <- underdog_wins + 1 else favorite_wins <- favorite_wins + 1
      }
      
      if (favorite_wins == 4) return(paste(favorite, "4", underdog, underdog_wins))
      if (underdog_wins == 4) return(paste(underdog, "4", favorite, favorite_wins))
    }
    return(ifelse(favorite_wins == 4, paste(favorite, "4", underdog, underdog_wins), paste(underdog, "4", favorite, favorite_wins)))
  }
  
  # Perform Monte Carlo simulation
  iterations <- 100000
  results <- replicate(iterations, simulate_series(favorite_wins, underdog_wins))
  
  # Calculate overall probabilities
  favorite_prob <- mean(grepl(paste0(favorite, " 4"), results))
  underdog_prob <- mean(grepl(paste0(underdog, " 4"), results))
  
  # Display overall win probabilities
  cat(favorite, "wins probability:", round(favorite_prob * 100, 2), "%\n")
  cat(underdog, "wins probability:", round(underdog_prob * 100, 2), "%\n")
  
  # Convert probabilities to American odds
  favorite_odds <- prob_to_american_odds(favorite_prob)
  underdog_odds <- prob_to_american_odds(underdog_prob)
  
  # Print the fair odds
  cat("\nSeries Fair Prices:\n")
  cat(favorite, "odds:", favorite_odds, "\n")
  cat(underdog, "odds:", underdog_odds, "\n")
  
  # Calculate and display individual series outcome probabilities
  outcome_probs <- table(results) / iterations
  cat("\nIndividual Series Outcome Probabilities and Odds:\n")
  for (outcome in names(outcome_probs)) {
    prob <- outcome_probs[outcome]
    odds <- prob_to_american_odds(prob)
    cat(outcome, "probability:", round(prob * 100, 2), "%", "Fair Price:", odds, "\n")
  }
}

# Run the Monte Carlo simulation
monte_carlo_best_of_7()
