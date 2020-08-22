# Function from Ávila-Ponce de León et al. https://www.medrxiv.org/content/10.1101/2020.05.11.20098517v1.full.pdf

betaDays <- function(days, beta_0, tau_beta, beta_1){
  beta_t <- beta_0*exp(-days/tau_beta) + beta_1
  return(beta_t)
}

days   <- 1:40
beta_0 <- 0.605
beta_1 <- 0.01
tau_beta <- 30
beta <- betaDays(days, beta_0, tau_beta, beta_1 )

plot(days, beta)
