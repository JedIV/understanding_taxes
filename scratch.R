# Lets talk about TAXES
library(ggplot2)
library(scales)

# Generate function for piecewise splits
splits    <- c(  0, 9525, 38700, 82500, 157500, 200000, 500000)
rates     <- c(0.1, 0.12,  0.22,  0.24,   0.32,   0.35,   0.37)
rate_function <- stepfun(splits[-1], rates)
max_income <- 1000000
max_rate   <- tail(rates,1)

# Calculate actual taxes paid
taxes <- function(x, ...){
    bracket <- max(which(splits < x))  
    priors <- sum((splits[1:bracket]- c(0,splits[1:(bracket - 1)])) * c(0,rates[1:(bracket - 1)]))
    marginal <- rates[bracket]*(x - splits[bracket])   
    return(priors + marginal)
}

# Calculate Take Home Pay
take_home <- function(x, ...){
    return(x - taxes(x))
}

# calculate the actual tax rate
effective_tax_rate <- function(x, ...){
    return(taxes(x) / x)
}

# vectorize the tax rate function
v_tax_rate <- function(x, splits, rates){
    return(sapply(x, FUN = effective_tax_rate))
}

# vectorize the taxes paid function
v_taxes_paid <- function(x, splits, rates){
    return(sapply(x, FUN = taxes))
}

# vectorize the taxes paid function
v_take_home <- function(x, splits, rates){
    return(sapply(x, FUN = take_home))
}

# Plot marginal and effective rates, total taxes paid, and take home pay
ggplot(data.frame(x=c(1,max_income)), aes(x)) +
  stat_function(fun=rate_function, geom="step", aes(colour="Marginal Rate"), n=1000, size = 0.5) +
  stat_function(fun=v_tax_rate, geom="path", aes(colour="Effective Rate"),   n=1000, size = 0.5) +
  scale_y_continuous(label=percent, limits = c(0,max_rate)) + 
  scale_x_continuous(label=comma) + 
  xlab("Income") +
  ylab("Tax Rate")

ggplot(data.frame(x=c(1,max_income)), aes(x)) +
  stat_function(fun=v_taxes_paid, geom="path", aes(colour="Total Taxes paid"),n=1000, size = 0.5) +
  stat_function(fun=v_take_home, geom="path", aes(colour="Take Home Pay"),    n=1000, size = 0.5) +
  scale_y_continuous(label=comma, limits = c(0,max_income)) + 
  scale_x_continuous(label=comma) + 
  xlab("Income") +
  ylab("Dollars per Year")
