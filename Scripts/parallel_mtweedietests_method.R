# Libraries
library(tweedie)
library(ggplot2)
library(parallel)
library(magrittr)
library(tidyverse)

# Functions
simTweedieTest <- function(N) { 
  t.test(rtweedie(N, mu=10000, phi=100, power=1.9), mu=10000)$p.value 
}

# Parallel MTweedieTests
MTweedieTests_parallel <- function(N, M, sig) {
  num_cores <- detectCores() - 1
  results <- mclapply(1:M, function(x) simTweedieTest(N), mc.cores=num_cores)
  sum(unlist(results) < sig) / M
}

# Non-parallel MTweedieTests
MTweedieTests <- function(N, M, sig) { 
  sum(replicate(M, simTweedieTest(N)) < sig) / M 
}

df <- expand.grid(N = c(10,100,1000,5000, 10000), M = 1000, share_reject = NA) 

# Using parallelized function for Assignment 3
num_cores <- detectCores() - 1
compute_share_reject <- function(i) {
  MTweedieTests_parallel(N = df$N[i], M = df$M[i], sig = .05)
}
results <- mclapply(1:nrow(df), compute_share_reject, mc.cores = num_cores)
df$share_reject <- unlist(results)

# For Assignment 4
simDat <- function(N, type, mu) {
  if (type == "tweedie") {
    return(rtweedie(N, mu = mu, phi = 100, power = 1.9))
  }
  if (type == "normal") {
    return(rnorm(N, mean = mu))
  }
  stop("invalid distribution")
}

simTest <- function(N, type, mu) {
  t.test(simDat(N = N, type = type, mu = mu), mu = mu)$p.value
}

MTests <- function(N, M, type, sig) {
  sum(replicate(M, simTest(N = N, type = type, mu = 10000)) < sig) / M
}

df2 <- expand.grid(N = c(10, 100, 1000, 5000, 10000), M = 1000, type = c("tweedie", "normal"), share_reject = NA) 

for(i in 1:nrow(df2)) {
  df2$share_reject[i] <- MTests(df2$N[i], df2$M[i], df2$type[i], .05)
}

# Plotting
df2 %>%
  ggplot2::ggplot(aes(x = log(N), y = share_reject, col = type)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw() 
