
biascorrected_boot_ci <- function(theta_hat, theta_boot, conf_int = 0.95) {
  
  # no acceleration calculated, because not possible to retain raw bootstrap samples
  alpha <- 1-conf_int
  u <- c(alpha/2, 1-alpha/2)
  z0 <- qnorm(mean(theta_boot <= theta_hat))
  zu <- qnorm(u)
  u_bias <- pnorm(z0 + (z0+zu))
  boot_ci <- quantile(theta_boot, u_bias)
  
  setNames(boot_ci, NULL) 
}


# # https://stats.stackexchange.com/questions/437477/calculate-accelerated-bootstrap-interval-in-r
# # https://arch.readthedocs.io/en/latest/bootstrap/confidence-intervals.html
# 
# set.seed(42)
# n <- 30 #Sample size
# x <- round(runif(n, 0, 100))
# 
# test <- function(x) var(x)/mean(x)^2 - 1/mean(x)
# 
# theta_hat <- test(x)
# theta_boot <- replicate(400, test(sample(x, replace = TRUE)))
# 
# 
# conf_int <- 0.95
# alpha <- 1-conf_int
# 
# 
# 
# #Desired quantiles
# u <- c(alpha/2, 1-alpha/2)
# 
# 
# #Percentile Bootstrap CI
# quantile(theta_boot, u)
# 
# 
# #Empirical/basic Bootstrap CI
# rev(theta_hat - quantile(theta_boot - theta_hat, u))
# 
# 
# #Compute constants
# z0 <- qnorm(mean(theta_boot <= theta_hat))
# zu <- qnorm(u)
# 
# #Compute acceleration
# I <- rep(NA, n)
# for(i in 1:n){
#   #Remove ith data point
#   xnew <- x[-i]
#   #Estimate theta
#   #theta_jack <- var(xnew)/mean(xnew)^2 - 1/mean(xnew)
#   theta_jack <- test(xnew)
#   I[i] <- (n-1)*(theta_hat - theta_jack)
# }
# #Estimate a
# a_hat <- (sum(I^3)/sum(I^2)^1.5)/6
# 
# 
# #Adjusted quantiles
# u_adjusted <- pnorm(z0 + (z0+zu)/(1-a_hat*(z0+zu)))
# 
# #Accelerated Bootstrap CI
# quantile(theta_boot, u_adjusted)
# 
# #Bias corrected Bootstrap CI
# # setting a_hat to 0
# u_bias <- pnorm(z0 + (z0+zu))
# quantile(theta_boot, u_bias)
# 
# #Bias corrected Bootstrap CI
# # by shifting
# quantile(theta_boot, u) - median(theta_boot) + theta_hat



