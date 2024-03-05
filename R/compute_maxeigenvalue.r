# compute_maxeigenvalue <- function(data, relsusinf = NULL) {
#   if(length(relsusinf) == 0) relsusinf <- rep(1, sqrt(length(data)))
#   max(Re(eigen((relsusinf %o% relsusinf)*matrix(data, ncol = sqrt(length(data))))$values))
# }

compute_maxeigenvalue <- function(data, relsus = NULL, relinf = NULL) {
  if(length(relsus) == 0) relsus <- rep(1, sqrt(length(data)))
  if(length(relinf) == 0) relinf <- rep(1, sqrt(length(data)))
  max(Re(eigen((relsus %o% relinf)*matrix(data, ncol = sqrt(length(data))))$values))
}

