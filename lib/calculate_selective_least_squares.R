calculate_selective_least_squares = function(sizes, quanta) {
  
  least_absolute_reminder = function(x, q) {
    lower <- floor(x / q)
    upper <- ceiling(x / q)
    
    rem_lower <- x - q * lower
    rem_upper <- x - q * upper
    
    abs_rem <- pmin(abs(rem_lower), abs(rem_upper))
    min_abs_rem <-
      ifelse(abs_rem == abs(rem_upper), rem_upper, rem_lower)
    min_abs_rem
  }
  
  quanta <- unique(quanta)
  sum = 0
  for (i in 1:length(sizes)) {
    X <- sizes[i]
    g_sq = c()
    for (j in 1:length(quanta)) {
      g_sq <- c(g_sq, least_absolute_reminder(X, quanta[j]) ^ 2)
    }
    sum <- sum + min(g_sq)
  }
  sum
}
