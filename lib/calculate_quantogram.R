calculate_quantogram = function(measurements,
                                params=CONSTANTS_QUANTOGRAM) {
  
  if (4 != sum(c("RNG_START", "RNG_END", "STEP", "Q_MIN") %in% names(params))) {
    print("Incorrect params")
    return(NULL)
  }
  
  N = length(measurements)
  A = sqrt(2 / N)
  
  
  results = list()
  rng = seq(params$RNG_START, params$RNG_END, by = params$STEP)
  
  for (n in 1:length(rng)) {
    q = rng[n]
    if (q < params$Q_MIN)
      next
    sum = 0
    for (i in 1:N) {
      e = measurements[i] %% q
      cosVal = 2.0 * pi * e / q
      sum = sum + cos(cosVal)
    }
    f_q = A * sum # quanta
    results[[n]] = c(q, f_q)
  }
  
  results = as_tibble(do.call(rbind, results))
  names(results) = c("q", "f_q")
  return(results)
}

get_quantum = function(measurements) {
  calculate_quantogram(measurements) %>% filter(f_q==max(f_q)) %>% pull(q)
}
