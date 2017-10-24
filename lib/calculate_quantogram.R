calculate_quantogram = function(v_input,
                                qMin = Q_MIN,
                                rangeStart = RNG_START,
                                rangeEnd = RNG_END,
                                step = STEP) {
  n = v_input %>% length
  A = sqrt(2 / n)
  results = NULL
  
  rng <- seq(rangeStart, rangeEnd, by = step)
  for (q in rng) {
    if (q < qMin)
      next
    sum = 0
    for (i in 1:n) {
      e = v_input[i] %% q
      cosVal = 2.0 * pi * e / q
      sum = sum + cos(cosVal)
    }
    f_q = A * sum # quanta
    results = rbind(results, data.frame(q, f_q))
  }
  return(results)
}
