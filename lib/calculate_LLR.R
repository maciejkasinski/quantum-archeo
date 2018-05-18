calculate_LLR = function(mainset, subsets_list) {
  
  ### mainset
  results = calculate_quantogram(mainset)
  q = filter(results, f_q == max(f_q))$q
  error = calculate_error(mainset, q)
  under_sum = cos(2 * pi * error / q)
  LLR1 = sum(under_sum)
  
  LLR2 = 0
  for (datai. in subsets_list) {
    results = calculate_quantogram(datai.)
    q = filter(results, f_q == max(f_q))$q
    error = calculate_error(datai., q)
    under_sum = cos(2 * pi * error / q)
    LLR2 = LLR2 + sum(under_sum)
  }
  
  return(list("upper"=LLR1,
              "lower"=LLR2,
              "ratio"=LLR1-LLR2))
}
