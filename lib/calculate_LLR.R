calculate_LLR = function(mainset, subsets_list) {
  
  ### mainset
  results = calculate_quantogram(mainset)
  q = results %>% group_by %>% filter(f_q == max(f_q)) %>% .$q
  error = calculate_error(mainset, q)
  under_sum = cos(2 * pi * error / q)
  LLR1 = sum(under_sum)

  LLR2 = 0
  for (datai. in subsets_list) {
    results = calculate_quantogram(datai.)
    q = results %>% group_by %>% filter(f_q == max(f_q)) %>% .$q
    error = calculate_error(datai., q)
    under_sum = cos(2 * pi * error / q)
    LLR2 = LLR2 + sum(under_sum)
  }
  
  message(paste0("LLR1 ", LLR1))
  message(paste0("LLR2 ", LLR2))
  message(paste0("LLR1 - LLR2 ", LLR1 - LLR2))
  
  return(LLR1 - LLR2)
}
