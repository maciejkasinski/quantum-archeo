calculate_error = function(input, quantum) {
  n_multiply = round(input / quantum, 0) # of floor?
  error = input - n_multiply * quantum
  return(error)
}
