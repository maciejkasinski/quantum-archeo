calculate_KDE = function(data_input, band = 0.05) {
  bkde(data_input, kernel = "normal", bandwidth = band) %>% as.data.frame %>%
    filter(x > 0, y > 0) -> kde
  return(kde)
}

sample_KDE = function(kde, size = 1000) {
  sample <- sample(size = size, x = kde$x, replace = T, prob = kde$y/sum(kde$y))
  return(sample)
}
