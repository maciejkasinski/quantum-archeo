sample_bootstrap = function(set, decrease_by=0) {
  sample(size = length(set) - decrease_by,
         x = set,
         replace = T)
}
