doc_bootstrap_plot = function(df = data_niche,
                              sector0,
                              build0 = NULL,
                              update_bootstrap = FALSE,
                              extra_suffix = "",
                              n_bootstrap_samples = 500) {
  
  
  if (is.null(build0)) {
    df <- data_niche %>% filter(sector == sector0)
  } else {
    df <- data_niche %>% filter(sector == sector0) %>%
      filter(build %in% build0)
  }
  
  set = df %>% get_size
  results = calculate_quantogram(set)
  # TODO: if >1 maximum
  best_score = results %>% group_by %>% filter(f_q == max(f_q))
  original_quanta = data.frame(sector = sector0,
                               q_hat = best_score$q,
                               peak_hat = best_score$f_q)
  
  # Bootstrap confidence interval procedure
  if (update_bootstrap) {
    
    results_all = NULL
    print(sector0)
    p <- progress_estimated(n_bootstrap_samples, min_time = 0)
    results_sector = NULL
    for (n in 1:nsamples) {
      
      # 1. Sample from original dataset
      sample = sample_bootstrap(set)
      
      # 2. Estimate quantum and peak
      results = calculate_quantogram(sample)
      
      # 3. Collect results
      # TODO: if >1 maximum
      win_score = results %>% group_by %>% filter(f_q == max(f_q))
      q_hat = win_score$q
      peak_hat = win_score$f_q
      
      results_sector[[n]] = data.frame(q_hat, peak_hat)
      
      p$tick()$print()
    }
    results_sector = do.call(rbind.data.frame, results_sector)
    df_results_all = data.frame(sector = sector0, results_sector)
    
    save(df_results_all, file=paste0("../data/output/bootstrap_", sector0, extra_suffix ,".RData"))
  } else {
    load(file = paste0("../data/output/bootstrap_", sector0, extra_suffix, ".RData"))
  }
  
  # Plot adjustment
  borders = quantile(df_results_all$q_hat, c(0.025, 0.975))
  df_results_all %>% group_by() %>% summarise(min = min(q_hat), max = max(q_hat)) -> limits
  eps = (limits$max - limits$min) * 1.5 / 2           # scale up x axis range
  center = limits$min + (limits$max - limits$min) / 2  # get center position
  
  # Plot boostrap confidence interval for quantum
  p <- ggplot(df_results_all,
              aes(q_hat)) +
    geom_histogram(binwidth = 0.005,
                   fill = "white",
                   color = "black") +
    scale_x_continuous(limits = c(pmax(center - eps, 0), pmin(center + eps, 1))) +
    geom_vline(
      data = original_quanta,
      aes(xintercept = q_hat),
      colour = col_blu,
      linetype = "dashed",
      size = 1) +
    geom_vline(
      xintercept = borders,
      colour = col_neg,
      linetype = "dashed",
      size = 1) +
    xlab(label = "quantum estimation") +
    theme_minimal()
  p
}
