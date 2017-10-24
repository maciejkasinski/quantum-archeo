doc_size_distr_plot = function(df = data_niche,
                               sector0,
                               build0 = NULL,
                               update = FALSE,
                               suffix = "",
                               margin = 0.1) {
  df <- df %>% filter(sector == sector0)
  quantogram = calculate_quantogram(df %>% .$size %>% unround_data)
  q0 = quantogram %>% group_by() %>% filter(f_q == max(f_q)) %>% .$q
  
  df_plot <- filter(df, sector == sector0) %>%
    arrange(size) %>%
    mutate(x = row_number())
  
  df_plot %>%
    mutate(
      close_to_q1 = ifelse(abs(q0 - size) / (q0) <= margin, T, F),
      close_to_q2 = ifelse(abs(q0 * 2 - size) / (q0 * 2) <= margin, T, F),
      close_to_q3 = ifelse(abs(q0 * 3 - size) / (q0 * 3) <= margin, T, F)
    ) -> df2
  
  
  df2 %>% group_by(close_to_q1) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q1) -> rng1
  df2 %>% group_by(close_to_q2) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q2) -> rng2
  df2 %>% group_by(close_to_q3) %>% summarise(min = min(size), max = max(size)) %>%
    filter(close_to_q3) -> rng3
  
  filter(df_plot, size >= rng1$min, size <= rng1$max) %>%
    summarise(min = min(x), max = max(x)) -> x_rng1
  
  # possibly 2*quantum not on the plot
  if (nrow(rng2) == 0) {
    x_rng2[1, "min"] <- 0
    x_rng2[1, "max"] <- 0
  } else { 
    filter(df_plot, size >= rng2$min, size <= rng2$max) %>%
      summarise(min = min(x), max = max(x)) -> x_rng2
    
  }
  if (nrow(rng3) == 0) {
    x_rng3[1, "min"] <- 0
    x_rng3[1, "max"] <- 0
  } else {
    filter(df_plot, size >= rng3$min, size <= rng3$max) %>%
      summarise(min = min(x), max = max(x)) -> x_rng3
  }
  
  p = ggplot(df_plot, aes(x = x, y = size)) +
    geom_segment(aes(xend = x, yend = 0), size = 0.3) +
    scale_x_continuous("") +
    scale_y_continuous(name = "Measurement length [m]", limits = c(0, 1.5)) +
    geom_dl(aes(label = sector0),
            method = list(
              "last.points",
              rot = 90,
              hjust = 1,
              vjust = 1.5
            )) +
    # scale_color_manual(values=cosine_palette_3) +
    # guides(colour = guide_legend(override.aes = list(size=5))) +
    guides(colour = "none") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.text.x = element_blank(),
      legend.position = "top"
    )
  p +
    annotate(
      "rect",
      xmin = x_rng1$min,
      xmax = x_rng1$max,
      ymin = q0 * (1 - margin / 2),
      ymax = q0 * (1 + margin / 2),
      alpha = 0.2,
      color = "blue",
      fill = "blue"
    ) +
    annotate(
      "rect",
      xmin = x_rng2$min,
      xmax = x_rng2$max,
      ymin = q0 * 2 * (1 - margin / 2),
      ymax = q0 * 2 * (1 + margin / 2),
      alpha = 0.2,
      color = "blue",
      fill = "blue"
    ) +
    annotate(
      "rect",
      xmin = x_rng3$min,
      xmax = x_rng3$max,
      ymin = q0 * 3 * (1 - margin / 2),
      ymax = q0 * 3 * (1 + margin / 2),
      alpha = 0.2,
      color = "blue",
      fill = "blue"
    )
}
