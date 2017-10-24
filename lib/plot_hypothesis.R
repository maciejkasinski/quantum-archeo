plot_hypothesis = function(data_llr, original_LLR) {
  
  borders = quantile(data_llr$llr, c(0.05, 0.95))
  p = ggplot(data_llr) +
    geom_histogram(aes(llr), color="gray", fill="white", bins = 30) +
    geom_vline(xintercept=original_LLR, colour="black", linetype="dashed") +
    geom_text(mapping=aes(x=original_LLR, y=2, label=paste0("LLR for original data: ", round(original_LLR, 2))),
              size=3, angle=90, vjust=-0.3, hjust=-0.1, colour = "black") +
    geom_vline(xintercept=borders, colour="red", linetype="dashed") +
    geom_text(mapping=aes(x=borders[1], y=2, label="rejection region"),
              size=3, angle=90, vjust=-3, hjust=-0.1, colour = "red") +
    geom_text(mapping=aes(x=borders[1], y=2, label="acceptance region"),
              size=3, angle=90, vjust=3, hjust=-0.1, colour = "green") +
    geom_text(mapping=aes(x=borders[2], y=2, label="acceptance region"),
              size=3, angle=90, vjust=-3, hjust=-0.1, colour = "green") +
    geom_text(mapping=aes(x=borders[2], y=2, label="rejection region"),
              size=3, angle=90, vjust=3, hjust=-0.1, colour = "red") +
    xlab("test statistic (LLR)") +
    theme_minimal()
  p
  return(p)
}
