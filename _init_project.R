library(tidyverse)
library(ggplot2)
library(stringi)
library(RColorBrewer)
library(directlabels)

data_niche <-
  read.csv2("data/sample_data.csv", stringsAsFactors = F) %>%
  mutate(size = as.numeric(size)) %>% as_tibble()

# Tools ----

# computing
source("lib/helpers.R")
source("lib/unround_data.R")
source("lib/calculate_quantogram.R")
source("lib/calculate_error.R")
source("lib/calculate_LLR.R")
source("lib/functions_KDE.R")

# plotting
source("lib/plot_quantogram.R")
source("lib/plot_hypothesis.R")
source("lib/doc_bootstrap_plot.R")

# Cosine quantogram parameters ----
source("config/CONSTANTS_QUANTOGRAM.R")

# Palette
col_neg = RColorBrewer::brewer.pal(9, "Reds")[6]
col_pos = RColorBrewer::brewer.pal(9, "Greens")[6]
col_neu = RColorBrewer::brewer.pal(9, "Greys")[5]
col_blu = RColorBrewer::brewer.pal(9, "Blues")[6]
