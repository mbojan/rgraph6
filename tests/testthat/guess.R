# Guesss format from string

library(magrittr)
library(purrr)
library(igraph)

iglist <- c(50, 100, 150) %>%
  lapply(ba.game, directed=FALSE)

g6 <- vapply(iglist, as_graph6, character(1))
s6 <- vapply(iglist, as_sparse6, character(1))
