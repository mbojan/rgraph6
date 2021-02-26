library(dplyr)
library(purrr)
library(igraph)


# Generate ----------------------------------------------------------------

set.seed(666)
# Generate graphs
d <- tibble(
  # Nodeset sizes
  vc = c(15, 30, 60, 120),
  # Edgeset sizes
  n = map(vc, ~ floor(seq(
    0.1 * .x * (.x - 1) / 2, 
    0.2 * .x * (.x - 1) / 2, 
    length = 5
  )))
) %>%
  tidyr::unnest("n") %>%
  mutate(
    g = map2(vc, n, ~ sample_gnm(.x, .y, directed = FALSE)),
    den = map_dbl(g, graph.density),
    g6 = as_graph6(g),
    s6 = as_sparse6(g)
  ) %>%
  tidyr::pivot_longer(
    one_of('g6', 's6'),
    names_to = "type",
    values_to = "value"
  ) %>%
  mutate(
    nch = nchar(value)
  )


# Visualize ---------------------------------------------------------------

if(FALSE) {
  # Size by density and vcount
  d %>%
    ggplot(aes(x=den, y = nch, group = interaction(type, vc), color = type)) +
    geom_point() +
    geom_line() +
    scale_y_log10()
}





# Save --------------------------------------------------------------------

g6 <- with(d, value[type == "g6"])
usethis::use_data(g6, overwrite = TRUE)

s6 <- with(d, value[type == "s6"])
usethis::use_data(s6, overwrite = TRUE)

d6 <- as_dgraph6(lapply(with(d, g[type == "s6"]), as.directed, mode = "arbitrary"))
usethis::use_data(d6, overwrite = TRUE)
