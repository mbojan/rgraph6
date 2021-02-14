library(grid)
library(hexSticker)
library(magrittr)
library(tidygraph)
library(ggraph)



# 1 -----------------------------------------------------------------------

dyad <- igraph::make_graph(~ a +--+ b)
g <- igraph::make_graph(~ a -- b -- c -- d -- e -- f -- c)

p <- dyad %>%
  as_tbl_graph() %>%
  create_layout(
    "manual",
    x = 1:2,
    y = 1
  ) %>%
  ggraph() +
  geom_node_text(aes(label = c("", as_graph6(g))), size=10) +
  geom_edge_arc(
    start_cap = circle(10, "pt"),
    end_cap = circle(10, "pt"),
    strength = 0.9,
    arrow = arrow(
      angle = 20,
      length = unit(5, "pt"),
      type = "closed",
      ends = "last"
    )
  ) +
  # theme_void() +
  scale_x_continuous(expand=c(0, .2))

p.graph <- g %>%
  create_layout(
    "manual",
    x = c(2, 1, 1, 1, 2, 2),
    y = c(3, 3, 2, 1, 1, 2)
  ) %>%
  ggraph() +
  geom_edge_link(width=.2) +
  geom_node_point(size=.2) +
  theme_void() +
  xlim(1:2) +
  ylim(c(1,3)) +
  scale_x_continuous(expand=c(.1, .1))

x <- 1.03; y <- 1; wx <- .075; wy <- wx * 2
s <- p + annotation_custom(
  ggplotGrob(p.graph),
  xmin = x -wx/2, xmax = x+wx/2,
  ymin = y-wy/2, ymax = y + wy/2
)


sticker(
  s,
  s_x = 1,
  s_y = 1,
  s_width = 2,
  s_height = 1.5,
  package = "rgraph6",
  p_color = "gray50",
  h_fill = "#ffffff",
  h_color = "black",
  url = "https://mbojan.github.io/rgraph6",
  u_size = 4,
  filename = "man/figures/logo.png",
  dpi = 300
)


# 2 -----------------------------------------------------------------------

set.seed(666)
g <- igraph::sample_pa(48, power = 0.5, directed = FALSE)

png("logo-graph.png")
g %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_link() +
  theme_void()
dev.off()

img <- png::readPNG("logo-graph.png")
htc <- halftoner::halftone(img)
plot(htc)
