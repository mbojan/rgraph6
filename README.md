
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rgraph6`: Representing Graphs as graph6, dgraph6 or sparse6 Strings

<!-- badges: start -->

[![R-CMD-check](https://github.com/mbojan/rgraph6/workflows/R-CMD-check/badge.svg)](https://github.com/mbojan/rgraph6/actions)
[![rstudio mirror
downloads](http://cranlogs.r-pkg.org/badges/rgraph6?color=2ED968)](http://cranlogs.r-pkg.org/)
[![cran
version](http://www.r-pkg.org/badges/version/rgraph6)](https://cran.r-project.org/package=rgraph6)
<!-- badges: end -->

Functions in this package allow for converting network data (undirected
graphs of size up to 128 nodes) to compact graph6 symbols and back.
Graph6 symbols are convenient in a number of contexts, especially when
working with large number of graphs.

## What are graph6, sparse6 and dgraph6 formats?

Graph6 is a compact format for representing undirected graphs as strings
of printable ASCII characters due to [Brendan
McKay](https://en.wikipedia.org/wiki/Brendan_McKay). See
[here](http://users.cecs.anu.edu.au/~bdm/data/formats.txt) for format
specification.

## Installation

<!--
Install released version from CRAN with:


```r
install.packages("rgraph6")
```
-->

Install development version from GutHub with:

``` r
# install.packages("remotes")
remotes::install_github("mbojan/rgraph6", build_vignettes=TRUE)
```

## Usage

Top level functions are

-   `adjacency_from_text()`
-   `edgelist_from_text()`
-   `igraph_from_text()`
-   `network_from_text()`

which try to guess the format used. For example:

``` r
# Create a vector with a mixture of graph6, dgraph6 and sparse6 symbols
x <- c(g6[1], s6[2], d6[3])
igraph_from_text(x)
#> [[1]]
#> IGRAPH 1464169 U--- 15 10 -- 
#> + edges from 1464169:
#>  [1]  1-- 7  1--11  2-- 7  2--11  2--12  2--15  5-- 9  7--10  8--15 13--15
#> 
#> [[2]]
#> IGRAPH 1463c87 U--- 15 13 -- 
#> + edges from 1463c87:
#>  [1]  2-- 7  2-- 9  4--10  6--10  6--12  7--12 11--12  5--13  6--13 10--13
#> [11]  4--15 10--15 14--15
#> 
#> [[3]]
#> IGRAPH 14643da D--- 15 15 -- 
#> + edges from 14643da:
#>  [1] 1-> 8 1->11 1->12 1->13 2->13 2->14 3->10 4-> 7 4-> 9 5-> 8 5->10 5->11
#> [13] 5->13 6-> 8 9->14
network_from_text(x)
#> Loading required namespace: network
#> [[1]]
#>  Network attributes:
#>   vertices = 15 
#>   directed = FALSE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 10 
#>     missing edges= 0 
#>     non-missing edges= 10 
#> 
#>  Vertex attribute names: 
#>     vertex.names 
#> 
#> No edge attributes
#> 
#> [[2]]
#>  Network attributes:
#>   vertices = 15 
#>   directed = FALSE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 13 
#>     missing edges= 0 
#>     non-missing edges= 13 
#> 
#>  Vertex attribute names: 
#>     vertex.names 
#> 
#> No edge attributes
#> 
#> [[3]]
#>  Network attributes:
#>   vertices = 15 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 15 
#>     missing edges= 0 
#>     non-missing edges= 15 
#> 
#>  Vertex attribute names: 
#>     vertex.names 
#> 
#> No edge attributes
```

### Tidy graph databases

The formats shine if we need to store large number of graphs in a data
frame. Let’s generate a list of random graphs as igraph objects and
store them in a data frame column of graph6 symbols:

``` r
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Generate list of igraph objects
set.seed(666)

d <- tibble::tibble(
  g6 = replicate(
    10,
    igraph::random.graph.game(sample(3:12, replace=TRUE), p=.5, directed=FALSE),
    simplify=FALSE
  ) %>%
    as_graph6()
)
d
#> # A tibble: 10 x 1
#>    g6            
#>    <chr>         
#>  1 "FblF_"       
#>  2 "DFc"         
#>  3 "HfTaMwk"     
#>  4 "KefToktrftZ~"
#>  5 "JPraDzZQ?M?" 
#>  6 "Bo"          
#>  7 "Ed`w"        
#>  8 "Gpuq|{"      
#>  9 "EbSG"        
#> 10 "ICNa@Gg\\o"
```

Nice and compact. We can go further by doing some computations and
saving the results together with the graph data, and even save it to a
simple CSV file!

``` r
d %>%
  dplyr::mutate(
    igraphs = igraph_from_text(g6),
    vc = purrr::map_dbl(igraphs, igraph::vcount),
    ec = purrr::map_dbl(igraphs, igraph::ecount),
    density = purrr::map_dbl(igraphs, igraph::edge_density)
  ) %>%
  dplyr::select(-igraphs) %>%
  write.csv(row.names = FALSE)
#> "g6","vc","ec","density"
#> "FblF_",7,11,0.523809523809524
#> "DFc",5,5,0.5
#> "HfTaMwk",9,18,0.5
#> "KefToktrftZ~",12,41,0.621212121212121
#> "JPraDzZQ?M?",11,24,0.436363636363636
#> "Bo",3,2,0.666666666666667
#> "Ed`w",6,8,0.533333333333333
#> "Gpuq|{",8,19,0.678571428571429
#> "EbSG",6,6,0.4
#> "ICNa@Gg\o",10,17,0.377777777777778
```
