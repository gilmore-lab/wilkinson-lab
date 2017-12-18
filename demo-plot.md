demo-plot
================
Rick Gilmore
2017-12-18 15:53:57

Create random test data file
----------------------------

``` r
n_subs <- 20
dx_groups <- c("TD", "DS", "ASD", "IDD")
n_dx_groups <-  length(dx_groups)
menu_pos <- c("Top", "Bottom", "Left", "Right")
n_menu_pos <- length(menu_pos)
aois <- c("People", "Actions", "Bgnd")
n_aois <- length(aois)

df <- expand.grid(sub_id = 1:20,
                 dx = dx_groups,
                 menu = menu_pos,
                 aoi = aois)

df$pct_lks <- runif(dim(df)[1], 0, 100)
```

Create test plot
----------------

``` r
p <- df %>%
  ggplot(aes(aoi, pct_lks)) +
  facet_grid(dx ~ menu) +
  geom_violin() +
  geom_point() +
  ggtitle("Percent looks by group, menu position, and area of interest")
p
```

![](demo-plot_files/figure-markdown_github-ascii_identifiers/pct-lks-by-dx-menu-pos-aoi-1.png)
