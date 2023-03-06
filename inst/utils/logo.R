library(ggplot2)
library(tidyr)
library(ggdist)
library(purrr)
library(dplyr)
library(hexSticker)
library(showtext)

# Loading Google fonts (http://www.google.com/fonts)
font_add_google("Righteous", "righteous")

n <- 10000
row_df <- data.frame(row = as.factor(1:n))
row_df$k <- abs(rnorm(n, 1, 0.2))
row_df$x_0 <- rnorm(n, 0, 0.75)

x <-  seq(-4, 4, 0.1)

logistic <- function(x, k, x_0) 1/(1 + exp(-k * (x - x_0)))

dat <- expand_grid(row_df, data.frame(x))
dat$y <- logistic(dat$x,  dat$k, dat$x_0)

clrs <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")


hex_plot <- dat %>%
    group_by(x) %>%
    median_qi(y, .width = c(0.95, 0.89, 0.80, 0.67, 0.50)) %>%
    ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
    geom_lineribbon(colour = NA) +
    stat_interval(data = distinct(dat, row, x_0),
                  aes(x = x_0, y = -0.15), 
                  size = 3,
                  .width = c(0.95, 0.89, 0.80, 0.67, 0.50),
                  inherit.aes = FALSE) +
    scale_colour_manual(values = rev(clrs)) +
    scale_fill_manual(values = rev(clrs)) +
    theme_void() +
    theme(legend.position = "none")

sticker(hex_plot,
        package = "bvq",
        p_size = 35, 
        p_x = 0.5,
        dpi = 500,
        p_color = clrs[1],
        p_y = 1.4,
        p_family = "righteous",
        h_color = clrs[4],
        h_fill = "white",
        s_x = 1, 
        s_y = 0.7,
        s_width = 3, 
        s_height = 1.7,
        filename = "inst/figures/logo.png")

sticker(hex_plot,
        package = "bvq",
        p_size = 35, 
        p_x = 0.5,
        dpi = 500,
        p_color = clrs[1],
        p_y = 1.4,
        p_family = "righteous",
        h_color = clrs[4],
        h_fill = "white",
        s_x = 1, 
        s_y = 0.7,
        s_width = 3, 
        s_height = 1.7,
        filename = "man/figures/logo.png")


