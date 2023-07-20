library(dplyr)
library(tibble)
library(purrr)
library(e1071)
library(ggplot2)
library(scico)

seed <- 1
set.seed(seed)

radians <- function(n) {
  seq(0, 2 * pi, length.out = n + 1)[1:n]
}

heart <- function(n = 80, r = 1) {
  th <- radians(n)
  x <- (16 * sin(th)^3)/17
  y <- (13 * cos(th) - 5 * cos(2 * th) - 2 * cos(3 * th) - 
          cos(4 * th))/17
  x <- x - mean(x)
  y <- y - mean(y)
  tibble(xh = x * r, yh = y * r, th)
}

circle <- function(n = 80, r = 1) {
  th <- radians(n)
  y <- cos(th)
  x <- sin(th)
  tibble(xc = x * r, yc = y * r, th)
}

square <- function(n = 80, r = 1) {
  x <- seq(0,r, length.out = n/8)
  y <- rep(r, n/8)
  x <- c(x, rep(r, n/4))
  y <- c(y, seq(r, -r, length.out = n/4))
  x <- c(x, seq(r, -r, length.out = n/4))
  y <- c(y, rep(-r, n/4))
  x <- c(x, rep(-r, n/4))
  y <- c(y, seq(-r, r, length.out = n/4))
  x <- c(x, seq(-r,0, length.out = n/8))
  y <- c(y, rep(r, n/8))
  tibble(xc = x, yc = y, th = radians(n))  
}

walk <- function(from, to, n, smooth = 2) {
  signal <- seq(from, to, length.out = n)
  noise <- c(0, rbridge(1, n - 3))
  while(smooth > 0) {
    noise <- (lag(noise) + noise + lead(noise))/3
    smooth <- smooth - 1
  }
  noise <- c(0, noise, 0)
  signal + noise
}

path <- function(data, n) {
  tibble(
    id = data$id,
    time = 1:n,
    x = walk(data$xc, data$xh, n),
    y = walk(data$yc, data$yh, n)
  )
}

npoints <- 20000
nsteps <- 200
heart_size <- 5
sz <- 12

dat <- full_join(
  x = heart(npoints, heart_size), 
  y = square(npoints, sz), 
  by = "th"
) %>%
  mutate(id = 1:n()) %>%
  transpose() %>%
  map_dfr(~path(.x, nsteps)) %>%
  mutate(
    shade = abs(2*id/npoints - 1) + sin(time/nsteps)/2
  )

pic <- ggplot(dat, aes(x,y, 
                       group = id, 
                       colour =  shade)) + 
  geom_path(show.legend = FALSE, size = .2, alpha = .3) + 
  theme_void() +
  scale_color_viridis_c(option = "magma") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-sz, sz), ylim = c(-sz, sz)) + 
  theme(plot.background = element_rect(fill = "black"))

ggsave("~/Desktop/tangle2.png", pic, width = 10, height = 10)
