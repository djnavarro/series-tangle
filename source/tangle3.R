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
  tibble(x = x * r, y = y * r)
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

walk <- function(from, to, n, smooth, scale) {
  signal <- seq(from, to, length.out = n)
  noise <- c(0, rbridge(1, n - 3)) * scale
  while(smooth > 0) {
    noise <- (lag(noise, default = 0) + noise + lead(noise, default = 0))/3
    smooth <- smooth - 1
  }
  noise <- c(0, noise, 0)
  signal + noise
}

path <- function(data, n, smooth, scale) {
  tibble(
    id = data$id,
    time = 1:n,
    x = walk(data$x, data$xend, n, smooth, scale),
    y = walk(data$y, data$yend, n, smooth, scale)
  )
}

npoints <- 200
nsteps <- 100
heart_size <- 5
sz <- 6

dat <- heart(npoints, heart_size) %>%
  mutate(id = row_number()) %>%
  sample_frac(.5) %>% 
  arrange(id) %>% 
  mutate(
    xend = lag(x), 
    yend = lag(y)
  ) %>%
  slice(-1) %>%
  #sample_frac(.5) %>% 
  transpose() %>%
  map_dfr(~path(.x, nsteps, smooth = 50, scale = 1)) %>%
  mutate(
    #shade = abs(2*id/npoints - 1) + sin(10*time/nsteps)/2
    shade = sin(10*time/nsteps)/2
    #shade = id
  )


pic <- ggplot(dat, aes(x,y, group = id, color = shade)) +
  geom_path(show.legend = FALSE, size = 1, alpha = 1, lineend = "round") +
  geom_point(data = dat %>% filter(time == 1), color = "black", size = 3) +
  theme_void() +
  scale_color_scico(palette = "vik") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(-sz, sz), ylim = c(-sz, sz)) +
  theme(plot.background = element_rect(fill = "steelblue4"))

ggsave("~/Desktop/tangle3.png", pic, width = 10, height = 10)
