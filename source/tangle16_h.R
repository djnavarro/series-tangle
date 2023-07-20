library(jasmines)
library(dplyr)
library(ggplot2)

scl <- .005

blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, 
           green = z[2, ]/255, 
           blue = z[3, ]/255)
  return(z)
} 
make_palette <- function(pin = "black", mix = "black") {
  base <- sample(colours(distinct=TRUE), 5)
  base[1] <- pin
  blends <- purrr::map_chr(base, ~blend(.x, mix, .5))
  colorRampPalette(blends)
}


radians <- function(n) {
  seq(0, 2 * pi, length.out = n + 1)[1:n]
}
heart <- function(n = 100, r = 1) {
  th <- radians(n)
  x <- (16 * sin(th)^3)/17
  y <- (13 * cos(th) - 5 * cos(2 * th) - 2 * cos(3 * th) - 
          cos(4 * th))/17
  x <- x - mean(x)
  y <- y - mean(y)
  tibble::tibble(x = x * r, y = y * r)
}

points <- use_seed(14) %>% 
  scene_bubbles(n = 5, grain = 500) %>% 
  unfold_warp(iterations = 1, scale = .5)

if(interactive()) ggplot(points, aes(x,y)) + geom_point(size = .1)


set.seed(9)
pal <- sample(colours(), 4)

kernel <- function(p, points, sd = scl) {
  px <- dnorm(points$x, mean = p[1], sd = sd)
  py <- dnorm(points$y, mean = p[2], sd = sd)
  return(sum(px * py))
}
propose <- function(p, sd = scl * 2) {
  p + rnorm(n = 2, mean = 0, sd = sd)
}
step <- function(p, i) {
  p_new <- propose(p)
  l_old <- kernel(p, points)
  l_new <- kernel(p_new, points)
  if(runif(1) < (l_new / l_old)) { p <- p_new }
  return(p)
}
smooth <- function(x, n = 20) {
  while(n > 0) {
    x <- (dplyr::lag(x) + x + dplyr::lead(x))/3
    n <- n - 1
  }
  return(x[!is.na(x)])
}


its <- 100000
ind <- sample(nrow(points), 1)
p0 <- c(points$x[ind], points$y[ind])
dat <- purrr::accumulate(1:its, step, .init = p0) 
dat <- purrr::map_dfr(dat, ~ tibble::tibble(x = .x[1], y = .x[2]))
dat <- tibble::tibble(
  x = smooth(dat$x),
  y = smooth(dat$y)
)
dat <- dplyr::mutate(dat, 
                     xend = dplyr::lag(x), 
                     yend = dplyr::lag(y), 
                     shade = dplyr::row_number())
dat <- dplyr::slice(dat, -1)
dat <- dplyr::sample_frac(dat, .4)


nb <- 200
lmb <- 10
brks <- seq(
  from = min(c(dat$x, dat$y)),
  to = max(c(dat$x, dat$y)),
  length.out = nb
)
grid <- expand.grid(x = brks, y = brks)
for(i in 1:nrow(grid)) {
  grid$shade[i] <- sum(dat$shade * exp(-lmb*abs(grid$x[i] - dat$x) - lmb*abs(grid$y[i] - dat$y))) / 
    sum(exp(-lmb*abs(grid$x[i] - dat$x) - lmb*abs(grid$y[i] - dat$y)))
}



bg <- "black"
pic <- ggplot(dat, aes(x, y, colour = shade)) + 
  geom_raster(data = grid, aes(fill = shade), alpha = .2, show.legend = FALSE) +
  geom_segment(aes(xend = xend, yend = yend), 
               alpha = .9, size = .3, show.legend = FALSE) + 
  # geom_point(data = points, size = 3, colour = bg) +
  #paletteer::scale_color_paletteer_c("grDevices::Oranges") +
  scale_color_gradientn(colours = pal) +
  scale_fill_gradientn(colours = pal) +
  theme_void() + 
  coord_equal() + 
  theme(plot.background = element_rect(fill = bg, colour = bg)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  NULL


ggsave(
  filename = here::here("image", "tangle16_h.png"),
  bg = bg,
  plot = pic,
  width = 5000/300,
  height = 5000/300,
  dpi = 300
)
