library(ggplot2)

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
points <- heart(r = 5)
kernel <- function(p, points, sd = .1) {
  px <- dnorm(points$x, mean = p[1], sd = sd)
  py <- dnorm(points$y, mean = p[2], sd = sd)
  return(sum(px * py))
}
propose <- function(p, sd = .1) {
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


its <- 50000
ind <- sample(80, 1)
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
dat <- dplyr::sample_frac(dat, .5)

bg <- "black"
pic <- ggplot(dat, aes(x, y, colour = shade)) + 
  geom_segment(aes(xend = xend, yend = yend), 
               alpha = .9, size = .3, show.legend = FALSE) + 
  # geom_point(data = points, size = 3, colour = bg) +
  scico::scale_color_scico(palette = "grayC") +
  theme_void() + 
  coord_equal() + 
  theme(plot.background = element_rect(fill = bg, colour = bg))

ggsave(
  filename = here::here("image", "tangle4.png"),
  bg = bg,
  plot = pic,
  width = 5000/300,
  height = 5000/300,
  dpi = 300
)
