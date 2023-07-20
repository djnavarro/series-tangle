library(jasmines)
library(dplyr)
library(ggplot2)

seed1 <- 18
seed2 <- 140:149

tangle17 <- function(seed1, seed2, bg = "black") {
  
  scl <- .005
  
  fname <- paste0("tangle17_", seed1, "_", seed2, ".png")
  
  set.seed(seed1)
  points <- use_seed(seed1) %>% 
    scene_bubbles(n = 5, grain = 500) %>% 
    unfold_warp(iterations = 1, scale = .5)
  
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
  
  make_data <- function(points) {
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
    return(dat)
  }
  
  make_grid <- function(dat) {
    
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
    
    return(grid)
  }
  
  set.seed(seed2)
  
  pal <- sample(colours(), 4)
  dat <- make_data(points)
  grd <- make_grid(dat)
  
  pic <- ggplot(dat, aes(x, y, colour = shade)) + 
    geom_raster(data = grd, aes(fill = shade), alpha = .2, show.legend = FALSE) +
    geom_segment(aes(xend = xend, yend = yend), 
                 alpha = .9, size = .3, show.legend = FALSE) + 
    scale_color_gradientn(colours = pal) +
    scale_fill_gradientn(colours = pal) +
    theme_void() + 
    coord_equal() + 
    theme(plot.background = element_rect(fill = bg, colour = bg)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    NULL
  
  ggsave(
    filename = here::here("image", fname),
    bg = bg,
    plot = pic,
    width = 5000/300,
    height = 5000/300,
    dpi = 300
  )
}


for(s in seed2) {
  cat(s, " ")
  tangle17(seed1, s, bg = "white")
}
