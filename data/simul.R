library(MASS)
library(dplyr)
library(gganimate)
library(ggplot2)
library(tidyr)

B <- toeplitz(c(1, rep(0.8, 3)))
sig <- Matrix::bdiag(B, B)

set.seed(6843513)
x1 <- mvrnorm(10, rep(c(1, 2), c(4, 4)), sig)
x2 <- mvrnorm(10, rep(c(0, -1), c(4, 4)), sig)
x3 <- mvrnorm(10, rep(c(-1, 1), c(4, 4)), sig)

x <- rbind(x1, x2, x3)
dimnames(x) <- list(
  sprintf(
    "%s%02i",
    rep(c("A", "B", "C"), 
        c(10, 10, 10)),
    rep(1:10, 3)),
  sprintf("X%i", 1:8))

writexl::write_xlsx(as_tibble(x, rownames = "Ind"), "simul.xlsx")

a <- colnames(x)
cov.ex <- cov(x)
res.eig.ex <- eigen(cov.ex)
lambda1 <- res.eig.ex$values[1]
w1 <- res.eig.ex$vectors[,1]
rank1 <- lambda1 * w1 %*% t(w1)
dimnames(rank1) <- dimnames(cov.ex)

before <- as_tibble(cov.ex, rownames = "row") %>%
  pivot_longer(-1, names_to = "col") %>%
  mutate(row = factor(row, levels = rev(a))) %>%
  ggplot(aes(col, row, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal() + 
  labs(title = "Covariance matrix")

after <- as_tibble(rank1, rownames = "row") %>%
  pivot_longer(-1, names_to = "col") %>%
  mutate(row = factor(row, levels = rev(a))) %>%
  ggplot(aes(col, row, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal()+ 
  labs(title = "Rank 1 approximation")

ggpubr::ggarrange(before, after, ncol = 2, legend = "none")

rank_k <- function(k) {
  lambdak <- res.eig.ex$values[k]
  wk <- res.eig.ex$vectors[,k]
  rankk <- lambdak * wk %*% t(wk)
  dimnames(rankk) <- dimnames(cov.ex)
  rankk_melted <- as_tibble(rankk, rownames = "row") %>%
    pivot_longer(-1, names_to = "col") %>%
    mutate(
      row = factor(row, levels = rev(a)),
      k = k)
  return(rankk_melted)
}

all_rank1 <- Reduce("rbind", lapply(1:8, rank_k))

anim1 <- all_rank1 %>%
  ggplot(aes(col, row, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal() + 
  # facet_wrap(~k) 
  # Here comes the gganimate code
  transition_states(
    k,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Dimension {closest_state}')

anim_save(filename = "anim1.gif", animation = anim1, device = "png")

inc_rank <- all_rank1 %>%
  group_by(row, col) %>%
  mutate(sum = cumsum(value)) %>% 
  ungroup()

anim2 <- inc_rank %>%
  ggplot(aes(col, row, fill = sum)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal() + 
  # facet_wrap(~k) 
  # Here comes the gganimate code
  transition_states(
    k,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Rank-{closest_state} approximation')

anim_save(filename = "anim2.gif", animation = anim2, device = "png")

res.svd.ex <- svd(x)
b <- rev(rownames(x))
rank1svd <- function(k) {
  deltak <- res.svd.ex$d[k]
  uk <- res.svd.ex$u[,k]
  wk <- res.svd.ex$v[,k]
  rankk <- deltak * uk %*% t(wk)
  dimnames(rankk) <- dimnames(x)
  rankk_melted <- as_tibble(rankk, rownames = "ind") %>%
    pivot_longer(-1, names_to = "var") %>%
    mutate(
      ind = factor(ind, levels = b),
      k = k)
  return(rankk_melted)
}


gdat <- as_tibble(x, rownames = "ind") %>%
  pivot_longer(-1, names_to = "var") %>%
  mutate(
    ind = factor(ind, levels = b)) %>%
  ggplot(aes(var, ind, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal()


gr1approx <- rank1svd(1) %>%
  ggplot(aes(var, ind, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal()


ggpubr::ggarrange(gdat, gr1approx, ncol = 2, legend = "none")

all_rank1_svd <- Reduce("rbind", lapply(1:7, rank1svd))

anim3 <- all_rank1_svd %>%
  ggplot(aes(var, ind, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_b() + 
  scale_x_discrete(position = "top") +
  coord_equal() + 
  # facet_wrap(~k) 
  # Here comes the gganimate code
  transition_states(
    k,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Dimension {closest_state}')

anim_save(filename = "anim3.gif", animation = anim3, device = "png")

inc_rank_svd <- all_rank1_svd %>%
  group_by(ind, var) %>%
  mutate(sum = cumsum(value)) %>% 
  ungroup()

anim4 <- inc_rank_svd %>%
  ggplot(aes(var, ind, fill = sum)) +
  geom_tile(color = "white") + 
  scale_fill_viridis_c() + 
  scale_x_discrete(position = "top") +
  coord_equal() + 
  # facet_wrap(~k) 
  # Here comes the gganimate code
  transition_states(
    k,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Rank-{closest_state} approximation')

anim_save(filename = "anim4.gif", animation = anim4, device = "png")