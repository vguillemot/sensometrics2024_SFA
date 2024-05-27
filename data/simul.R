library(MASS)
library(dplyr)
library(gganimate)
library(ggplot2)

B <- toeplitz(c(1, rep(0.8, 3)))
sig <- Matrix::bdiag(B, B)

set.seed(6843513)
x1 <- mvrnorm(10, rep(c(1, 2), c(4, 4)), sig)
x2 <- mvrnorm(10, rep(c(0, -1), c(4, 4)), sig)
x3 <- mvrnorm(10, rep(c(-1, 1), c(4, 4)), sig)

x <- rbind(x1, x2, x3)
dimnames(x) <- list(
  sprintf(
    "%s%i",
    rep(c("A", "B", "C"), 
        c(10, 10, 10)),
    rep(1:10, 3)),
  sprintf("X%i", 1:8))

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
