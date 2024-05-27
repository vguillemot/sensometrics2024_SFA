## Libraries
library(sGSVD)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

## Script to extract 30 respondants from the simulated "osiq" data
bigosiq <- as.matrix(data4PCCAR::mentalImageryOSIQ$OSIQ)
res.prcomp <- prcomp(bigosiq)

dat.bigpca <- tibble(
  resp = rownames(bigosiq),
  as_tibble(res.prcomp$x),
  group = substr(resp, 1, 1)
)

dat.bigpca %>%
  ggplot(aes(PC1, PC2, color = group)) + 
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = 0, color = "grey") + 
  geom_point() + 
  theme_bw()


biggroups <- substr(rownames(bigosiq), 1, 1)
set.seed(641)
osiq <-bigosiq[
  c(sample(which(biggroups == "L"), 10),
    sample(which(biggroups == "M"), 10),
    sample(which(biggroups == "H"), 10)), ]

I <- nrow(osiq)
J <- ncol(osiq)


## Test that it works with sGSVD
res <- sparseSVD(
  X = scale(osiq), ## the data 
  k = 2L, ## number of dimensions
  init = "svd",
  rdsLeft = 1 * sqrt(I) * rep(1, 4),
  rdsRight = 0.45 * sqrt(J) * rep(1, 4))

dat.u <- tibble(
  resp = rownames(osiq),
  as_tibble(res$u),
  group = substr(resp, 1, 1)
)

dat.v <- tibble(
  items = colnames(osiq),
  as_tibble(res$v),
  type = substr(items, 1, 1)
)

dat.u %>%
  ggplot(aes(V1, V2, color = group)) + 
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = 0, color = "grey") + 
  geom_point() + 
  theme_bw()

dat.v %>% filter(V1 != 0 | V2 != 0) %>%
  ggplot(aes(V1, V2, color = type)) + 
  geom_hline(yintercept = 0, color = "grey") + 
  geom_vline(xintercept = 0, color = "grey") + 
  geom_point() + 
  geom_text_repel(aes(label = items), max.overlaps = 30) + 
  theme_bw()

(lolli1 <- dat.v %>%
  ggplot(aes(reorder(items, V1*V2), V1, color = type)) + 
  geom_segment(aes(xend = items, yend = V1), y = 0) + 
  geom_text(aes(y = 0.04 * ((V1<0)-0.5), label = items)) + 
  geom_point() + 
  theme_minimal() + 
  theme(axis.text.x = element_blank()))

(lolli2 <- dat.v %>%
  ggplot(aes(reorder(items, V1*V2), V2, color = type)) + 
  geom_segment(aes(xend = items, yend = V2), y = 0) + 
  geom_text(aes(y = 0.04 * ((V2<0)-0.5), label = items)) + 
  geom_point() + 
  theme_minimal() + 
  theme(axis.text.x = element_blank()))



lolli1 / lolli2



