## ── libraries ───────────────────────────────────────────────────────
library(tidyverse) # dplyr, ggplot2, tidyr, etc.
library(DBI)
library(RPostgres)
library(GGally) # pair plots
library(corrplot) # correlation heat‑map
library(cluster) # silhouette, clusGap
library(dbscan) # density clustering
library(factoextra) # PCA & clustering visuals
library(ggcorrplot)

## ── data pull ───────────────────────────────────────────────────────
con <- dbConnect(
  Postgres(),
  dbname   = "postgres",
  host     = "aws-0-eu-west-2.pooler.supabase.com",
  user     = "pgstudent.rvdwflidqvcvffdccwrh",
  password = "0%jkXK^tjMZwuG",
  port     = 5432
)
oil_df <- tbl(con, "olive_oil") %>%
  collect() %>%
  select(-id)
dbDisconnect(con)

## ── initial EDA ─────────────────────────────────────────────────────
summary(oil_df)

# 1. Distribution per fatty acid
oil_df %>%
  pivot_longer(everything(), names_to = "acid") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~acid, scales = "free") +
  theme_minimal()

# 2. Correlation matrix
ggcorrplot(cor(oil_df), # or "square", "lower", etc.
  type = "upper", # show only upper triangle
  lab = TRUE, # add correlation labels
  title = "Correlation Matrix of Fatty Acids",
  tl.cex = 10, # text size of labels
  tl.col = "black", # label color
  ggtheme = theme_minimal()
)

# 3. Pairwise scatterplots
ggpairs(oil_df, progress = T)

?ggpairs
# 4. Boxplots (quick outlier scan)
oil_df %>%
  pivot_longer(everything(), names_to = "acid") %>%
  ggplot(aes(x = acid, y = value)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()

## ── scaling + PCA (retain first 4 PCs ≈90 % var) ───────────────────
pcs <- prcomp(scale(oil_df), center = TRUE)
pc_tb <- as_tibble(pcs$x[, 1:5])

# Scree plot
fviz_eig(pcs, addlabels = TRUE)

# PCA biplot
fviz_pca_biplot(pcs, repel = F)

## ── k‑means (choose k by silhouette) ────────────────────────────────
set.seed(42)
k_seq <- 2:8
kmfits <- map(k_seq, ~ kmeans(pc_tb, centers = .x, nstart = 25))
k_sil <- map_dbl(kmfits, ~ mean(silhouette(.x$cluster, dist(pc_tb))[, 3]))
best_k <- k_seq[which.max(k_sil)]
km_ok <- kmfits[[which.max(k_sil)]]

# Prepare data frame with k and corresponding silhouette scores
sil_df <- data.frame(
  k = k_seq,
  silhouette = k_sil
)

# Plot silhouette scores vs. number of clusters
ggplot(sil_df, aes(x = k, y = silhouette)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "red", size = 1) +
  annotate("text",
    x = best_k, y = max(sil_df$silhouette),
    label = paste("Best k =", best_k), vjust = 1, color = "red", size = 5, hjust = -0.2
  ) +
  labs(
    title = "Choosing Best Number of Clusters by Silhouette Score",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Score"
  ) +
  scale_x_continuous(breaks = k_seq, limits = range(k_seq)) +
  theme_minimal()

# Cluster plot
pc_tb <- pc_tb %>%
  mutate(km = factor(km_ok$cluster))
pc_tb %>%
  mutate(km = factor(km_ok$cluster)) %>%
  ggplot(aes(PC1, PC2, colour = km)) +
  geom_point(size = 2, alpha = .7) +
  labs(title = paste("k-means (k =", best_k, ")")) +
  theme_minimal()

## ── DBSCAN ─────────────────────────────────────────────────────────
library(dbscan)
pc_tb <- as_tibble(pcs$x[, 1:5])
minPts <- 5
kNNdistplot(pc_tb, k = minPts)
abline(h = 1.2, col = "red", lty = 2)
title(main = "kNN Distance Plot for Choosing eps")


db_mod <- dbscan(pc_tb, eps = 1.2, minPts = 5)
pc_tb <- pc_tb %>% mutate(db = factor(db_mod$cluster + 1))
ggplot(pc_tb, aes(PC1, PC2, colour = db)) +
  geom_point(size = 2, alpha = .7) +
  labs(title = "DBSCAN (eps = 1.2, minPts = 5)") +
  theme_minimal()

## ── hierarchical (Ward) ────────────────────────────────────────────
hc_tree <- hclust(dist(pc_tb[, 1:5]), method = "ward.D2")
hc_grp <- cutree(hc_tree, k = best_k)
fviz_dend(hc_tree, k = best_k, show_labels = FALSE, rect_fill = TRUE, k_colors = "jco")

pc_tb %>%
  mutate(hc = factor(hc_grp)) %>%
  ggplot(aes(PC1, PC2, colour = hc)) +
  geom_point(size = 2, alpha = .7) +
  labs(title = "Hierarchical clusters") +
  theme_minimal()

## ── quality summary ────────────────────────────────────────────────
db_sil <- mean(silhouette(db_mod$cluster, dist(pc_tb[, 1:5]))[, 3])
hc_sil <- mean(silhouette(hc_grp, dist(pc_tb[, 1:5]))[, 3])

tibble(
  method = c("k‑means", "DBSCAN", "hierarchical"),
  clusters = c(
    best_k,
    n_distinct(db_mod$cluster),
    n_distinct(hc_grp)
  ),
  avg_sil = c(max(k_sil), db_sil, hc_sil)
)
