unsup_clustering <- function(pc_tb) {
  k_seq <- 2:8
  kmfits <- purrr::map(k_seq, ~ kmeans(pc_tb, centers = .x, nstart = 25))
  k_sil <- purrr::map_dbl(kmfits, ~ mean(cluster::silhouette(.x$cluster, dist(pc_tb))[, 3]))
  best_k <- k_seq[which.max(k_sil)]
  km_ok <- kmfits[[which.max(k_sil)]]

  db_mod <- dbscan::dbscan(pc_tb, eps = 1.2, minPts = 5)
  hc_tree <- hclust(dist(pc_tb), method = "ward.D2")
  hc_grp <- cutree(hc_tree, k = best_k)

  km_sil <- max(k_sil)
  db_sil <- mean(cluster::silhouette(db_mod$cluster, dist(pc_tb))[, 3])
  hc_sil <- mean(cluster::silhouette(hc_grp, dist(pc_tb))[, 3])

  tibble::tibble(
    method = c("k‑means", "DBSCAN", "hierarchical"),
    clusters = c(
      best_k,
      dplyr::n_distinct(db_mod$cluster),
      dplyr::n_distinct(hc_grp)
    ),
    avg_sil = c(km_sil, db_sil, hc_sil)
  )
}
