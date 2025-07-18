test_that("unsup_clustering() returns valid silhouette table", {
  # quick PCA dataframe
  set.seed(123)
  pc_df <- tibble::as_tibble(matrix(rnorm(100 * 5), ncol = 5))
  
  cl <- unsup_clustering(pc_df)
  
  # object structure
  expect_named(cl, c("km_model","db_model","hc_model",
                     "hc_groups","cluster_df","best_k"))
  # k-means model
  expect_s3_class(cl$km_model, "kmeans")
  # silhouette values between 0 and 1
  expect_true(all(dplyr::between(cl$cluster_df$avg_sil, 0, 1)))
  # best_k must be 2–8 (the range we search)
  expect_true(cl$best_k %in% 2:8)
}
)