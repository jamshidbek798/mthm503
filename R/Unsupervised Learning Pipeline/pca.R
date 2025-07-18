unsup_pca <- function(oil_df) {
  pcs <- prcomp(scale(oil_df), center = TRUE)
  pc_tb <- as_tibble(pcs$x[, 1:5])
  return(list(pca = pcs, pca_df = pc_tb))
}
