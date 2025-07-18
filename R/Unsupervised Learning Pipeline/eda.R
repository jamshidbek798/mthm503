unsup_eda <- function(oil_df) {
  summary_stats <- summary(oil_df)

  p1 <- oil_df %>%
    pivot_longer(everything(), names_to = "acid") %>%
    ggplot(aes(value)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    facet_wrap(~acid, scales = "free") +
    theme_minimal()

  corr_plot <- ggcorrplot(cor(oil_df),
    type = "upper", lab = TRUE,
    title = "Correlation Matrix of Fatty Acids",
    tl.cex = 10, tl.col = "black",
    ggtheme = theme_minimal()
  )

  pair_plot <- GGally::ggpairs(oil_df, progress = TRUE)

  box_plot <- oil_df %>%
    pivot_longer(everything(), names_to = "acid") %>%
    ggplot(aes(x = acid, y = value)) +
    geom_boxplot() +
    coord_flip() +
    theme_minimal()

  list(summary = summary_stats, histograms = p1, corr = corr_plot, pairs = pair_plot, box = box_plot)
}
