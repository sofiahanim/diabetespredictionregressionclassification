# Load necessary libraries
library(ggplot2)
library(readr)
library(corrplot)
library(GGally)  # For enhanced visualizations like pair plots

# Load cleaned data
data <- read_csv("./data/processed/cleaned_data.csv")

# Visualizing distributions of all numeric variables
numeric_cols <- names(data)[sapply(data, is.numeric)]
lapply(numeric_cols, function(col) {
  p <- ggplot(data, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    geom_density(alpha = 0.4, fill = "red") +
    labs(title = paste("Distribution and Density of", col), x = col, y = "Frequency/Density")
  ggsave(paste0("./outputs/images/histogram_", col, ".png"), plot = p, width = 10, height = 8)
})

# Generating boxplots for numerical variables to identify outliers
lapply(numeric_cols, function(col) {
  p <- ggplot(data, aes_string(x = "1", y = col)) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Boxplot of", col))
  ggsave(paste0("./outputs/images/boxplot_", col, ".png"), plot = p, width = 10, height = 8)
})

# Correlation matrix for numeric variables
cor_matrix <- cor(data[numeric_cols], use = "complete.obs")
corr_plot <- corrplot(cor_matrix, method = "color", order = "hclust", tl.col = "black", tl.srt = 45)
ggsave("./outputs/images/correlation_matrix.png", plot = corr_plot, width = 10, height = 8)

# Advanced visualization: Pair Plot for a deeper look into data interactions
pair_plot <- ggpairs(data[, c(numeric_cols, "Outcome")], aes(color = Outcome), lower = list(continuous = "smooth"))
ggsave("./outputs/images/scatterplot_matrix.png", plot = pair_plot, width = 12, height = 12)

# Summarizing the data for a comprehensive overview
summary_stats <- summary(data)
write.csv(summary_stats, "./outputs/summary_statistics.csv")
print(summary_stats)
