# Make sur to put you're own file path at lign 86,306,435
# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggrepel)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(colorspace)

# Function to normalize RGB values
normalize_rgb <- function(color_str) {
  rgb_values <- as.numeric(strsplit(gsub("[()]", "", color_str), ",")[[1]])
  return(rgb_values / 255.0)
}

# Function to convert RGB to HLS
rgb_to_hls <- function(r, g, b) {
  max_val <- max(r, g, b)
  min_val <- min(r, g, b)
  l <- (max_val + min_val) / 2
  
  if (max_val == min_val) {
    h <- 0
    s <- 0
  } else {
    d <- max_val - min_val
    s <- if (l > 0.5) d / (2 - max_val - min_val) else d / (max_val + min_val)
    
    h <- switch(which(c(r, g, b) == max_val)[1],
                (g - b) / d + (if (g < b) 6 else 0),
                (b - r) / d + 2,
                (r - g) / d + 4)
    h <- h / 6
  }
  
  return(c(h = h, l = l, s = s))
}

# Function to classify skin tones based on lightness
classify_skin_tone <- function(r_norm, g_norm, b_norm) {
  hls <- rgb_to_hls(r_norm, g_norm, b_norm)
  l <- hls["l"]
  
  tone <- case_when(
    l >= 0.75 ~ "Fair",
    l >= 0.6 ~ "Light",
    l >= 0.45 ~ "Medium",
    TRUE ~ "Tan"
  )
  
  return(tone)
}

# Function to classify colors into seasons
classify_color_season <- function(r_norm, g_norm, b_norm) {
  hls <- rgb_to_hls(r_norm, g_norm, b_norm)
  h <- hls["h"]
  l <- hls["l"]
  
  # Determine temperature
  temperature <- if ((h >= 0 && h < 0.30) || (h >= 0.80 && h <= 1)) "Warm" else "Cool"
  
  # Determine value
  value <- case_when(
    l >= 0.55 ~ "Light",
    l <= 0.35 ~ "Dark",
    TRUE ~ "Medium"
  )
  
  # Assign season
  season <- case_when(
    temperature == "Warm" && value == "Light" ~ "Spring",
    temperature == "Cool" && value == "Light" ~ "Summer",
    temperature == "Warm" && value %in% c("Medium", "Dark") ~ "Autumn",
    temperature == "Cool" && value %in% c("Medium", "Dark") ~ "Winter",
    TRUE ~ "Unknown"
  )
  
  return(season)
}

# Load data
df <- read.csv('datasets/Profile_of_Body_Metrics_and_Fashion_Colors.csv', sep = ';')

# Process colors and create normalized values
process_colors <- function(df, col_name) {
  rgb_matrix <- t(sapply(df[[col_name]], normalize_rgb))
  colnames(rgb_matrix) <- c("R", "G", "B")
  return(rgb_matrix)
}

# Process all color columns
skin_rgb <- process_colors(df, "Skin.Color")
clothes_rgb <- process_colors(df, "Clothes.Color")
pants_rgb <- process_colors(df, "Pants.Color")

# Add normalized values to dataframe
df <- df %>%
  mutate(
    # Skin colors
    Skin_R_norm = skin_rgb[,"R"],
    Skin_G_norm = skin_rgb[,"G"],
    Skin_B_norm = skin_rgb[,"B"],
    
    # Clothes colors
    Clothes_R_norm = clothes_rgb[,"R"],
    Clothes_G_norm = clothes_rgb[,"G"],
    Clothes_B_norm = clothes_rgb[,"B"],
    
    # Pants colors
    Pants_R_norm = pants_rgb[,"R"],
    Pants_G_norm = pants_rgb[,"G"],
    Pants_B_norm = pants_rgb[,"B"]
  )

# Classify colors
df <- df %>%
  mutate(
    Skin_Tone = mapply(classify_skin_tone, 
                       Skin_R_norm, Skin_G_norm, Skin_B_norm),
    Clothes_Season = mapply(classify_color_season, 
                            Clothes_R_norm, Clothes_G_norm, Clothes_B_norm),
    Pants_Season = mapply(classify_color_season, 
                          Pants_R_norm, Pants_G_norm, Pants_B_norm)
  )

# Create color visualization plots
plot_color_space <- function(data, x_col, y_col, z_col, color_col, title) {
  ggplot(data, aes_string(x = x_col, y = y_col, color = color_col)) +
    geom_point(alpha = 0.6) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    ggtitle(title)
}

# Create visualization plots
p1 <- plot_color_space(df, "Skin_R_norm", "Skin_G_norm", "Skin_B_norm", 
                       "Skin_Tone", "Skin Colors by Tone")
p2 <- plot_color_space(df, "Clothes_R_norm", "Clothes_G_norm", "Clothes_B_norm", 
                       "Clothes_Season", "Clothes Colors by Season")
p3 <- plot_color_space(df, "Pants_R_norm", "Pants_G_norm", "Pants_B_norm", 
                       "Pants_Season", "Pants Colors by Season")

# Print distribution summaries
print("Distribution of Skin Tones:")
print(table(df$Skin_Tone))

print("\nDistribution of Clothes Seasons:")
print(table(df$Clothes_Season))

print("\nDistribution of Pants Seasons:")
print(table(df$Pants_Season))
# Function to create bar plots
create_bar_plot <- function(data, category, title) {
  data %>%
    group_by(!!sym(category)) %>%
    summarise(Count = n()) %>%
    ggplot(aes(x = reorder(!!sym(category), -Count), y = Count, fill = !!sym(category))) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = title,
      x = category,
      y = "Count",
      fill = category
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Bar plot for Skin Tone
skin_tone_plot <- create_bar_plot(df, "Skin_Tone", "Distribution of Skin Tones")
print(skin_tone_plot)
ggsave("skin_tone_distribution.png", skin_tone_plot, width = 8, height = 6)

# Bar plot for Clothes Season
clothes_season_plot <- create_bar_plot(df, "Clothes_Season", "Distribution of Clothes Seasons")
print(clothes_season_plot)
ggsave("clothes_season_distribution.png", clothes_season_plot, width = 8, height = 6)

# Bar plot for Pants Season
pants_season_plot <- create_bar_plot(df, "Pants_Season", "Distribution of Pants Seasons")
print(pants_season_plot)
ggsave("pants_season_distribution.png", pants_season_plot, width = 8, height = 6)


# Create color harmony scoring matrix based on color theory
color_harmony_scores <- matrix(
  c(0.9, 0.8, 0.4, 0.3,  # Fair skin scores
    0.9, 0.8, 0.5, 0.4,  # Light skin scores
    0.5, 0.9, 0.8, 0.4,  # Medium skin scores
    0.4, 0.5, 0.9, 0.8), # Tan skin scores
  nrow = 4, byrow = TRUE,
  dimnames = list(
    c("Fair", "Light", "Medium", "Tan"),  # Skin tones
    c("Summer", "Spring", "Autumn", "Winter")  # Seasons
  )
)

# Apply harmony scores to dataset
balanced_df <- balanced_df %>%
  mutate(
    Color_Harmony_Score = mapply(
      function(skin, clothes) {
        color_harmony_scores[skin, clothes]
      },
      Skin_Tone,
      Clothes_Season
    ),
    Harmony_Level = case_when(
      Color_Harmony_Score >= 0.8 ~ "High",
      Color_Harmony_Score >= 0.5 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Print harmony analysis
print("Color Harmony Analysis:")
print("Average harmony score by skin tone:")
print(tapply(balanced_df$Color_Harmony_Score, balanced_df$Skin_Tone, mean))
print("\nDistribution of harmony levels:")
print(table(balanced_df$Harmony_Level))

# Simplified Harmony Plot
harmony_plot <- ggplot(balanced_df, aes(x = Skin_Tone, y = Clothes_Season)) +
  geom_tile(color = "black", fill = "white") +  # No gradient, just a basic grid
  geom_text(aes(label = round(Color_Harmony_Score, 2)), size = 4) +  # Add harmony scores as text
  theme_minimal() +
  ggtitle("Color Harmony Scores by Skin Tone and Clothing and Pants Season") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centered and larger title
    axis.title.x = element_blank(),  # No axis titles
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis text for readability
  )

print(harmony_plot)


# Function to perform MCA and plot dimension distributions
perform_mca <- function(data, vars, title, file_prefix) {
  # Select relevant columns for MCA
  mca_data <- data %>% select(all_of(vars)) %>% mutate(across(everything(), as.factor))
  
  # Perform MCA
  mca_result <- MCA(mca_data, graph = FALSE)
  
  # Plot MCA variable categories
  var_plot <- fviz_mca_var(mca_result, repel = TRUE, ggtheme = theme_minimal()) +
    ggtitle(paste("MCA -", title))
  print(var_plot)
  ggsave(paste0(file_prefix, "_variables.png"), var_plot, width = 8, height = 6)
  
  # Plot MCA dimension distribution
  dim_plot <- fviz_eig(mca_result, addlabels = TRUE, ylim = c(0, 50)) +
    ggtitle(paste("MCA Dimension Distribution -", title))
  print(dim_plot)
  ggsave(paste0(file_prefix, "_dimensions.png"), dim_plot, width = 8, height = 6)
  
  # Return MCA result
  return(mca_result)
}

# MCA with Clothes Harmony Skin
mca_clothes_harmony_skin <- perform_mca(
  balanced_df,
  vars = c("Clothes_Season", "Skin_Tone", "Harmony_Level"),
  title = "Clothes, Harmony, and Skin",
  file_prefix = "mca_clothes_harmony_skin"
)

# MCA with Clothes, Pants, Harmony, Skin
mca_clothes_pants_harmony_skin <- perform_mca(
  balanced_df,
  vars = c("Clothes_Season", "Pants_Season", "Skin_Tone", "Harmony_Level"),
  title = "Clothes, Pants, Harmony, and Skin",
  file_prefix = "mca_clothes_pants_harmony_skin"
)

# MCA with BMI, Gender, Clothes, Pants, Harmony, Skin
mca_bmi_gender_clothes_pants_harmony_skin <- perform_mca(
  balanced_df,
  vars = c("BMI", "Gender", "Clothes_Season", "Pants_Season", "Skin_Tone", "Harmony_Level"),
  title = "BMI, Gender, Clothes, Pants, Harmony, and Skin",
  file_prefix = "mca_bmi_gender_clothes_pants_harmony_skin"
)


ggsave("harmony_skin_clothes_pants_season.png", harmony_plot, width = 8, height = 6)
ggsave("mca_clothes_harmony_skin_variables.png", fviz_mca_var(mca_clothes_harmony_skin, repel = TRUE, ggtheme = theme_minimal()), width = 8, height = 6)
ggsave("mca_clothes_harmony_skin_dimensions.png", fviz_eig(mca_clothes_harmony_skin, addlabels = TRUE, ylim = c(0, 50)), width = 8, height = 6)
ggsave("mca_clothes_pants_harmony_skin_variables.png", fviz_mca_var(mca_clothes_pants_harmony_skin, repel = TRUE, ggtheme = theme_minimal()), width = 8, height = 6)
ggsave("mca_clothes_pants_harmony_skin_dimensions.png", fviz_eig(mca_clothes_pants_harmony_skin, addlabels = TRUE, ylim = c(0, 50)), width = 8, height = 6)
ggsave("harmony_skin_clothes_pants_season.png", harmony_plot, width = 8, height = 6)
ggsave("mca_bmi_gender_clothes_pants_harmony_skin_variables.png", fviz_mca_var(mca_bmi_gender_clothes_pants_harmony_skin, repel = TRUE, ggtheme = theme_minimal()), width = 8, height = 6)
ggsave("mca_bmi_gender_clothes_pants_harmony_skin_dimensions.png", fviz_eig(mca_bmi_gender_clothes_pants_harmony_skin, addlabels = TRUE, ylim = c(0, 50)), width = 8, height = 6)



# Read the dataset
music_data <- read.csv("datasets/muse_v3_modified.csv", nrows = 5000)

# Remove rows with NA values
music_data <- na.omit(music_data)

## Univariate Regression 1: Genre -> Emotions Mean
genre_model <- lm(emotions_mean ~ as.factor(genre_id), data = music_data)
genre_rmse <- sqrt(mean((music_data$emotions_mean - predict(genre_model, music_data))^2))

## Univariate Regression 2: Artist -> Emotions Mean
artist_model <- lm(emotions_mean ~ as.factor(artist_id), data = music_data)
artist_rmse <- sqrt(mean((music_data$emotions_mean - predict(artist_model, music_data))^2))

## Multivariate Models
# Emotions Mean
multi_model_mean <- lm(emotions_mean ~ as.factor(genre_id) + as.factor(artist_id), data = music_data)
multi_rmse_mean <- sqrt(mean((music_data$emotions_mean - predict(multi_model_mean, music_data))^2))

# Arousal
multi_model_arousal <- lm(arousal_tags ~ as.factor(genre_id) + as.factor(artist_id), data = music_data)
multi_rmse_arousal <- sqrt(mean((music_data$arousal_tags - predict(multi_model_arousal, music_data))^2))

# Dominance
multi_model_dominance <- lm(dominance_tags ~ as.factor(genre_id) + as.factor(artist_id), data = music_data)
multi_rmse_dominance <- sqrt(mean((music_data$dominance_tags - predict(multi_model_dominance, music_data))^2))

# Valence
multi_model_valence <- lm(valence_tags ~ as.factor(genre_id) + as.factor(artist_id), data = music_data)
multi_rmse_valence <- sqrt(mean((music_data$valence_tags - predict(multi_model_valence, music_data))^2))

# Create results table
results_df <- data.frame(
  Model = c("Genre Only", "Artist Only", "Genre + Artist (Mean)", 
            "Genre + Artist (Arousal)", "Genre + Artist (Dominance)", 
            "Genre + Artist (Valence)"),
  R_squared = c(
    summary(genre_model)$r.squared,
    summary(artist_model)$r.squared,
    summary(multi_model_mean)$r.squared,
    summary(multi_model_arousal)$r.squared,
    summary(multi_model_dominance)$r.squared,
    summary(multi_model_valence)$r.squared
  ),
  RMSE = c(genre_rmse, artist_rmse, multi_rmse_mean, 
           multi_rmse_arousal, multi_rmse_dominance, multi_rmse_valence)
)

# Print results table
print(results_df)

# Create correlation matrix
cor_matrix <- cor(music_data[c("emotions_mean", "valence_tags", "arousal_tags", "dominance_tags")])

# Create heatmap
heatmap_plot <- ggplot(data = melt(cor_matrix), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap of Emotional Dimensions",
       x = "", y = "") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4)

# Save the heatmap
ggsave("emotional_correlations_heatmap.png", heatmap_plot, width = 10, height = 8, dpi = 300)

# Plot 1: QQ plots for residuals analysis
png("qq_plots_comparison.png", width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2), mar = c(5,5,4,2))

qqnorm(residuals(multi_model_mean), 
       main="QQ Plot - Emotions Mean Model",
       sub="Normal Q-Q Plot of Residuals")
qqline(residuals(multi_model_mean))

qqnorm(residuals(multi_model_arousal), 
       main="QQ Plot - Arousal Model",
       sub="Normal Q-Q Plot of Residuals")
qqline(residuals(multi_model_arousal))

qqnorm(residuals(multi_model_dominance), 
       main="QQ Plot - Dominance Model",
       sub="Normal Q-Q Plot of Residuals")
qqline(residuals(multi_model_dominance))

qqnorm(residuals(multi_model_valence), 
       main="QQ Plot - Valence Model",
       sub="Normal Q-Q Plot of Residuals")
qqline(residuals(multi_model_valence))

dev.off()


# Plot 2: Actual vs Predicted plots for multivariate models
# Create actual vs predicted plots
png("actual_vs_predicted_plots.png", width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2), mar = c(5,5,4,2))

plot(music_data$emotions_mean, predict(multi_model_mean),
     main="Actual vs Predicted: Emotions Mean",
     xlab="Actual Values", ylab="Predicted Values",
     pch=16, col=rgb(0,0,1,0.5))
abline(0,1, col="red", lwd=2)

plot(music_data$arousal_tags, predict(multi_model_arousal),
     main="Actual vs Predicted: Arousal",
     xlab="Actual Values", ylab="Predicted Values",
     pch=16, col=rgb(0,0,1,0.5))
abline(0,1, col="red", lwd=2)

plot(music_data$dominance_tags, predict(multi_model_dominance),
     main="Actual vs Predicted: Dominance",
     xlab="Actual Values", ylab="Predicted Values",
     pch=16, col=rgb(0,0,1,0.5))
abline(0,1, col="red", lwd=2)

plot(music_data$valence_tags, predict(multi_model_valence),
     main="Actual vs Predicted: Valence",
     xlab="Actual Values", ylab="Predicted Values",
     pch=16, col=rgb(0,0,1,0.5))
abline(0,1, col="red", lwd=2)

dev.off()

# Read the dataset

music_data <- read.csv("datasets/muse_v3.csv")

# Step 1: Filter for songs with exactly one emotion tag and create mean column
filtered_music <- music_data %>%
  # Keep only rows where number_of_emotion_tags equals 1
  filter(number_of_emotion_tags == 1) %>%
  # Create new column with mean of the three emotion dimensions
  mutate(emotion_tags_mean = (valence_tags + arousal_tags + dominance_tags) / 3)

# Step 2: Group by seeds to find unique emotion groups
emotion_groups <- filtered_music %>%
  # Group by the seeds column
  group_by(seeds) %>%
  # Count number of songs in each emotion group
  summarise(
    song_count = n(),
    # Calculate mean values for each emotion dimension within groups
    mean_valence = mean(valence_tags),
    mean_arousal = mean(arousal_tags),
    mean_dominance = mean(dominance_tags),
    mean_overall = mean(emotion_tags_mean)
  ) %>%
  # Sort by number of songs in descending order
  arrange(desc(song_count))

# Print summary statistics
cat("Summary of Analysis:\n")
cat("Number of songs with exactly one emotion tag:", nrow(filtered_music), "\n")
cat("Number of unique emotion groups:", nrow(emotion_groups), "\n")

# Display top 10 most common emotion tags
cat("\nTop 10 most common emotion tags and their counts:\n")
print(head(emotion_groups[, c("seeds", "song_count")], 10))

# Calculate average emotional dimensions for single-tag songs
emotion_averages <- filtered_music %>%
  summarise(
    avg_valence = mean(valence_tags),
    avg_arousal = mean(arousal_tags),
    avg_dominance = mean(dominance_tags),
    avg_overall = mean(emotion_tags_mean)
  )

cat("\nAverage emotional dimensions for single-tag songs:\n")
print(emotion_averages)

# Load additional required libraries for visualization
library(ggplot2)
library(gridExtra)

# Create a function to generate emotion dimension plots
# This makes it easier to adjust parameters and recreate plots
create_emotion_plots <- function(data, top_n_groups = 10) {
  # Clean the data and select top groups
  emotion_groups_clean <- data %>%
    mutate(seed_clean = gsub("\\[|\\]|\\'", "", seeds)) %>%
    arrange(desc(song_count)) %>%
    head(top_n_groups)
  
  # Create consistent theme elements
  plot_theme <- theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  
  # Create the three plots with consistent styling
  # Plot 1: Valence vs Arousal
  p1 <- ggplot(emotion_groups_clean, 
               aes(x = mean_arousal, y = mean_valence, 
                   size = song_count, color = seed_clean)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 15)) +
    plot_theme +
    labs(title = "Valence vs Arousal",
         x = "Arousal (Intensity)",
         y = "Valence (Pleasantness)",
         size = "Songs in Group",
         color = "Emotion Tag") +
    geom_text(aes(label = seed_clean), 
              vjust = -1.5, 
              size = 3,
              check_overlap = TRUE)
  
  # Plot 2: Arousal vs Dominance
  p2 <- ggplot(emotion_groups_clean, 
               aes(x = mean_dominance, y = mean_arousal, 
                   size = song_count, color = seed_clean)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 15)) +
    plot_theme +
    labs(title = "Arousal vs Dominance",
         x = "Dominance (Control)",
         y = "Arousal (Intensity)",
         size = "Songs in Group",
         color = "Emotion Tag") +
    geom_text(aes(label = seed_clean), 
              vjust = -1.5, 
              size = 3,
              check_overlap = TRUE)
  
  # Plot 3: Valence vs Dominance
  p3 <- ggplot(emotion_groups_clean, 
               aes(x = mean_dominance, y = mean_valence, 
                   size = song_count, color = seed_clean)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 15)) +
    plot_theme +
    labs(title = "Valence vs Dominance",
         x = "Dominance (Control)",
         y = "Valence (Pleasantness)",
         size = "Songs in Group",
         color = "Emotion Tag") +
    geom_text(aes(label = seed_clean), 
              vjust = -1.5, 
              size = 3,
              check_overlap = TRUE)
  
  # Return list of plots
  return(list(p1 = p1, p2 = p2, p3 = p3))
}

# Generate the plots using our function
emotion_plots <- create_emotion_plots(emotion_groups)

# Arrange and display all three plots
combined_plots <- grid.arrange(
  emotion_plots$p1, 
  emotion_plots$p2, 
  emotion_plots$p3, 
  ncol = 2,
  top = grid::textGrob(
    "Emotional Dimensions of Music Groups",
    gp = grid::gpar(fontsize = 14, fontface = "bold")
  )
)

# Print additional summary statistics for the visualized groups
cat("\nDetailed statistics for visualized emotion groups:\n")
emotion_groups %>%
  head(10) %>%  # This matches our default top_n_groups
  select(seeds, song_count, mean_valence, mean_arousal, mean_dominance) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  print(n = Inf)

# To visualize a different number of top groups, simply call:
# new_plots <- create_emotion_plots(emotion_groups, top_n_groups = 15)  # for example
# Save the visualization

ggsave(
  "emotion_dimensions_visualization.png", 
  combined_plots, 
  width = 15, 
  height = 10, 
  dpi = 300
)

# First, let's create a function for consistent plot styling
create_single_emotion_plot <- function(data, x_var, y_var, x_label, y_label, title, top_n_groups = 10) {
  # Clean the data and select top groups
  emotion_groups_clean <- data %>%
    mutate(seed_clean = gsub("\\[|\\]|\\'", "", seeds)) %>%
    arrange(desc(song_count)) %>%
    head(top_n_groups)
  
  # Create the plot with consistent styling
  ggplot(emotion_groups_clean, 
         aes(x = !!sym(x_var), y = !!sym(y_var), 
             size = song_count, color = seed_clean)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(5, 15)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 20)),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    labs(title = title,
         x = x_label,
         y = y_label,
         size = "Number of Songs",
         color = "Emotion Tag") +
    geom_text(aes(label = seed_clean), 
              vjust = -1, 
              size = 3.5,
              check_overlap = TRUE)
}

# Create the three separate plots
plot1 <- create_single_emotion_plot(
  emotion_groups,
  "mean_arousal", "mean_valence",
  "Arousal (Intensity)", "Valence (Pleasantness)",
  "Valence vs Arousal in Music Emotions"
)

plot2 <- create_single_emotion_plot(
  emotion_groups,
  "mean_dominance", "mean_arousal",
  "Dominance (Control)", "Arousal (Intensity)",
  "Arousal vs Dominance in Music Emotions"
)

plot3 <- create_single_emotion_plot(
  emotion_groups,
  "mean_dominance", "mean_valence",
  "Dominance (Control)", "Valence (Pleasantness)",
  "Valence vs Dominance in Music Emotions"
)

# Save each plot separately with better dimensions
ggsave("valence_arousal_plot.png", plot1, width = 12, height = 8, dpi = 300)
ggsave("arousal_dominance_plot.png", plot2, width = 12, height = 8, dpi = 300)
ggsave("valence_dominance_plot.png", plot3, width = 12, height = 8, dpi = 300)

# Display additional information about the visualization
cat("\nVisualization Summary:\n")
cat("Created three separate plots for better clarity:\n")
cat("1. Valence vs Arousal: Shows pleasantness against intensity\n")
cat("2. Arousal vs Dominance: Shows intensity against control\n")
cat("3. Valence vs Dominance: Shows pleasantness against control\n")
cat("\nEach plot includes:\n")
cat("- Point size indicating number of songs in each emotion group\n")
cat("- Clear labels for emotion tags\n")
cat("- Consistent scaling across all three visualizations\n")


# First, let's get the songs tagged as "sleazy" and "fierce" from our dataset
sleazy_songs <- music_data %>%
  filter(grepl("sleazy", seeds, fixed = TRUE)) %>%
  select(artist, track, seeds)

fierce_songs <- music_data %>%
  filter(grepl("fierce", seeds, fixed = TRUE)) %>%
  select(artist, track, seeds)

# Find artists who appear in both categories
sleazy_artists <- unique(sleazy_songs$artist)
fierce_artists <- unique(fierce_songs$artist)
overlapping_artists <- intersect(sleazy_artists, fierce_artists)

# Now create the analysis of these overlapping artists
overlap_analysis <- music_data %>%
  # Filter for artists who have both sleazy and fierce songs
  filter(artist %in% overlapping_artists) %>%
  filter(grepl("sleazy|fierce", seeds, fixed = FALSE)) %>%
  # Group by artist to get their average emotional values
  group_by(artist) %>%
  summarise(
    avg_valence = mean(valence_tags),
    avg_arousal = mean(arousal_tags),
    avg_dominance = mean(dominance_tags),
    total_songs = n(),
    # Create a combined string of unique tags
    emotion_tags = paste(unique(gsub("\\[|\\]|\\'", "", seeds)), collapse = ", ")
  ) %>%
  arrange(desc(total_songs))

# Create the visualization
emotional_space_plot <- ggplot(overlap_analysis, 
                               aes(x = avg_arousal, y = avg_valence, size = total_songs)) +
  # Add points
  geom_point(alpha = 0.6, color = "purple") +
  # Add labels with ggrepel to avoid overlap
  geom_text_repel(
    aes(label = artist),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    force = 10,
    max.overlaps = Inf
  ) +
  # Add a theme with white background
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "right"
  ) +
  # Add labels
  labs(
    title = "Artists with Both Sleazy and Fierce Songs",
    subtitle = "Positioned by average emotional values",
    x = "Arousal (Intensity)",
    y = "Valence (Pleasantness)",
    size = "Number of Songs"
  )

# Display the plot
print(emotional_space_plot)

# Save the plot
ggsave("artist_emotional_space.png", emotional_space_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Print information about these artists
cat("\nArtists with both Sleazy and Fierce songs:\n")
print(overlap_analysis %>% 
        select(artist, total_songs, emotion_tags, avg_valence, avg_arousal) %>%
        arrange(desc(total_songs)),
      n = Inf)

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total number of artists with both tags:", nrow(overlap_analysis), "\n")
cat("Average songs per artist:", round(mean(overlap_analysis$total_songs), 2), "\n")
cat("Average valence:", round(mean(overlap_analysis$avg_valence), 2), "\n")
cat("Average arousal:", round(mean(overlap_analysis$avg_arousal), 2), "\n")

# Save the detailed analysis
write.csv(overlap_analysis, "artist_emotional_analysis.csv", row.names = FALSE)