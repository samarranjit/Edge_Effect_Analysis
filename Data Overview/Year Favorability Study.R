# -----------------------------
# Load Required Libraries
# -----------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(scales)  # for rescale()

# -----------------------------
# Load Your CSV
# -----------------------------
# Replace this path with your actual file path
df <- read_csv(results_path,"/Results/Top 3 Crops Overall Yield Analysis/Mean_Yield_by_Location_Year_Crop.csv")

# Expected columns: Year, Crop, Field_Name, Mean_Yield

# -----------------------------
# Compute Crop-Year Average Yield
# -----------------------------
crop_year_summary <- df %>%
  group_by(Crop, Year) %>%
  summarise(
    avg_yield = mean(Mean_Yield, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Compute Favorability Score (Z-score) & 0–100 Scale
# -----------------------------
crop_favorability <- crop_year_summary %>%
  group_by(Crop) %>%
  mutate(
    yield_mean = mean(avg_yield),
    yield_sd = sd(avg_yield),
    favorability_score = (avg_yield - yield_mean) / yield_sd,
    favorability_percent = rescale(favorability_score, to = c(0, 100))
  ) %>%
  ungroup()

# print(crop_favorability %>% filter(Crop == "WHEAT"))

View(crop_favorability)
# write.xlsx(crop_favorability, "C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Yearly Yield BreakPoint/Yearly Yield Analysis/crop_favorability_scores.xlsx")

# -----------------------------
# Identify Most/Least Favorable Years per Crop
# -----------------------------
favorable_summary <- crop_favorability %>%
  group_by(Crop) %>%
  summarise(
    best_year = Year[which.max(favorability_score)],
    best_score = max(favorability_score),
    worst_year = Year[which.min(favorability_score)],
    worst_score = min(favorability_score),
    .groups = "drop"
  )

print("📋 Favorability Summary per Crop:")
print(favorable_summary)

# -----------------------------
# Plot Favorability Score Over Time
# -----------------------------
ggplot(crop_favorability, aes(x = Year, y = favorability_score, color = Crop)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Crop, scales = "free_y") +
  labs(
    title = "Crop Yield Favorability Over Time",
    y = "Favorability Score (Z-Score)",
    x = "Year"
  ) +
  theme_minimal(base_size = 14)

# -----------------------------
# (Optional) Save Results
# -----------------------------
# write_csv(crop_favorability, "favorability_scores_by_crop_year.csv")
# write_csv(favorable_summary, "most_and_least_favorable_years.csv")
