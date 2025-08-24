#7/15/2025: Changing the path to the new location of the BreakPoints2.xlsx file inorder to evaluate the yield differences



library(ggplot2)
library(dplyr)
library(openxlsx)

# Load and filter data
# breakpoint_data <- read.xlsx("../Yearly Yield BreakPoint/BreakPoints.xlsx", sheet = 1)
breakpoint_data <- read.xlsx("Z:/temp/YearlyBreakpoints2/WithFieldAverage/BreakPoints2.xlsx", sheet = 1)
View(breakpoint_data)

breakpoint_data_clean <- breakpoint_data %>%
  filter(
    !is.na(BreakPoint),
    !is.na(Yield_Inside_BreakPoint),
    !is.na(Yield_Outside_BreakPoint),
    Crop %in% c("SOYBEANS","WHEAT",  "CORN"),
    BreakPoint < 50,
    BreakPoint >5,
    Breakpoint_strength < 5
  )
breakpoint_data_clean$Breakpoint_strength <- breakpoint_data_clean$Breakpoint_strength*100
# Ensure Year is a factor for clean categorical spacing
breakpoint_data_clean$Year <- as.factor(breakpoint_data_clean$Year)
# Define a dodge position so both boxplot and points align
dodge <- position_dodge(width = 0.8)

summary(breakpoint_data_clean)

ggplot(breakpoint_data_clean, aes(x = Year, y = BreakPoint, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
    alpha = 0.5, size = 0.55
  ) +
  scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(
    title = "Yearly Breakpoint Distribution by Crop (2014 - 2024)",
    x = "Year",
    y = "Breakpoint Distance (m)"
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
breakpoint_summary_table <- breakpoint_data_clean %>%
  group_by(Crop, Year) %>%
  summarise(
    Count = n(),
    Mean_BreakPoint = round(mean(BreakPoint, na.rm = TRUE), 2),
    Median_BreakPoint = round(median(BreakPoint, na.rm = TRUE), 2),
    SD_BreakPoint = round(sd(BreakPoint, na.rm = TRUE), 2),
    Min_BreakPoint = round(min(BreakPoint, na.rm = TRUE), 2),
    Max_BreakPoint = round(max(BreakPoint, na.rm = TRUE), 2)
  ) %>%
  arrange(Crop, Year)

View(breakpoint_summary_table)

# write.xlsx(breakpoint_summary_table, file = "Z:/temp/tempdata.xlsx", rownames = FALSE)
# write.xlsx(breakpoint_summary_table, file = "C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Yearly Yield BreakPoint/Yearly Yield Analysis/Yearly_Breakpoints_Distribution_By_Crop_Data.xlsx", rownames = FALSE)

#=========================================================================================
#Crop Specific:


breakpoint_data_clean_soybeans <- breakpoint_data_clean %>% filter(Crop == "SOYBEANS")
breakpoint_data_clean_corn <- breakpoint_data_clean %>% filter(Crop == "CORN")
breakpoint_data_clean_wheat <- breakpoint_data_clean %>% filter(Crop == "WHEAT")

breakpoint_data_clean_soybeans$Yield_Difference <- breakpoint_data_clean_soybeans$Yield_Inside_BreakPoint - breakpoint_data_clean_soybeans$Yield_Outside_BreakPoint
breakpoint_data_clean_corn$Yield_Difference <- breakpoint_data_clean_corn$Yield_Inside_BreakPoint - breakpoint_data_clean_corn$Yield_Outside_BreakPoint
breakpoint_data_clean_wheat$Yield_Difference <- breakpoint_data_clean_wheat$Yield_Inside_BreakPoint - breakpoint_data_clean_wheat$Yield_Outside_BreakPoint

breakpoint_data_clean_soybeans$yield_loss <- breakpoint_data_clean_soybeans$Yield_Difference / breakpoint_data_clean_soybeans$Yield_Inside_BreakPoint * 100
breakpoint_data_clean_corn$yield_loss <- breakpoint_data_clean_corn$Yield_Difference / breakpoint_data_clean_corn$Yield_Inside_BreakPoint * 100
breakpoint_data_clean_wheat$yield_loss <- breakpoint_data_clean_wheat$Yield_Difference / breakpoint_data_clean_wheat$Yield_Inside_BreakPoint * 100


BP_soybeans <- ggplot(breakpoint_data_clean_soybeans, aes(x = Year, y = BreakPoint)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge,fill = "skyblue") +
  geom_point(
    # position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
    alpha = 0.5, size = 0.55
  ) +
  scale_x_discrete(limits =soybeans_favorability_year_order) +
  
  # scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(
    title = "Soybeans",
    x = "Year",
    y = "Breakpoint Distance (m)"
  )+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
    # legend.title = element_text(size = 12),
    # legend.text = element_text(size = 11)
  )


ggplot(breakpoint_data_clean_wheat, aes(x = Year, y = yield_loss)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge, fill =  "lightyellow") +
  geom_point(
    # position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
    alpha = 0.5, size = 0.55
  ) +
  scale_x_discrete(limits =wheat_favorability_year_order) +
  
  # scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(
    title = "Yearly Yield Loss Inside vs Outside Breakpoint For Wheat (2014 - 2024)",
    x = "Year",
    y = "Yield Loss (% of Inner Yield)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "none"
    
  )

#----------------------------------------------------------------------------------------

# evaluating the yield difference

breakpoint_data_clean$Yield_Difference <- breakpoint_data_clean$Yield_Inside_BreakPoint - breakpoint_data_clean$Yield_Outside_BreakPoint
View(breakpoint_data_clean)

# evaluating yield loss
breakpoint_data_clean$yield_loss <- breakpoint_data_clean$Yield_Difference / breakpoint_data_clean$Yield_Inside_BreakPoint * 100

BPYieldLoss_plot_wheat <- ggplot(breakpoint_data_clean_wheat, aes(x = Year, y = yield_loss, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0, dodge.width = 0.8),
    alpha = 0.5, size = 0.55
  ) +
  scale_x_discrete(limits =wheat_favorability_year_order) +
  scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(
    title = "Wheat",
    x = "Year (Highest Yield -> Lowest yield)",
    y = "Yield Loss (% of Inner Yield)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "none"
  )
# Summary table for yield loss
breakpoint_yield_loss_summary <- breakpoint_data_clean %>%
  group_by(Crop, Year) %>%
  summarise(
    Count = n(),
    Mean_Yield_Loss = round(mean(yield_loss, na.rm = TRUE), 2),
    Median_Yield_Loss = round(median(yield_loss, na.rm = TRUE), 2),
    SD_Yield_Loss = round(sd(yield_loss, na.rm = TRUE), 2),
    Min_Yield_Loss = round(min(yield_loss, na.rm = TRUE), 2),
    Max_Yield_Loss = round(max(yield_loss, na.rm = TRUE), 2)
  ) %>%
  arrange(Crop, Year) %>% ungroup()
View(breakpoint_yield_loss_summary)
write.csv(breakpoint_yield_loss_summary, file = "Z:/temp/tempdata.csv", row.names = FALSE)

# summary table for yield difference
breakpoint_yield_difference_summary <- breakpoint_data_clean %>%
  group_by(Crop, Year) %>%
  summarise(
    Count = n(),
    Mean_Yield_Difference = round(mean(Yield_Difference, na.rm = TRUE), 2),
    Median_Yield_Difference = round(median(Yield_Difference, na.rm = TRUE), 2),
    SD_Yield_Difference = round(sd(Yield_Difference, na.rm = TRUE), 2),
    Min_Yield_Difference = round(min(Yield_Difference, na.rm = TRUE), 2),
    Max_Yield_Difference = round(max(Yield_Difference, na.rm = TRUE), 2)
  ) %>%
  arrange(Crop, Year) %>% ungroup()
View(breakpoint_yield_difference_summary)

write.csv(breakpoint_yield_difference_summary, file = "Z:/temp/tempdata.csv", row.names = FALSE)


# =====================================================================================
# Breakpoint Strength Analysis
# =====================================================================================
dodge <- position_dodge(width = .75)   # define once

wheat_favorability_year_order <- c(
  "2022",
  "2021",
  "2023",
  "2020",
  "2016",
  "2017",
  "2024",
  "2018",
  "2019",
  "2015"
  
)

corn_favorability_year_order <- c(
  "2023",
  "2022",
  "2021",
  "2020",
  "2016",
  "2014",
  "2017",
  "2019",
  "2015",
  "2018",
  "2024"
)

soybeans_favorability_year_order <- c(
  "2014",
  "2021",
  "2017",
  "2020",
  "2022",
  "2023",
  "2016",
  "2018",
  "2019",
  "2015",
  "2024"
  
)

strength_plot_corn <- ggplot(breakpoint_data_clean_corn, aes(x = factor(Year), y = Breakpoint_strength, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge) +
  geom_point(alpha = 0.6, position = dodge, size = 0.55) +
  scale_x_discrete(limits = corn_favorability_year_order) +
  # geom_smooth(aes(group = Crop), method = "lm", se = TRUE) +
  scale_fill_manual(values = c(SOYBEANS = "skyblue",CORN = "lightgreen", WHEAT     = "lightyellow")) +
  labs(title = "Corn",
       x = "Year (High Yield Year -> Low Yield Year)",
       y = "Breakpoint strength") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position  = "none"  # Hide legend
  )


strength_plot_soybeans <- ggplot(breakpoint_data_clean_soybeans, aes(x = factor(Year), y = Breakpoint_strength, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge) +
  geom_point(alpha = 0.6, position = dodge, size = 0.55) +
  scale_x_discrete(limits = soybeans_favorability_year_order) +
  # geom_smooth(aes(group = Crop), method = "lm", se = TRUE) +
  scale_fill_manual(values = c(SOYBEANS = "skyblue",CORN = "lightgreen", WHEAT     = "lightyellow")) +
  labs(title = "Soybeans",
       x = "Year (High Yield Year -> Low Yield Year)",
       y = "Breakpoint strength") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position  = "none"  # Hide legend
  )



strength_plot_wheat <- ggplot(breakpoint_data_clean_wheat, aes(x = factor(Year), y = Breakpoint_strength, fill = Crop)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, position = dodge) +
  geom_point(alpha = 0.6, position = dodge, size = 0.55) +
  scale_x_discrete(limits = wheat_favorability_year_order) +
  # geom_smooth(aes(group = Crop), method = "lm", se = TRUE) +
  scale_fill_manual(values = c(SOYBEANS = "skyblue",CORN = "lightgreen", WHEAT     = "lightyellow")) +
  labs(title = "Wheat",
       x = "Year (High Yield Year -> Low Yield Year)",
       y = "Breakpoint strength") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position  = "none"  # Hide legend
  )
combined_strength_plot <- (strength_plot_corn | strength_plot_soybeans | strength_plot_wheat )

ggsave("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Yearly Yield BreakPoint/Yearly Yield Analysis/Breakpoint_strength_plot_Corn_Ordered.pdf",device = pdf, strength_plot)


breakpoint_strength_summary <- breakpoint_data_clean %>%
  group_by(Crop, Year) %>%
  summarise(
    Count = n(),
    Mean_Breakpoint_strength = round(mean(Breakpoint_strength, na.rm = TRUE), 2),
    Median_Breakpoint_strength = round(median(Breakpoint_strength, na.rm = TRUE), 2),
    SD_Breakpoint_strength = round(sd(Breakpoint_strength, na.rm = TRUE), 2),
    Min_Breakpoint_strength = round(min(Breakpoint_strength, na.rm = TRUE), 2),
    Max_Breakpoint_strength = round(max(Breakpoint_strength, na.rm = TRUE), 2)
  ) %>%
  arrange(Crop, Year) %>% ungroup()
View(breakpoint_strength_summary)

write.xlsx(breakpoint_strength_summary, file = "Z:/temp/tempdata.xlsx", rownames = FALSE)
  

# =====================================================================================
# ADDITIONAL ADVANCED ANALYSES FOR RESEARCH PAPER
# =====================================================================================

# Required libraries for advanced analyses
library(corrplot)
library(car)
library(lme4)
library(emmeans)
library(broom)
library(tidyr)
library(purrr)  # Required for map_dbl function

# 1. TEMPORAL TREND ANALYSIS
# =====================================================================================
library(tidyverse)

# Convert Year to numeric
breakpoint_data_clean$Year_numeric <- as.numeric(as.character(breakpoint_data_clean$Year))

# Group by Crop, nest data, and fit models
breakpoint_trends <- breakpoint_data_clean %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(BreakPoint ~ Year_numeric, data = .x)),
    slope = map_dbl(model, ~ coef(.x)[["Year_numeric"]]),
    p_value = map_dbl(model, ~ summary(.x)$coefficients["Year_numeric", "Pr(>|t|)"]),
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    trend_direction = case_when(
      p_value < 0.05 & slope > 0 ~ "Increasing",
      p_value < 0.05 & slope < 0 ~ "Decreasing",
      TRUE ~ "No significant trend"
    )
  ) %>%
  select(Crop, slope, p_value, r_squared, trend_direction)
print("Breakpoint Temporal Trends by Crop:")
print(breakpoint_trends)

library(tidyverse)

# Ensure Year is numeric
breakpoint_data_clean$Year_numeric <- as.numeric(as.character(breakpoint_data_clean$Year))

# Group by crop and analyze trends in yield loss
yield_loss_trends <- breakpoint_data_clean %>%
  group_by(Crop) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(yield_loss ~ Year_numeric, data = .x)),
    slope = map_dbl(model, ~ coef(.x)[["Year_numeric"]]),
    p_value = map_dbl(model, ~ summary(.x)$coefficients["Year_numeric", "Pr(>|t|)"]),
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    trend_direction = case_when(
      p_value < 0.05 & slope > 0 ~ "Increasing",
      p_value < 0.05 & slope < 0 ~ "Decreasing",
      TRUE ~ "No significant trend"
    )
  ) %>%
  select(Crop, slope, p_value, r_squared, trend_direction)

print("Yield Loss Temporal Trends by Crop:")
print(yield_loss_trends)

# 2. STATISTICAL SIGNIFICANCE TESTING
# =====================================================================================
cat("\n=== STATISTICAL SIGNIFICANCE TESTING ===\n")

# ANOVA for breakpoint differences among crops
breakpoint_anova <- aov(BreakPoint ~ Crop, data = breakpoint_data_clean)
print("ANOVA for Breakpoint differences among crops:")
print(summary(breakpoint_anova))

# Post-hoc Tukey HSD test
tukey_breakpoint <- TukeyHSD(breakpoint_anova)
print("Tukey HSD for Breakpoint differences:")
print(tukey_breakpoint)

# ANOVA for yield loss differences among crops
yield_loss_anova <- aov(yield_loss ~ Crop, data = breakpoint_data_clean)
print("ANOVA for Yield Loss differences among crops:")
print(summary(yield_loss_anova))

# Post-hoc Tukey HSD test for yield loss
tukey_yield_loss <- TukeyHSD(yield_loss_anova)
print("Tukey HSD for Yield Loss differences:")
print(tukey_yield_loss)

# 3. CORRELATION ANALYSIS
# =====================================================================================
cat("\n=== CORRELATION ANALYSIS ===\n")

# Correlation between breakpoint distance and yield metrics
correlation_data <- breakpoint_data_clean %>%
  select(BreakPoint, Yield_Inside_BreakPoint, Yield_Outside_BreakPoint, 
         Yield_Difference, yield_loss, Year_numeric) %>%
  filter(complete.cases(.))

correlation_matrix <- cor(correlation_data)
print("Correlation Matrix:")
print(round(correlation_matrix, 3))

# Create correlation plot
png("Z:/temp/correlation_plot.png", width = 800, height = 600)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")
dev.off()

# 4. BREAKPOINT CATEGORIES ANALYSIS
# =====================================================================================
cat("\n=== BREAKPOINT CATEGORIES ANALYSIS ===\n")

# Categorize breakpoints into distance ranges
breakpoint_data_clean$BP_Category <- cut(breakpoint_data_clean$BreakPoint,
                                         breaks = c(5, 15, 25, 35, 50),
                                         labels = c("Short (5-15m)", "Medium (15-25m)", 
                                                   "Long (25-35m)", "Very Long (35-50m)"),
                                         include.lowest = TRUE)

# Analysis by breakpoint categories
bp_category_summary <- breakpoint_data_clean %>%
  group_by(Crop, BP_Category) %>%
  summarise(
    Count = n(),
    Mean_Yield_Loss = round(mean(yield_loss, na.rm = TRUE), 2),
    SD_Yield_Loss = round(sd(yield_loss, na.rm = TRUE), 2),
    Mean_Inside_Yield = round(mean(Yield_Inside_BreakPoint, na.rm = TRUE), 2),
    Mean_Outside_Yield = round(mean(Yield_Outside_BreakPoint, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print("Breakpoint Category Analysis:")
print(bp_category_summary)

# Statistical test for yield loss across categories
category_anova <- aov(yield_loss ~ BP_Category * Crop, data = breakpoint_data_clean)
print("ANOVA for Yield Loss across Breakpoint Categories:")
print(summary(category_anova))

# 5. ECONOMIC IMPACT ANALYSIS
# =====================================================================================
cat("\n=== ECONOMIC IMPACT ANALYSIS ===\n")

# Approximate commodity prices (2024 averages - adjust as needed)
commodity_prices <- data.frame(
  Crop = c("CORN", "SOYBEANS", "WHEAT"),
  Price_per_bushel = c(4.25, 11.50, 6.75)  # USD per bushel
)

# Calculate economic losses
economic_analysis <- breakpoint_data_clean %>%
  left_join(commodity_prices, by = "Crop") %>%
  mutate(
    Economic_Loss_per_acre = abs(Yield_Difference) * Price_per_bushel,
    Economic_Loss_percentage = (Economic_Loss_per_acre / (Yield_Inside_BreakPoint * Price_per_bushel)) * 100
  )

economic_summary <- economic_analysis %>%
  group_by(Crop) %>%
  summarise(
    Mean_Economic_Loss_per_acre = round(mean(Economic_Loss_per_acre, na.rm = TRUE), 2),
    Median_Economic_Loss_per_acre = round(median(Economic_Loss_per_acre, na.rm = TRUE), 2),
    Mean_Economic_Loss_percentage = round(mean(Economic_Loss_percentage, na.rm = TRUE), 2),
    Total_Fields = n(),
    .groups = 'drop'
  )

print("Economic Impact Summary:")
print(economic_summary)

# 6. YIELD STABILITY ANALYSIS
# =====================================================================================
cat("\n=== YIELD STABILITY ANALYSIS ===\n")

# Calculate coefficient of variation for yield stability
yield_stability <- breakpoint_data_clean %>%
  group_by(Crop, Year) %>%
  summarise(
    CV_Inside = sd(Yield_Inside_BreakPoint) / mean(Yield_Inside_BreakPoint) * 100,
    CV_Outside = sd(Yield_Outside_BreakPoint) / mean(Yield_Outside_BreakPoint) * 100,
    Stability_Difference = CV_Outside - CV_Inside,
    .groups = 'drop'
  )

stability_summary <- yield_stability %>%
  group_by(Crop) %>%
  summarise(
    Mean_CV_Inside = round(mean(CV_Inside, na.rm = TRUE), 2),
    Mean_CV_Outside = round(mean(CV_Outside, na.rm = TRUE), 2),
    Mean_Stability_Difference = round(mean(Stability_Difference, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print("Yield Stability Analysis:")
print(stability_summary)

# 7. FIELD SIZE IMPACT ANALYSIS (if field area data available)
# =====================================================================================
cat("\n=== WEATHER/ENVIRONMENTAL CORRELATION ANALYSIS ===\n")

# Year-based analysis for potential climate correlations
yearly_summary <- breakpoint_data_clean %>%
  group_by(Year) %>%
  summarise(
    Mean_Breakpoint = mean(BreakPoint),
    Mean_Yield_Loss = mean(yield_loss),
    Field_Count = n(),
    Variability_Index = sd(BreakPoint) / mean(BreakPoint),
    .groups = 'drop'
  ) %>%
  mutate(
    Climate_Period = case_when(
      Year %in% c("2014", "2015", "2016") ~ "Early Period",
      Year %in% c("2017", "2018", "2019") ~ "Middle Period",
      Year %in% c("2020", "2021", "2022", "2023", "2024") ~ "Recent Period"
    )
  )

print("Yearly Environmental Analysis:")
print(yearly_summary)

# Climate period comparison
climate_comparison <- breakpoint_data_clean %>%
  mutate(
    Climate_Period = case_when(
      Year %in% c("2014", "2015", "2016") ~ "Early Period",
      Year %in% c("2017", "2018", "2019") ~ "Middle Period",
      Year %in% c("2020", "2021", "2022", "2023", "2024") ~ "Recent Period"
    )
  ) %>%
  group_by(Climate_Period, Crop) %>%
  summarise(
    Mean_Breakpoint = round(mean(BreakPoint), 2),
    Mean_Yield_Loss = round(mean(yield_loss), 2),
    Count = n(),
    .groups = 'drop'
  )

print("Climate Period Comparison:")
print(climate_comparison)

# 8. ADVANCED VISUALIZATION PLOTS
# =====================================================================================
cat("\n=== CREATING ADVANCED VISUALIZATION PLOTS ===\n")

# Yield loss distribution by crop
p1 <- ggplot(breakpoint_data_clean, aes(x = yield_loss, fill = Crop)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  facet_wrap(~Crop, scales = "free_y") +
  scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(title = "Distribution of Yield Loss by Crop",
       x = "Yield Loss (%)", y = "Frequency") +
  theme_minimal()
ggsave("Z:/temp/yield_loss_distribution.png", p1, width = 12, height = 8)

# Breakpoint vs Yield Loss scatter plot
p2 <- ggplot(breakpoint_data_clean, aes(x = BreakPoint, y = yield_loss, color = Crop)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("SOYBEANS" = "blue", "CORN" = "green", "WHEAT" = "orange")) +
  labs(title = "Relationship between Breakpoint Distance and Yield Loss",
       x = "Breakpoint Distance (m)", y = "Yield Loss (%)") +
  theme_minimal()
ggsave("Z:/temp/breakpoint_vs_yield_loss.png", p2, width = 10, height = 6)

# Economic loss by year and crop
p3 <- ggplot(economic_analysis, aes(x = Year, y = Economic_Loss_per_acre, fill = Crop)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("SOYBEANS" = "skyblue", "CORN" = "lightgreen", "WHEAT" = "lightyellow")) +
  labs(title = "Economic Loss per Acre by Year and Crop",
       x = "Year", y = "Economic Loss ($/acre)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Z:/temp/economic_loss_by_year.png", p3, width = 12, height = 8)

# 9. COMPREHENSIVE SUMMARY STATISTICS
# =====================================================================================
cat("\n=== COMPREHENSIVE SUMMARY FOR RESEARCH PAPER ===\n")

comprehensive_summary <- list(
  "Dataset Overview" = data.frame(
    Total_Fields = nrow(breakpoint_data_clean),
    Years_Covered = length(unique(breakpoint_data_clean$Year)),
    Crops_Analyzed = length(unique(breakpoint_data_clean$Crop)),
    Date_Range = paste(min(breakpoint_data_clean$Year), "to", max(breakpoint_data_clean$Year))
  ),
  
  "Breakpoint Statistics" = breakpoint_data_clean %>%
    summarise(
      Mean_Breakpoint = round(mean(BreakPoint), 2),
      Median_Breakpoint = round(median(BreakPoint), 2),
      SD_Breakpoint = round(sd(BreakPoint), 2),
      Min_Breakpoint = min(BreakPoint),
      Max_Breakpoint = max(BreakPoint)
    ),
  
  "Yield Impact Statistics" = breakpoint_data_clean %>%
    summarise(
      Mean_Yield_Loss_Percent = round(mean(yield_loss), 2),
      Median_Yield_Loss_Percent = round(median(yield_loss), 2),
      Mean_Yield_Difference = round(mean(Yield_Difference), 2),
      Fields_With_Yield_Loss = sum(yield_loss > 0),
      Percent_Fields_With_Loss = round(sum(yield_loss > 0) / n() * 100, 1)
    )
)

print("COMPREHENSIVE SUMMARY FOR RESEARCH PAPER:")
print(comprehensive_summary)

# Export all results
write.xlsx(list(
  "Breakpoint_Trends" = breakpoint_trends %>% select(-model),
  "Yield_Loss_Trends" = yield_loss_trends %>% select(-model),
  "BP_Category_Analysis" = bp_category_summary,
  "Economic_Summary" = economic_summary,
  "Stability_Analysis" = stability_summary,
  "Climate_Period_Analysis" = climate_comparison,
  "Yearly_Environmental" = yearly_summary,
  "Comprehensive_Stats" = do.call(rbind, comprehensive_summary)
), file = "Z:/temp/comprehensive_breakpoint_analysis.xlsx")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results exported to: Z:/temp/comprehensive_breakpoint_analysis.xlsx\n")
cat("Plots saved to: Z:/temp/\n")










#=========================================================================================
# Coorrelation Measurement
#=========================================================================================

Cor_Method = "spearman"
# CORN:
# =========================================================================================

#  Breakpoint_distance ~ BreakPoint_stregnth


cor.test(
  breakpoint_data_clean_soybeans$BreakPoint, 
  breakpoint_data_clean_soybeans$Breakpoint_strength, 
  method = Cor_Method
)

#breakpoint_distance ~ yield_loss
cor.test(
  breakpoint_data_clean_wheat$BreakPoint, 
  breakpoint_data_clean_wheat$yield_loss, 
  method = Cor_Method
)

#breakpoint_strength ~ yield_loss
cor.test(
  breakpoint_data_clean_soybeans$Breakpoint_strength, 
  breakpoint_data_clean_soybeans$yield_loss, 
  method = Cor_Method
)


#breakpoint_strength ~ average yield 
cor.test(
  breakpoint_data_clean_corn$Breakpoint_strength, 
  breakpoint_data_clean_corn$Field_Average_Yield, 
  method = Cor_Method
)

## ===============================================================================================================================================================================================================================================================================================

# Comparing Median values with Favorability of Years

breakpoint_details_by_years <- read.xlsx("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Yearly Yield BreakPoint/Yearly Yield Analysis/Breakpoint_Details_By_year.xlsx")
View(breakpoint_details_by_years)

breakpoint_details_by_years_corn <- breakpoint_details_by_years %>% filter(Crop == "CORN")
breakpoint_details_by_years_soybeans <- breakpoint_details_by_years %>% filter(Crop == "SOYBEANS")
breakpoint_details_by_years_wheat <- breakpoint_details_by_years %>% filter(Crop == "WHEAT")

cor.test(
  breakpoint_details_by_years_wheat$Median_BreakPoint, 
  breakpoint_details_by_years_wheat$favorability_percent, 
  method = "kendall" # Using "kendall" as an alternative to "spearman"
)

cor.test(
  breakpoint_details_by_years_wheat$Median_Breakpoint_strength, 
  breakpoint_details_by_years_wheat$favorability_percent, 
  method = "kendall"
)

cor.test(
  breakpoint_details_by_years_wheat$Median_Yield_Loss, 
  breakpoint_details_by_years_wheat$favorability_percent, 
  method = "kendall"
)


#=========================================================================================
# Scatter Plots for the correlation analysis
#=========================================================================================


library(patchwork)

library(rlang)

make_scatter <- function(df, xvar, yvar, crop_name, colour_pt) {
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  
  # Fit the linear model dynamically
  formula <- as.formula(paste0(rlang::as_name(yvar), " ~ ", rlang::as_name(xvar)))
  lm_model <- lm(formula, data = df)
  
  # Plot
  ggplot(df, aes(x = !!xvar, y = !!yvar)) +
    geom_point(alpha = .7, size = 2, colour = colour_pt) +
    geom_smooth(method = "lm", se = TRUE,
                colour = "darkred", fill = "pink",
                alpha = .25, linewidth = 1) +
    labs(title     = crop_name,
         x         = rlang::as_name(xvar),
         y         = rlang::as_name(yvar),
         subtitle  = paste0("n = ", nrow(df),
                            "  |  Adj. R² = ",
                            round(summary(lm_model)$adj.r.squared, 5))) +
    theme_minimal(base_size = 12) +
    theme(plot.title    = element_text(size = 16, face = "bold", hjust = .5),
          plot.subtitle = element_text(size = 11, hjust = .5))
}

panel_soy   <- make_scatter(breakpoint_data_clean_soybeans,Breakpoint_strength, yield_loss,  "Soybeans", "royalblue")
panel_corn  <- make_scatter(breakpoint_data_clean_corn,   Breakpoint_strength,   yield_loss,  "Corn",     "forestgreen")
panel_wheat <- make_scatter(breakpoint_data_clean_wheat,  Breakpoint_strength,  yield_loss, "Wheat",    "orange")

panel_all <- (panel_soy | panel_corn | panel_wheat)  # patchwork layout
print(panel_all)

ggsave("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Yearly Yield BreakPoint/Yearly Yield Analysis/CorrelationPlots/breakpoint_strength_vs_distance.pdf", panel_all)


#=========================================================================================
# Overall Field Average comparison
#=========================================================================================
# ── libraries ────────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(patchwork)

# ── helper that returns a ready box-plot ggplot object ───────────────────────
make_bp_plot <- function(df, crop_label, fill_col) {
  
  
  # 5-m BreakPoint bins
  # breaks <- seq(
  #   floor(min(df$Field_Average_Yield, na.rm = TRUE) / 50) * 50,
  #   ceiling(max(df$Field_Average_Yield, na.rm = TRUE) / 50) * 50,
  #   by = 50
  # )
  # breaks <- c(50, 100,145, 190, Inf) #Corn
  # breaks <- c(15, 32,48, 63, Inf) # Soybeans
  breaks <- c(33, 49,63, 78, Inf) # Wheat
  
  df <- df %>%
    mutate(
      Break_bin = cut(Field_Average_Yield,
                      breaks = breaks,
                      # labels = c("50-100", "100-145", "145-190", "190-235"),
                      # labels = c("15-32", "32-48", "48-63", "63-80"),
                      labels = c("33-49", "49-63", "63-78", "78-93"),
                      include.lowest = TRUE,
                      right = TRUE,
                      dig.lab = 4)
    )
  
  ggplot(df, aes(x = Break_bin, y = BreakPoint)) +
    geom_boxplot(fill = fill_col,      # <- fixed fill colour
                 alpha = 0.65, width = 0.7, outlier.size = 0.8) +
    labs(
      title = crop_label,
      x     = "Average Yield (bu/ac)",
      y     = "Breakpoint Distance (m)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(size = 13, face = "bold", hjust = 0.5),
    )
}

# ──────────────────────────────── build the three plots with their colours ────────────────────────────────
p_corn    <- make_bp_plot(breakpoint_data_clean_corn,     "Corn",     "lightgreen")
p_soybean <- make_bp_plot(breakpoint_data_clean_soybeans, "Soybeans", "skyblue")
p_wheat   <- make_bp_plot(breakpoint_data_clean_wheat,    "Wheat",    "lightyellow")

# ──────────────────────────────── patch them together ──────────────────────────────────────────────────────
combined_plot <- p_corn | p_soybean | p_wheat +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "BreakPoint Distance vs. Yield Loss (5-m bins)",
    theme = theme(plot.title = element_text(size = 15, face = "bold"))
  )

combined_plot            # display (or ggsave) the final figure








