# ======================logs ============================================================

# -7/1/2025 : making changes as asked by Gebba: Calculating Yield Loss at the edges


# ======================logs ============================================================



# Your existing code (working part)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
# library(ggplotly)

crop = "Wheat"

# field_edge_yield_data <- read.xlsx("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Edge Specific Analysis/Results/10m/EdgeSpecificAnalysisResults.xlsx", colNames = TRUE)
# field_edge_yield_data <- read.xlsx("../Surrounding Specifc Effect on Edge Yield/Results.xlsx", colNames = TRUE)
field_edge_yield_data <- read.xlsx("../Surrounding Specifc Effect on Edge Yield/ResultsWithMidFieldYield.xlsx", colNames = TRUE)
View(field_edge_yield_data)

crop_field_edge_yield_data <- field_edge_yield_data %>% filter(Crop == crop & !is.na(Yield_MidField))
# crop_field_edge_yield_data <- field_edge_yield_data %>% filter(Crop %in% c("Corn", "Corn", "Corn"))
# View(crop_field_edge_yield_data)

crop_long <- crop_field_edge_yield_data %>%
  pivot_longer(
    cols = ends_with("_Yield"),        # Select yield columns
    names_to = "Direction",            # New column: direction name
    values_to = "Yield"                # New column: actual yield values
  )

crop_long$Direction <- gsub("_Yield", "", crop_long$Direction)



# crop_long <- crop_long %>% filter(!grepl("Original", Direction))
# Replace values in the "Direction" column
crop_long$Edge_Type <- apply(crop_long, 1, function(row) {
  direction_name <- row["Direction"]        # Get the string value e.g., "North"
  value <- row[direction_name]              # Use it to get the value from the column of the same name
  return(value)                             # Replace it
})

crop_long <- crop_long %>% filter(!grepl("/", Edge_Type))



crop_long_fixed <- crop_long

# View(crop_long_fixed)


crop_long_fixed[crop_long_fixed$Edge_Type == "Water", "Edge_Type"] <- "Other"
crop_long_fixed[crop_long_fixed$Edge_Type == "Human", "Edge_Type"] <- "Other"

crop_long_fixed <- crop_long_fixed %>% filter(Edge_Type != "Other") 
table(crop_long_fixed$Edge_Type)

# crop_long_fixed$Total_Average <- mean(crop_long_fixed$Yield, na.rm =  TRUE)
crop_long_fixed$Yield <- as.numeric(crop_long_fixed$Yield) / as.numeric(crop_long_fixed$Average) * 100

# Reorder factor levels to control boxplot order: A, B, C
crop_long_fixed$Edge_Type <- factor(crop_long_fixed$Edge_Type, levels = c("Barren", "Crop", "Forest", "Road", "Other"))

View(crop_long_fixed)


# crop_long_fixed$Yield_Loss = 100 * (crop_long_fixed$Yield_MidField - crop_long_fixed$Yield) / crop_long_fixed$Yield_MidField 

# Basic boxplot by edge type only
p1 <- ggplot(crop_long_fixed, aes(x = Edge_Type, y = Yield)) +
  geom_boxplot(fill = "lightyellow", alpha = 0.7,) +
  geom_point(alpha = 0.8, color = "black", size = 1.5)+
  # theme_minimal() +
  labs(
    title = paste(crop, "Yield by Edge Type"),
    subtitle = "Buffer Size = 20m",
    x = "Edge Type",
    y = "Percent of Total Average Yield"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold")
        )

print(p1)
# ggsave(filename=file.path(paste0("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Surrounding Specifc Effect on Edge Yield/Corn/Relative_Corn_Yield_20m_By_edge_type.pdf")),width = 10, height = 8, plot = p1, device = "pdf")



interactive_plot <- ggplotly(p1)
 # Show the interactive plot
interactive_plot

# Boxplot with direction as fill (your original goal)
p2 <- ggplot(crop_long_fixed, aes(x = Edge_Type, y = Yield, fill = Direction)) +
  geom_boxplot(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = paste(crop,"Yield by Edge Type and Direction"),
    x = "Edge Type",
    y = "Yield (bushels/acre)",
    fill = "Field Direction"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")

print(p2)

# Faceted version (clearer visualization)
p3 <- ggplot(crop_long_fixed, aes(x = Edge_Type, y = Yield)) +
  
  geom_boxplot(aes(fill = Edge_Type), alpha = 0.8) +
  geom_point(alpha = 0.8, color = "red", size = 1)+
    facet_wrap(~Direction, scales = "free_x", nrow = 1) +
  # theme_minimal() +
  labs(
    title = paste(crop,"Yield by Edge Type, Separated by Field Direction"),
    subtitle = "Buffer Size = 20m",
    x = "Edge Type",
    y = "Average Yield inside Buffer (bu/acre)"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text( size = 12 ,hjust = 0.5, angle = 30),
    axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold")
  )+
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")  # Add gap between facet plots
  ) +
  scale_fill_brewer(palette = "Set3")

print(p3)
# ggsave(filename=file.path("C:/Users/acer/OneDrive - Texas State University/ChoLab/USDA Crop yield Stability Study/BARC_Yield_Analysis/Results/Final Compiled Results/Surrounding Specifc Effect on Edge Yield/Corn/Corn_Yield_20m_By_edge_type_faceted_by_Direction.pdf"), width=10, height=8, plot = p3, device = "pdf")

  # SUMMARY STATISTICS
print("=== SUMMARY STATISTICS ===")

# Overall summary by edge type
edge_summary <- crop_long_fixed %>%
  group_by(Edge_Type) %>%
  summarise(
    n_observations = n(),
    mean_yield = round(mean(Yield, na.rm = TRUE), 2),
    median_yield = round(median(Yield, na.rm = TRUE), 2),
    sd_yield = round(sd(Yield, na.rm = TRUE), 2),
    min_yield = round(min(Yield, na.rm = TRUE), 2),
    max_yield = round(max(Yield, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_yield))

print("Summary by Edge Type:")
print(edge_summary)
  # write.csv(as.matrix(edge_summary), file = file.path(paste0(getwd(), "/Results/Edge Specific Analysis/Results/10m1.1/Corn/TotalPercentage/Corn_Fields_Edge_summary.csv")))
  # write.csv(as.matrix(edge_summary), file = file.path("Z:/temp/Corn/Corn_Fields_Edge_Type_Summary.csv"))


# Detailed summary by edge type and direction
detailed_summary <- crop_long_fixed %>%
  group_by(Edge_Type, Direction) %>%
  summarise(
    n_observations = n(),
    mean_yield = round(mean(Yield, na.rm = TRUE), 2),
    median_yield = round(median(Yield, na.rm = TRUE), 2),
    sd_yield = round(sd(Yield, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(Edge_Type, Direction)

print("Detailed Summary by Edge Type and Direction:")
print(detailed_summary)
lwrite.csv(as.matrix(detailed_summary), file = file.path("Z:/temp/Corn/Corn_Fields_detailed_summary.csv"))


# Count of observations per combination
count_table <- table(crop_long_fixed$Edge_Type, crop_long_fixed$Direction)
print("Count of observations (Edge Type x Direction):")

print(count_table)

# write.csv(as.matrix(count_table), file = file.path(paste0(getwd(), "/Results/Edge Specific Analysis/Results/10m1.1/Corn/TotalPercentage/Corn_Fields_Edge_Type_by_Direction.csv")))
write.csv(as.matrix(count_table), file = file.path("Z:/temp/Corn/Corn_Fields_Edge_Type_by_Direction.csv"))









#=========================================================================================
#Statistical testing on the data that we got
# View(crop_long_fixed)
crop_long_fixed$Edge_Type <- as.factor(crop_long_fixed$Edge_Type)

#=========================================================================================
# ANOVA Test
# Yield ~ Edge_Type
anova_result <- aov(Yield ~ Edge_Type, data = crop_long_fixed)
summary(anova_result)
#Writing the results into file:
anova_df <- broom::tidy(anova_result)
# write.csv( anova_df,file = file.path(getwd(), "Results/Edge Specific Analysis/Results/10m1.1/Corn/TotalPercentage/Corn_Anova_Test_Yield_EdgeType.csv"), row.names = FALSE)
write.csv( anova_df,file = file.path("Z:/temp/Corn/Corn_Anova_Test_Yield_EdgeType.csv"), row.names = FALSE)

#=========================================================================================
#TUKEY Test
tukey <- TukeyHSD(anova_result)
print(tukey)
#Writing the results into file:
tukey_df <- broom::tidy(tukey)
write.csv( tukey_df,file = file.path("Z:/temp/Corn/Corn_TUKEY_HSD_Yield_EdgeType.csv"), row.names = FALSE)

#=======================================================================================
# Yield ~ Edge_Type
crop_long_fixed$Direction <- as.factor(crop_long_fixed$Direction)
anova_interaction <- aov(Yield ~ Edge_Type * Direction, data = crop_long_fixed)
summary(anova_interaction)
#Writing the results into file:
anova_interaction_df <- broom::tidy(anova_interaction)
write.csv( anova_interaction_df,file = file.path("Z:/temp/Corn/Corn_ANOVA_TEST_Yield_EdgeType_Direction.csv"), row.names = FALSE)






#======================================================================================
#Yield Loss at the edges
#======================================================================================
p1 <- ggplot(crop_long_fixed, aes(x = Edge_Type, y = Yield_Loss)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7,) +
  geom_point(alpha = 0.8, color = "black", size = 1.5)+
  # theme_minimal() +
  labs(
    title = paste(crop, "Yield Loss at Edges by Edge Type"),
    subtitle = "Buffer Size = 20m",
    x = "Edge Type",
    y = "Percent of Yield Loss"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.text.y = element_text(angle =0 , size = 12 ,hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold")
  )

print(p1)

edge_summary_yield_loss <- crop_long_fixed %>%
  group_by(Edge_Type) %>%
  summarise(
    n_observations = n(),
    mean_yield_loss_percent = round(mean(Yield_Loss, na.rm = TRUE), 2),
    median_yield_loss_percent = round(median(Yield_Loss, na.rm = TRUE), 2),
    sd_yield_loss_percent = round(sd(Yield_Loss, na.rm = TRUE), 2),
    min_yield_loss_percent = round(min(Yield_Loss, na.rm = TRUE), 2),
    max_yield_loss_percent = round(max(Yield_Loss, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_yield_loss_percent))
print(edge_summary)

write.csv( edge_summary_yield_loss,file = file.path("Z:/temp/Corn/Corn_Yield_Loss_By_EdgeType.csv"), row.names = FALSE)




