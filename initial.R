library(readxl)
library(dplyr)
library(rpart)
library(caret)
library(rpart.plot)
library(ggplot2)
library(tidyr)

data <- read_excel("C:/Users/Renee Stanislaw/Downloads/data (1).xlsx")
#View(data)

Catapult_Data_1_ <- read_excel("C:/Users/Renee Stanislaw/Downloads/Catapult Data (1).xlsx")

#Create the dataframe with Nike ID and Average Acceleration for the three games
average_acceleration <- Catapult_Data_1_ %>%
  group_by(`Nike ID`) %>%
  summarise(average_acceleration = mean(`Max Acceleration`, na.rm = TRUE))
#View(average_acceleration)

biodex <- read_excel("C:/Users/Renee Stanislaw/Downloads/biodex.xlsx")
#View(biodex)

biodex <- biodex[-c(6,7, 10, 14, 31), ]

#Quantile section
WSOC2022 <- read_excel("C:/Users/Renee Stanislaw/Downloads/WSOC 2022 - Ben and Evan.xlsx")
#View(WSOC2022)

WSOC2023 <- read_excel("C:/Users/Renee Stanislaw/Downloads/WSOC 2023 - Ben and Evan.xlsx")
#View(WSOC2023)

#game data for 2022 season
games_WSOC2022 <- WSOC2022 %>%
  filter(grepl("game", `Activity Name`, ignore.case = TRUE)) %>%
  select(`Max Acceleration`)

#game data for 2023 season
game_WSOC2023 <- WSOC2023 %>%
  filter(grepl("game", `Activity Name`, ignore.case = TRUE)) %>%
  select(`Max Acceleration`)

#Create the quantiles with the combined data sets
max_acceleration_combined <- c(games_WSOC2022$`Max Acceleration`, game_WSOC2023$`Max Acceleration`)

quantiles <- quantile(max_acceleration_combined, probs = c(0.2, 0.8))

# Create labels based on quantiles
labels <- cut(max_acceleration_combined, 
              breaks = c(-Inf, quantiles[1], quantiles[2], Inf), 
              labels = c("lower", "middle", "upper"))

# Calculate label distribution
label_distribution <- table(labels) / length(labels)

# Convert label_distribution to percentage
label_distribution_percentage <- label_distribution * 100
print(label_distribution)

data_with_labels <- data.frame(Max_Acceleration = max_acceleration_combined, Label = labels)

# Subset data for each label
lower_values <- data_with_labels[data_with_labels$Label == "lower", "Max_Acceleration"]
middle_values <- data_with_labels[data_with_labels$Label == "middle", "Max_Acceleration"]
upper_values <- data_with_labels[data_with_labels$Label == "upper", "Max_Acceleration"]

# Print the values
print("Lower values:")
print(lower_values)
print("Middle values:")
print(middle_values)
print("Upper values:")
print(upper_values)

boxplot(Max_Acceleration ~ Label, data = data_with_labels, 
        xlab = "Label", ylab = "Max Acceleration",
        main = "Max Acceleration Distribution by Label")

# Reorder `average_acceleration` by Nike ID to match the order of `biodex` by subject_id
average_acceleration_reordered <- average_acceleration[match(biodex$subject_id, average_acceleration$`Nike ID`), ]
#View(average_acceleration_reordered)

common_ids <- intersect(average_acceleration_reordered$`Nike ID`, biodex$subject_id)

# Filter both dataframes based on common Nike IDs
average_acceleration_filtered <- average_acceleration_reordered[average_acceleration_reordered$`Nike ID` %in% common_ids, ]
biodex_filtered <- biodex[biodex$subject_id %in% common_ids, ]

# View the filtered dataframes
#View(average_acceleration_filtered)

biodex_filtered <- biodex[biodex$subject_id %in% common_ids, !(names(biodex) %in% c("subject_id", "field_sport", "primary_position", "year", "dot"))]
biodex_filtered <- biodex_filtered[-11, ]
average_acceleration_filtered <- average_acceleration_filtered[-11, ]
#View(biodex_filtered)
#View(average_acceleration_filtered)
#Now everything matches

# Extract average accelerations from average_acceleration_filtered
breaks <- c(-Inf, quantiles[1], quantiles[2], Inf)
labels <- c("lower", "middle", "upper")

# Assign labels based on the breaks and labels
average_acceleration_filtered$label <- cut(average_acceleration_filtered$average_acceleration, 
                                           breaks = breaks, 
                                           labels = labels)

# View the modified dataframe
#View(average_acceleration_filtered)

average_acceleration = average_acceleration_filtered$average_acceleration

#Correlation matrix of Biodex data
#correlation_matrix <- cor(biodex_filtered)
#print(correlation_matrix)

#correlation_melted <- melt(correlation_matrix)

# Plot the heatmap
#ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  #geom_tile(color = "white") +
  #scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       #limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #labs(title = "Correlation Heatmap",
       #x = "Variables", y = "Variables",
       #fill = "Correlation") +
  #coord_fixed()

#cor(prcomp(data_trimmed[,-13], center = T, scale. = T)$x, average_acceleration_filtered$average_acceleration)
#pca_result <- prcomp(biodex_filtered, center = TRUE, scale. = TRUE)

# Extract principal component scores
#pca_scores <- pca_result$x

# Plot PCA results
#plot(pca_scores[,1], average_acceleration_filtered$average_acceleration,
     #xlab = "PC1", ylab = "Average Acceleration",
     #main = "PCA: PC1 vs Average Acceleration")

#
linear_model <- lm(data$average_acceleration~.,data)
summary(linear_model)
plot(linear_model)
plot(data$average_acceleration ~ data[, -which(names(data) == "average_acceleration")], main = "Scatter plot with Regression Line")
correlation_matrix <- cor(data[, -which(names(data) == "average_acceleration")])

# Plot the correlation matrix as a heatmap
library(ggplot2)
library(reshape2)
correlation_matrix <- cor(biodex_filtered)
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Collinearity Plot",
       x = "Variables", y = "Variables",
       fill = "Correlation") +
  coord_fixed()

# Add regression line to the plot
abline(linear_model, col = "red")

pca <- prcomp(data[,-13], scale. = T, center = T)

for (i in 1:12){
  print(paste0("PCA ", i, ":   ",
  cor(pca$x[,i], data$average_acceleration)))
}

pca_x_3 <- as.data.frame(cbind(pca$x[,c(9,12)], data$average_acceleration))
pca_lm <- lm(pca_x_3$V3 ~., pca_x_3 )
summary(pca_lm)

biplot(pca, choices = c(9,12))

pca_x_9 <- as.data.frame(cbind(pca$x[,c(9)], data$average_acceleration))
pca9_lm <- lm(pca_x_9$V2 ~., pca_x_9 )
summary(pca9_lm)
plot(pca_x_9)

pca_x_12 <- as.data.frame(cbind(pca$x[,c(12)], data$average_acceleration))
pca12_lm <- lm(pca_x_12$V2 ~., pca_x_12 )
summary(pca12_lm)
plot(pca_x_12)

#pca_x_12 <- as.data.frame(cbind(pca$x[, 12], data$average_acceleration))

# Renaming the columns
#colnames(pca_x_12) <- c("PC_12", "average_acceleration")

# Performing linear regression
#pca12_lm <- lm(average_acceleration ~ PC_12, data = pca_x_12)

# Printing summary of the regression
#summary(pca12_lm)

# Plotting the data
#plot(average_acceleration ~ PC_12, data = pca_x_12, main = "PCA vs. average_acceleration", xlab = "PCA 12", ylab = "average_acceleration")

# Adding regression line
#abline(pca12_lm, col = "red")

a <- prcomp(iris[1:4], scale. = T, center = T)

a$rotation[,1]

library(ggplot2)

# Assuming 'a$rotation[,1]' contains the data you want to plot
data <- pca$x[, 12]

# Create a data frame with the data
df <- data.frame(x = 1:length(data), y = data)

# Create the lollipop plot
plot <- ggplot(df, aes(x = x, y = y)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "darkgreen", size = 2.5) + # Adjust size and color here
  geom_point(color = "darkgreen", size = 3) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the title
  labs(title = "Lollipop Plot of Principal Component 12",
       x = "Variable",
       y = "Vector Weight")

# Display the plot
print(plot)

a <- prcomp(iris[1:4], scale = TRUE, center = TRUE)

# Extract vector weights for Principal Component 12
vector_weights <- a$rotation[, 1]

# Print out the vector weights
print(vector_weights)

data$rotation[,12]

