rm(list = ls())
# Load required libraries
library(tidyverse)
library(sf)
library(spatstat)

#gpsData <- read_csv("/Volumes/VERBATIM/amakhala/spreadsheets and tables/allPoints_071924.csv")
gpsData <- read_csv("/Volumes/VERBATIM/amakhala/spreadsheets and tables/allPoints_010925.csv")

# lionPts<- gpsData %>% filter(Species == "lion")
# head(lionPts)
# Remove rows with missing latitude or longitude
gps_data_clean <- gpsData %>% filter(!is.na(Latitude) & !is.na(Longitude))

gps_lion <- gps_data_clean %>% filter(Species == "lion")
gps_lion <- gps_lion %>%
  mutate(
    DIST_fences = DIST_fences / 1000,
    DIST_n2 = DIST_n2 / 1000,
    DIST_r342 = DIST_r342 / 1000,
    DIST_river = DIST_river / 1000
  )


# Convert to sf object with the correct initial CRS (WGS84)
lion_sf <- st_as_sf(gps_lion, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to Hartebeesthoek94 / Lo27 (EPSG:2047)
lion_projected <- st_transform(lion_sf, crs = 2054)

# Create a bounding box and convert to polygon
bbox <- st_bbox(lion_projected)
bbox_poly <- st_as_sfc(bbox)

# Calculate the area in square kilometers
area_km2 <- st_area(bbox_poly) / 1e6

# Convert sf object to ppp for density calculation
lion_coords <- st_coordinates(lion_projected)
lion_ppp <- ppp(x = lion_coords[,1], y = lion_coords[,2], 
                window = owin(xrange = bbox[c(1,3)], yrange = bbox[c(2,4)]))

# Calculate appropriate eps based on the extent of the data
x_range <- diff(bbox[c(1,3)])
y_range <- diff(bbox[c(2,4)])
eps_x <- max(x_range / 100, 1)  # Ensure minimum 1 meter resolution
eps_y <- max(y_range / 100, 1)

# Calculate point density
density_map <- density(lion_ppp, sigma = bw.scott(lion_ppp), eps = c(eps_x, eps_y))

# Convert density to lions per square kilometer
density_map_per_km2 <- density_map * 1e6

# Plot the density map
plot(density_map_per_km2, main = "Lion Density (per sq km)")
points(lion_ppp, pch = 16, col = "red", cex = 0.5)

# Extract density values at specific points (in lions per square kilometer)
density_values <- density_map_per_km2[lion_ppp]

# Add density values to the original dataframe
gps_lion$density_per_km2 <- density_values

# View the first few rows of the updated dataframe
print(head(gps_lion))

# Save the updated dataframe
write_csv(gps_lion, "gps_lion_with_density_per_km2.csv")

# Print summary statistics
cat("Summary of lion density (per sq km):\n")
print(summary(density_values))
cat("\nTotal study area:", round(as.numeric(area_km2), 2), "sq km\n")
cat("Total number of lions:", npoints(lion_ppp), "\n")
cat("Overall density:", round(npoints(lion_ppp) / as.numeric(area_km2), 4), "lions per sq km\n")

# Print information about the density calculation
cat("\nDensity calculation information:\n")
cat("X range:", x_range, "meters\n")
cat("Y range:", y_range, "meters\n")
cat("Epsilon X:", eps_x, "meters\n")
cat("Epsilon Y:", eps_y, "meters\n")

lion_sf <- st_as_sf(gps_lion, coords = c("Longitude", "Latitude"), crs = 4326)


river <- st_read("/Volumes/VERBATIM/amakhala/qgis/riverSegments.shp")
N2<- st_read("/Volumes/VERBATIM/amakhala/qgis/N2Segments.shp")
r342<- st_read("/Volumes/VERBATIM/amakhala/qgis/r342Segments.shp")
fence<- st_read("/Volumes/VERBATIM/amakhala/qgis/fenceSegments.shp")
river2<- st_transform(river, crs = 4326)
fence2<- st_transform(fence, crs = 4326)
r342_st<- st_transform(r342, crs = 4326)
riverCrop<- st_crop(river2, fence2)
r342crop <- st_crop(r342_st, fence2)

ggplot() +
  geom_sf(data = lion_sf, aes(color = density_per_km2), size = 1) +
  geom_sf(data = fence2, color = "green", size = 1) +
  geom_sf(data = N2, color = "red", size = 1) +
  geom_sf(data = r342crop, color = "orange", size = 1) +
  geom_sf(data = riverCrop, color = "blue", size = 1) +
  scale_color_viridis_c() +  # Use viridis color scale for better color representation
  theme_minimal() +
  labs(title = "Lion Density Map", x = "Longitude", y = "Latitude", color = "Density")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



library(lme4)
library(lmerTest)
lion_model <- lmer(density_per_km2 ~ DIST_n2 + DIST_fences + DIST_river + DIST_r342 +
                     
                     (1|Animal) + (1|Sighting_D) + (1|Habitat), data = gps_lion)

summary(lion_model)
plot(lion_model)

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Assuming gps_lion is already in your environment

# Reshape the data to long format
gps_lion_long <- gps_lion %>%
  dplyr::select(density_per_km2, DIST_n2, DIST_r342, DIST_fences, DIST_river) %>%
  pivot_longer(cols = starts_with("DIST_"),
               names_to = "distance_type",
               values_to = "distance_value")

# Create the plot
ggplot(gps_lion_long, aes(x = distance_value, y = density_per_km2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
  facet_wrap(~ distance_type, scales = "free_x", ncol = 2) +
  labs(title = "Lion Density vs. Distance Variables",
       x = "Distance",
       y = "Density (per km²)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))

# If you want to use predictions from your mixed model:
# (Note: This part assumes lion_model is in your environment)

# Create prediction data frames for each distance variable
create_pred_data <- function(var_name) {
  pred_data <- gps_lion %>%
    dplyr::select(starts_with("DIST_")) %>%
    summarise(across(everything(), mean)) %>%
    replicate(100, ., simplify = FALSE) %>%
    bind_rows() %>%
    mutate(!!var_name := seq(min(gps_lion[[var_name]]), 
                             max(gps_lion[[var_name]]), 
                             length.out = 100))
  
  # Add representative levels for random effects
  pred_data$Animal <- levels(gps_lion$Animal)[1]
  pred_data$Sighting_D <- levels(gps_lion$Sighting_D)[1]
  pred_data$Habitat <- levels(gps_lion$Habitat)[1]
  
  pred_data$predicted <- predict(lion_model, newdata = pred_data, re.form = NA)
  pred_data$distance_type <- var_name
  pred_data$distance_value <- pred_data[[var_name]]
  return(pred_data)
}

# Create prediction data for all distance variables
all_pred_data <- bind_rows(
  create_pred_data("DIST_n2"),
  create_pred_data("DIST_r342"),
  create_pred_data("DIST_fences"),
  create_pred_data("DIST_river")
)
# Warning message:
#   Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
# ℹ Please use `linewidth` instead.

# Create the plot with model predictions
ggplot(gps_lion_long, aes(x = distance_value, y = density_per_km2)) +
  geom_point(alpha = 0.5) +
  geom_line(data = all_pred_data, aes(y = predicted), color = "blue", size = 1) +
  facet_wrap(~ distance_type, scales = "free_x", ncol = 2) +
  labs(title = "Lion Density vs. Distance Variables (Mixed Model)",
       x = "Distance (km)",
       y = "Density (points/km²)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))