#clear environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(pracma)  # For numerical integration using trapz function
library(caTools)  # For running mean smoothing to help with peak detection

# Set working directory         - user to set this -
# setwd("I:/My Drive/SOSG/Students/Leo/Data/incubations/Day1")
setwd("I:/My Drive/SOSG/Students/Leo/Data/incubations/Day2")

# Read and format the glucose CSV file         - user to set this -
# glucose_data <- read.csv("26feb_CO2.csv", header = TRUE)
glucose_data <- read.csv("27feb_CO2.csv", header = TRUE)

# Convert columns to appropriate data types with corrected time format
glucose_data <- glucose_data %>%
  mutate(
    TIME = as.POSIXct(TIME, format = "%H:%M:%S", tz = "UTC"),  # Convert to POSIXct format
    CO2 = as.numeric(CO2),
    TEMP = as.numeric(TEMP),
    HOUR = as.character(HOUR)
  )

# Read and format the injection time CSV file         - user to set this -
# time_data <- read.csv("time26Mar.csv", header = TRUE, stringsAsFactors = FALSE)
time_data <- read.csv("time27feb.csv", header = TRUE, stringsAsFactors = FALSE)

# Remove empty columns
empty_cols <- sapply(time_data, function(x) all(is.na(x) | x == ""))
time_data <- time_data[, !empty_cols]

# Convert columns to appropriate data types
time_data <- time_data %>%
  mutate(
    jar = as.numeric(jar),
    site = as.factor(site),
    tmnt = as.factor(tmnt),
    rep = as.numeric(rep)
  )

# Convert time data to long format
time_long <- time_data %>%
  pivot_longer(cols = starts_with("yt"), names_to = "timept", values_to = "time") %>%
  mutate(timept = str_replace(timept, "yt", ""))  # Remove 'yt' prefix from timept values

# Convert zvolt columns to long format
zvolt_long <- time_data %>%
  pivot_longer(cols = starts_with("zvolt"), names_to = "timept", values_to = "injvol") %>%
  mutate(timept = str_replace(timept, "zvolt", ""))  # Strip 'zvolt' prefix to align with timept

# Suppose df2 has an extra column "new_column" you want to add to df1
data_merged <- merge(time_long, zvolt_long[, c("jar", "site", "tmnt", "rep", "timept", "injvol")],
                   by = c("jar", "site", "tmnt", "rep", "timept"),
                   all.x = TRUE)  # Left join: keep all rows from df1

##### REMOVE COLUMNS STARTING WITH 'zvolt' #####
data_merged2 <- data_merged %>% select(-starts_with("zvolt"))


# Function to detect significant peaks based on sudden rises and falls in CO2 concentration
detect_and_calculate_peaks <- function(df, threshold_increase = 2, smoothing_window = 3, min_peak_distance = 5) {
  df <- df %>% arrange(TIME)
  
  # Smooth data to reduce noise but preserve peak structure
  df$CO2_smooth <- runmean(df$CO2, k = smoothing_window, endrule = "mean")
  
  # Identify peaks: a peak is detected where CO2 sharply increases above a threshold and then decreases
  potential_peaks <- which(diff(df$CO2_smooth) > threshold_increase) + 1  # Lower threshold to detect more peaks
  
  if (length(potential_peaks) == 0) return(NULL)  # Return NULL if no peaks found
  
  # Ensure detected peaks are spaced apart by at least 'min_peak_distance' seconds
  filtered_peaks <- potential_peaks[1]  # Always keep the first peak
  for (i in 2:length(potential_peaks)) {
    if (as.numeric(difftime(df$TIME[potential_peaks[i]], df$TIME[filtered_peaks[length(filtered_peaks)]], units = "secs")) >= min_peak_distance) {
      filtered_peaks <- c(filtered_peaks, potential_peaks[i])
    }
  }
  
  if (length(filtered_peaks) == 0) return(NULL)
  
  # Calculate metrics for each confirmed peak
  peak_metrics <- map_dfr(filtered_peaks, function(peak_index) {
    
    # Define peak boundaries dynamically
    right_limit <- min(nrow(df), peak_index + 20)  # Look ahead up to 20 points
    peak_region <- df$CO2[peak_index:right_limit]
    
    # Find the true maximum within the detected peak region
    max_index <- which.max(peak_region) + (peak_index - 1)
    peak_time <- df$TIME[max_index]  # Update to time of actual max value
    peak_height <- df$CO2[max_index]  # Update to actual max CO2 value
    
    # Define boundaries for integration
    left_index <- max(1, max_index - 10)  # Start 10 points before peak
    right_index <- min(nrow(df), max_index + 20)  # End 20 points after peak to capture gradual decline
    
    time_window <- df$TIME[left_index:right_index]
    co2_window <- df$CO2[left_index:right_index]
    
    # Calculate area under the curve (AUC) using trapezoidal integration
    auc <- trapz(as.numeric(difftime(time_window, min(time_window), units = "secs")), co2_window)
    
    data.frame(
      HOUR = unique(df$HOUR),  # Record the corresponding hour
      Peak_Time = peak_time,  # Time at which peak occurred
      Peak_Height = peak_height,  # Maximum CO2 value in peak
      AUC = auc  # Total CO2 detected within the peak
    )
  })
  
  return(peak_metrics)
}

# Apply peak detection and calculation to each HOUR group
peak_metrics <- glucose_data %>%
  group_by(HOUR) %>%
  group_split() %>%
  map_dfr(detect_and_calculate_peaks)

# Save the peak metrics to a new CSV file
write.csv(peak_metrics, "peak_metrics.csv", row.names = FALSE)

# Summary statistics: Count of identified peaks per HOUR
table_peaks <- peak_metrics %>%
  group_by(HOUR) %>%
  summarise(Count = n())

print(table_peaks)

##Zach found that the peak detection method often creates duplicate peaks for
##the same time point. OR it sometimes detects peaks where there isn't one. This 
## is obvious in the summary table. you can tweak the peak detection parameters if
## desired. However, Zach found a more effective solution is to integrate under the 
## curve for the 30seconds following the injection.
## Reasoning: the peak resolves after about 20 seconds (at most). Z compared the identified
## peak curve area and the area under the curve for the 30 seconds after the
##injection time. These were almost identical. Z suggests you QC/QA your curves

### Calculate Area Under Curve (AUC) for 30s Post Injection ###

# Function to calculate AUC for 30s post injection
calculate_auc_post_injection <- function(injection_time, data) {
  start_time <- as.POSIXct(injection_time, format = "%H:%M:%S", tz = "UTC")
  end_time <- start_time + 30  # 30 seconds after injection
  subset_data <- data %>% filter(TIME >= start_time & TIME <= end_time)
  
  if (nrow(subset_data) > 1) {
    auc <- trapz(as.numeric(difftime(subset_data$TIME, min(subset_data$TIME), units = "secs")), subset_data$CO2)
  } else {
    auc <- NA
  }
  return(auc)
}


# Apply AUC calculation to each injection time
data_merged3 <- data_merged2 %>%
  mutate(AUC_30s = sapply(time, calculate_auc_post_injection, data = glucose_data))

##### CORRECT AUC BY INJECTION VOLUME #####
##This produces a value of CO2/ml injected. Still needs to be calibrated with a standard curve
data_merged3 <- data_merged3 %>%
  mutate(AUCcor = AUC_30s / injvol)

# Save the updated dataframe with corrected AUC
write.csv(data_merged3, "time_with_AUC_corrected.csv", row.names = FALSE)

############################################ plot the results

##### VISUALIZE AUCcor OVER TIME #####

##### REMOVE INVALID REPLICATES #####

# Step 1: Remove NAs and zero values
data_merged3_filtered <- data_merged3 %>%
  filter(!is.na(AUCcor) & AUCcor != 0)

# Step 2: Identify and remove points where AUCcor drops by 400+ from one timept to the next within each jar
data_merged3_filtered <- data_merged3_filtered %>%
  arrange(jar, as.numeric(timept)) %>%  # Ensure correct ordering within each jar
  group_by(jar) %>%
  mutate(AUC_diff = AUCcor - lag(AUCcor)) %>%  # Compute difference from previous timept
  filter(is.na(AUC_diff) | AUC_diff > -400) %>%  # Keep only values where decrease < 400
  select(-AUC_diff)  # Remove temporary column

# Load ggplot2 if not already loaded
library(ggplot2)

##### VISUALIZE AUCcor OVER TIME (FILTERED) #####

ggplot(data_merged3_filtered, aes(x = as.numeric(timept), y = AUCcor, 
                               group = jar, color = site, shape = tmnt)) +
  geom_line(size = 1, alpha = 0.8) +  # Connect points with lines
  geom_point(size = 3) +  # Plot points
  labs(title = "Time Series of Corrected AUC (Filtered)",
       x = "Time Point",
       y = "Corrected AUC (AUCcor)",
       color = "Site",
       shape = "Treatment") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggplot(data_merged3_filtered, aes(x = as.numeric(timept), y = AUCcor, 
                                  group = jar, color = tmnt)) +
  geom_line(size = 1, alpha = 0.8) +  # Connect points with lines
  geom_point(size = 3) +  # Plot points
  labs(title = "Time Series of Corrected AUC (Filtered)",
       x = "Time Point",
       y = "Corrected AUC (AUCcor)",
       color = "Treatment") +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
