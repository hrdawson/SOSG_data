#clear environment
rm(list = ls())
# data.load
# Load necessary libraries
library(tidyverse)
library(pracma)  # For numerical integration using trapz function
library(caTools)  # For running mean smoothing to help with peak detection


# Set working directory         - user to set this -
setwd("I:/My Drive/SOSG/Students/Leo/Data/incubations/Day3/Leo work/SIR analysis/day 3_working_data")
# setwd("C://Users//leolo//Desktop//SIR data//SIR analysis//day 3_working_data")



# Read and format the glucose CSV file         - user to set this -
# glucose_data <- read.csv("26feb_CO2.csv", header = TRUE)
glucose_data <-  read.csv("GU-SIR-RUNS.csv", header = TRUE)


# Convert columns to appropriate data types with corrected time format
glucose_data <- glucose_data %>%
  mutate(
    TIME = as.POSIXct(TIME, format = "%H:%M:%S", tz = "UTC"),  # Convert to POSIXct format
    CO2 = as.numeric(CO2),
    TEMP = as.numeric(TEMP),
    HOUR = as.character(HOUR)
  )
debug_lines <- readLines("GU-SIR-RUNS.csv")[3:5]  # Show first 3 data rows
print(debug_lines)

# Read and format the injection time CSV file         - user to set this -
# time_data <- read.csv("time26Mar.csv", header = TRUE, stringsAsFactors = FALSE)
time_data <- read.csv("time28mar.csv", header = TRUE, stringsAsFactors = FALSE)

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


################
###now to calculate the PPM of headspace samples using linear models generated from a standard curve
######

# Add calculated PPM column using case_when. The new column contains the ppm values calculated from std curve
data_merged3 <- data_merged3 %>%
  mutate(
    PPM_calc = case_when(
      injvol == 3 ~ 1.9014 * AUC_30s + 16.2114,
      injvol == 2 ~ 2.8468 * AUC_30s + 32.1518,
      injvol == 1 ~ 5.7866 * AUC_30s + 90.6498,
      TRUE ~ NA_real_  # fallback for any unexpected injvol values
    )
  )

##Convert PPM CO2 to ug CO2-C per L headspace with the ideal gas law
#Cm (ug CO2-C L headspace^-1) = (Cv x M x P) / (R x T)
#Cv = ppm (volume) CO2
#M = molecular weight of C (12 ug/uumol)
#P = pressure (1 atm)
#R = universal gas constant (0.0820575 L atm / K mole)
#T = incubation temp in deg K (273.15 + deg C)
data_merged3 <- data_merged3 %>%
  mutate(
    T_K = 273.15 + 19,  # Convert to Kelvin
    CO2C_ugL = (PPM_calc * 12 * 1) / (0.0820575 * T_K)
  )


##Convert µg CO2-C L headspace-1 to µg CO2-C gram soil-1
## vessel volume is 120 ml or 0.120 L
## mass of soil used is 10 g (unless changed JAMES)
# Step 6: Convert to mg CO2-C per kg soil
# Assuming:
# - chamber volume = 0.120 L
# - soil weight = 10 g

data_merged3 <- data_merged3 %>%
  mutate(
    mg_CO2_perKgsoil = CO2C_ugL * 0.120 / 10
  )


# Create a new column for percent CO2
data_merged3 <- data_merged3 %>%
  mutate(percentCO2 = PPM_calc / 10000)

# Ensure timept is numeric
data_merged3 <- data_merged3 %>%
  mutate(timept = as.numeric(timept))

# Calculate CO2 (ml per hour per g soil i.e. respiration rate)
data_merged3 <- data_merged3 %>%
  mutate(CO2_ml_phr_pgsoil = ((percentCO2 / 100) * 120) / (timept * 10))

# Calculate microbial biomass carbon (mg per 100 g soil)
data_merged3 <- data_merged3 %>%
  mutate(mg_biomassC_p100gsoil = 40.04 * (CO2_ml_phr_pgsoil * 100) + 0.37)

#convert mg biomass C per 100 g soil to ug biomass C per g soil
data_merged3 <- data_merged3 %>%
  mutate(ugBMC_pgsoil = mg_biomassC_p100gsoil * 10)
# Calculate outliers to label
outliers <- data_merged3 %>%
  group_by(timept, tmnt) %>%
  mutate(
    Q1 = quantile(ugBMC_pgsoil, 0.25, na.rm = TRUE),
    Q3 = quantile(ugBMC_pgsoil, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = ugBMC_pgsoil < (Q1 - 1.5 * IQR) | ugBMC_pgsoil > (Q3 + 1.5 * IQR)
  ) %>%
  filter(is_outlier)

# Plot with violin, raw points, mean, and outlier labels
# Calculate outliers to label
outliers <- data_merged3 %>%
  group_by(timept, tmnt) %>%
  mutate(
    Q1 = quantile(ugBMC_pgsoil, 0.25, na.rm = TRUE),
    Q3 = quantile(ugBMC_pgsoil, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = ugBMC_pgsoil < (Q1 - 1.5 * IQR) | ugBMC_pgsoil > (Q3 + 1.5 * IQR)
  ) %>%
  filter(is_outlier)

# Plot with violin, raw points, mean, and outlier labels
ggplot(data_merged3, aes(x = factor(timept), y = ugBMC_pgsoil, fill = tmnt)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.3) +  # faint raw points
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  geom_text(
    data = outliers,
    aes(label = jar),
    position = position_jitter(width = 0.2),
    vjust = -1,
    size = 3,
    color = "red"
  ) +
  labs(
    title = "Microbial Biomass Carbon (µg/g soil) Over Time",
    x = "Timepoint",
    y = "µg Microbial Biomass C per g Soil",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )


# Remove jar 6 from analysis bc the peak wasn't detected for t1, which is what we'll use for the biomass calc
# data_merged3_filtered <- data_merged3 %>% filter(jar != 6)

# data_merged3 <- data_merged3 %>% filter(jar != 6)
# data_merged3 <- data_merged3 %>% filter(jar != 1)




# Filter to only the relevant treatments
fb_data <- data_merged3_filtered %>%
  filter(tmnt %in% c("GLU", "CAP", "BRO"))

# # Calculate mean respiration for each treatment, site, and timept
fb_summary <- fb_data %>%
  group_by(site, timept, tmnt) %>%
  summarise(mean_resp = mean(mg_CO2_perKgsoil, na.rm = TRUE), .groups = "drop")

# Reshape the data so treatments are in columns
fb_wide <- fb_summary %>%
  pivot_wider(names_from = tmnt, values_from = mean_resp)

# Calculate FBratio
fb_wide <- fb_wide %>%
  mutate(FBratio = (GLU - CAP) / (GLU - BRO))

# Filter the dataset for timepoint 1
fb_wide_tp1 <- fb_wide %>% filter(timept == 1)

# View results
print(fb_wide)


##try to plot respiration rate (this is only valid for T1)
# Filter the dataset for timepoint 1
data_tp1 <- data_merged3 %>% filter(timept == 1)
day3_df <- data_tp1 %>% mutate(day = "Day3")


# Update the ggplot
ggplot(data_tp1, aes(x = interaction(site), y = mg_CO2_perKgsoil, fill = tmnt)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  # geom_jitter(width = 0.2, size = 1, alpha = 0.3) +  # faint raw points
  # stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +
  geom_text(
    data = outliers %>% filter(timept == 1),  # only label outliers from timepoint 1
    aes(x = interaction(site, tmnt), y = mg_CO2_perKgsoil, label = jar),
    position = position_jitter(width = 0.2),
    vjust = -1,
    size = 3,
    color = "red",
    inherit.aes = FALSE
  ) +
  labs(
    title = "Respiration Rate at Timepoint 1",
    x = "Site x Treatment",
    y = "Respiration Rate (mg CO2-C / kg soil / hr)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

# Create the plot and assign it to a variable
p1 <- ggplot(data_tp1, aes(x = site, y = mg_CO2_perKgsoil, fill = tmnt)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_text(
    data = outliers %>% filter(timept == 1),
    aes(x = site, y = mg_CO2_perKgsoil, label = jar),
    position = position_jitter(width = 0.2),
    vjust = -1,
    size = 3,
    color = "red",
    inherit.aes = FALSE
  ) +
  labs(
    title = "Respiration Rate at Timepoint 1",
    x = "Site",
    y = "Respiration Rate (mg CO2-C / kg soil / hr)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

# Save the plot as a JPEG
ggsave("respiration_rate_timepoint1.jpeg", plot = p1, width = 8, height = 6, dpi = 300)


#Merge all three days of data.
#do not run the below line of code every time. I've written a .csv with the merge complete. Can use the below csv for the analyses
#alld <- bind_rows(day1_df, day2_df, day3_df)
# Save the peak metrics to a new CSV file so we don't need to do this weird merge thing every time
# write.csv(alld, "inc_all_days.csv", row.names = FALSE)
alld <-  read.csv("inc_all_days.csv", header = TRUE)

# Add vegetation type column based on site
alld <- alld %>%
  mutate(vtype = case_when(
    site %in% c("2K-C", "AQ-C", "GU-C", "PG-C", "SP-C") ~ "forested",
    site %in% c("2K-O", "AQ-O", "GU-O", "PG-O", "SP-O") ~ "open",
    TRUE ~ NA_character_  # In case there are other sites not specified
  ))

library(forcats)

# Force site order: SP, AQ, PG, 2K, GU (both -C and -O variants)
alld <- alld %>%
  mutate(site = fct_relevel(site,
                            "SP-C", "SP-O",
                            "AQ-C", "AQ-O",
                            "PG-C", "PG-O",
                            "2K-C", "2K-O",
                            "GU-C", "GU-O"))

# # Calculate mean respiration for each treatment, site, and timept
fb_summary1 <- alld %>%
  group_by(site, timept, tmnt) %>%
  summarise(mean_resp = mean(mg_CO2_perKgsoil, na.rm = TRUE), .groups = "drop")

# Reshape the data so treatments are in columns
fb_wide <- fb_summary1 %>%
  pivot_wider(names_from = tmnt, values_from = mean_resp)

# Calculate FBratio
fb_wide <- fb_wide %>%
  mutate(FBratio = (GLU - CAP) / (GLU - BRO))

# Filter the dataset for timepoint 1
fb_wide_tp1 <- fb_wide %>% filter(timept == 1)

# View results
print(fb_wide)

# Add a unique ID column (1 to 119)
alld <- alld %>%
  mutate(ID = 1:n())

library(tidyverse)

# Identify outliers by tmnt and site (you can adjust this logic as needed)
outliers <- alld %>%
  group_by(site, tmnt) %>%
  mutate(
    Q1 = quantile(mg_CO2_perKgsoil, 0.25, na.rm = TRUE),
    Q3 = quantile(mg_CO2_perKgsoil, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier = mg_CO2_perKgsoil < (Q1 - 1.5 * IQR) | mg_CO2_perKgsoil > (Q3 + 1.5 * IQR)
  ) %>%
  filter(is_outlier)

# Create the boxplot with labeled outliers
ggplot(alld, aes(x = site, y = mg_CO2_perKgsoil, fill = tmnt)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Don't show default outliers
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.3) +  # Show all raw data points faintly
  geom_text(
    data = outliers,
    aes(label = ID),
    color = "red",
    position = position_jitter(width = 0.2),
    vjust = -0.8,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Respiration Rate by Site and Treatment",
    x = "Site",
    y = expression("Respiration Rate (mg CO"[2]*"-C kg soil"^{-1}*" hour"^{-1}*")"),
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

#########Linear mixed effects analysis
# Convert columns to appropriate data types
alld <- alld %>%
  mutate(
    day = as.factor(day),
    vtype = as.factor(vtype),
    rep = as.factor(rep)
  )

# Load required package
library(lme4)      # For linear mixed effects models
library(lmerTest)  # To get p-values for fixed effects
library(MuMIn)     # For model comparison (AIC, R2)


#####Run the model and comparisons for all treatments and sites. 
#Z doesn't think it is useful to compare the MBC derived from the non-glucose treatments
# see line ~590 for the MBC analysis from glucose only data, which Z thinks is appropriate for this
#originally tried linear mixed effects models but had issues with singularity. 
#pivoted to linear models without random effects after consulting CRAN https://rdrr.io/cran/lme4/man/isSingular.html
# lmer tutorial chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://jontalle.web.engr.illinois.edu/MISC/lme4/bw_LME_tutorial.pdf

# Full model: interaction between treatment and site
#m1 <- lm(ugBMC_pgsoil ~ tmnt * site , data = alld)  ###m1 interaction term non-significant
m2 <- lm(ugBMC_pgsoil ~ tmnt + site , data = alld) #######m2 is the optimal model for microbial biomass based on comparison of models below
m3 <- lm(ugBMC_pgsoil ~ tmnt , data = alld)
m4 <- lm(ugBMC_pgsoil ~ site , data = alld)

m2 <- lm(mg_biomassC_p100gsoil ~ tmnt + site , data = alld) #######m2 is the optimal model for microbial biomass based on comparison of models below
m3 <- lm(mg_biomassC_p100gsoil ~ tmnt , data = alld)
m4 <- lm(mg_biomassC_p100gsoil ~ site , data = alld)


# Compare AIC values
AIC(m1, m2, m3, m4)

anova(m2, m4)  # Compare additive model vs interaction model
anova(m2, m3)
anova(m3, m2)  # Compare treatment-only model vs additive model

anova(m2)  # for model terms (tmnt, site, tmnt:site)

library(emmeans)
emmeans(m4, pairwise ~ tmnt | site)  # or ~ site | tmnt

# View model summary
summary(m1)  # This gives estimates and p-values for fixed effects


# Residual plot
plot(model_bmc)

# QQ plot of residuals
qqnorm(resid(model_bmc))
qqline(resid(model_bmc))

# Histogram of residuals
hist(resid(model_bmc), breaks = 20, main = "Residuals", xlab = "Residual value")


### filtering out non-glucose treatments for mbc analysis

# Create a new column 'site1' with only the first two letters from 'site'
alld <- alld %>%
  mutate(site1 = sub("-.*", "", site))

# Step 1: Filter data to only include GLU treatment
glu_data <- alld %>% filter(tmnt == "GLU")

comparisons <- combn(unique(as.character(interaction(glu_data$site1, glu_data$vtype))), 2, simplify = FALSE)

# Step 2: Fit linear models
m_glu1 <- lm(ugBMC_pgsoil ~ site1 * vtype, data = glu_data)  # Model with interaction
m_glu2 <- lm(ugBMC_pgsoil ~ site1 + vtype, data = glu_data)  # Model without interaction

# Step 3: View summaries of models
summary(m_glu1)
summary(m_glu2)

# Step 4: Compare models using ANOVA
anova(m_glu2, m_glu1)  # m_glu2 is nested within m_glu1

# Step 5: Check model assumptions
# QQ plot of residuals
qqnorm(residuals(m_glu1))
qqline(residuals(m_glu1))

# Histogram of residuals
hist(residuals(m_glu1), main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk test for normality
shapiro.test(residuals(m_glu1))

# Step 6: Pairwise comparisons of sites using estimated marginal means
library(emmeans)
emm <- emmeans(m_glu1, ~ site1 * vtype)

# Extract the pairwise contrasts
contrast_table <- contrast(emm, method = "pairwise")

# Sort contrasts by p-value
sorted_contrasts <- as.data.frame(contrast_table) %>%
  arrange(p.value)

# View sorted contrasts
print(sorted_contrasts)

# Step 7: Log transformation of microbial biomass
glu_data <- glu_data %>%
  mutate(log_ugBMC_pgsoil = log(ugBMC_pgsoil))

# Step 8: Refit linear models using log-transformed data
m_glu1_log <- lm(log_ugBMC_pgsoil ~ site1 * vtype, data = glu_data)  # Model with interaction
m_glu2_log <- lm(log_ugBMC_pgsoil ~ site1 + vtype, data = glu_data)  # Model without interaction

# Step 9: View summaries of log-transformed models
summary(m_glu1_log)
summary(m_glu2_log)

# Step 10: Compare log-transformed models using ANOVA
anova(m_glu2_log, m_glu1_log)

# Step 11: Check assumptions on log-transformed model
qqnorm(residuals(m_glu1_log))
qqline(residuals(m_glu1_log))

hist(residuals(m_glu1_log), main = "Histogram of Residuals (log-transformed)", xlab = "Residuals")

shapiro.test(residuals(m_glu1_log))


# Step 12: Pairwise comparisons using log model
emm_log <- emmeans(m_glu1_log, ~ site1 * vtype)
contrast_table_log <- contrast(emm_log, method = "pairwise")
sorted_contrasts_log <- as.data.frame(contrast_table_log) %>%
  arrange(p.value)
print(sorted_contrasts_log)

# Step 12: Pairwise comparisons using original (non-log) model
emm_orig <- emmeans(m_glu1, ~ site1 * vtype)
contrast_table_orig <- contrast(emm_orig, method = "pairwise")
sorted_contrasts_orig <- as.data.frame(contrast_table_orig) %>%
  arrange(p.value)
print(sorted_contrasts_orig)

# Step 13: Plot non-log-transformed microbial biomass with stat_compare_means
library(ggplot2)
library(ggpubr)

# Force specific order of site1 factor levels
glu_data$site1 <- factor(glu_data$site1, levels = c("SP", "AQ", "PG", "2K", "GU"))

# Generate boxplot without significance annotations
ggplot(glu_data, aes(x = interaction(site1), y = ugBMC_pgsoil, fill = vtype)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(
    aes(fill = vtype),
    shape = 21,
    color = "black",
    position = position_jitter(width = 0, height = 0.1),
    size = 2,
    alpha = 0.3
  ) +
  labs(
    x = "Site",
    y = expression("Microbial Biomass C ("*mu*"g g"^{-1}~"soil)"),
    fill = "Vegetation Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )
