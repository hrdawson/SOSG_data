library(readxl)
library(lubridate)

path = "raw_data/Costin Throughfall data for R.xlsx"

costin <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(~ read_excel(path = path, sheet = .x), .id = "sheet") |>
  # Pivot treatment
  pivot_longer(cols = forested:open, names_to = "treatment", values_to = "precip_in") |>
  # Replace 'l's with '1's
  mutate(precip_in = str_replace_all(precip_in, "l", "1"),
         #Make numeric
         precip_in = as.numeric(precip_in),
         # Make R readable dates
         date.R = ymd(paste0(year, "-", month, "-", day))
         ) |>
  drop_na(precip_in) |>
  filter(precip_in > 0) |>
  # Turn fog binary
  mutate(fog = case_when(
    fog == "fog" ~ "yes",
    TRUE ~ "no"
  )) |>
  # Add average wind speed
  mutate(wind.speed.avg = (wind.speed.min + wind.speed.max)/2,
         wind.direction = case_when(
    wind.direction == "NA" ~ "none",
    TRUE ~ wind.direction
  ),
  weather = replace_na(weather, "no precip"))


costin.comp = costin |>
  filter(complete.equal == TRUE) |>
  # Scale by leaf area and weight
  mutate(precip.leafArea = case_when(
    site == "Sawpit" ~ precip_in/394,
    site == "Boggy Plains" ~ precip_in/360,
    site == "Pipers Gap" ~ precip_in/210,
    site == "Charlotte Pass" ~ precip_in/138),
    precip.leafWt = case_when(
      site == "Sawpit" ~ precip_in/32,
      site == "Boggy Plains" ~ precip_in/29,
      site == "Pipers Gap" ~ precip_in/17,
      site == "Charlotte Pass" ~ precip_in/11)
  )

length(table(costin.comp$date.R))


ggplot(costin, aes(x = site, y = precip_in, color = treatment, fill = treatment)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_y_log10() +
  theme_bw()

ggsave("visualizations/2024.02.05_CostinViz.png", width = 6, height = 8, units = "in")

library(car)
library(lmerTest)

costin.lm = lmer(precip_in ~ treatment + wind.direction + wind.speed.avg + weather + fog +
               (1|site),
                 data = costin)
Anova(costin.lm, Type = "II")

lmer(precip_in ~ treatment + wind.speed.min + wind.speed.max + weather + fog +
       (1|site),
     data = costin) |>
tukey_hsd(precip_in ~ weather)
