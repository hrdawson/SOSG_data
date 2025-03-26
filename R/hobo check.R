hobo_columns = c("obs", "datetime", "T1", "T2", "T3", "T4", "Tdevice", "host", "stopped", "endfile")

hobo = read.csv("raw_data/2024.10.07_HoboTest.csv", skip = 2, header = FALSE) |>
  setNames(hobo_columns) |>
  select(-c("host", "stopped", "endfile")) |>
  mutate(datetime = lubridate::ymd_hms(datetime)) |>
  filter(datetime > ymd("2011-07-23")) |>
  pivot_longer(cols = T1:Tdevice, names_to = "variable", values_to = "value")

ggplot(hobo,
       aes(x = datetime, y = value, colour = variable)) +
  geom_point() +
  theme_bw()
