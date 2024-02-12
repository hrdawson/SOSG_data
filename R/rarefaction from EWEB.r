library(vegan)
library(Hotelling)
library(readxl)

##This is the iNEXT that actually works ----
# Format the data for subplot analysis ----
subplot.raw = read.csv("clean_data/5x5 subplot data.csv") |>
  select(plot, midlineDistance, species, coverPercent) |>
  # Create unique subplot names
  mutate(subplot = paste0(plot, ".", midlineDistance)) |>
  # Duplicate species in some subplots
  # But we don't care what value they have for these analyses
  mutate(value = case_when(
    coverPercent > 0 ~ '1',
    TRUE ~ NA
  )) |>
  select(-coverPercent) |>
  distinct() |>
  mutate(value = as.numeric(str_trim(value))) |>
  # Pivot species
  pivot_wider(names_from = species, values_from = value) |>
  select(-c(plot, midlineDistance)) |>
  # Transpose
  t() |>
  janitor::row_to_names(1) %>%
  # Turn binary
  replace(is.na(.), 0)

incid = list()

#Low dieback severity
incid[[1]] = as.data.frame(subplot.raw) |> select('1.10':'10.40') |> mutate_if(is.character,as.numeric)

# Moderate dieback severity
incid[[2]] = as.data.frame(subplot.raw) |> select('11.10':'12.40', '17.10':'18.40') |> mutate_if(is.character,as.numeric)

# High dieback severity
incid[[3]] = as.data.frame(subplot.raw) |> select('13.10':'16.40', '19.10':'20.40') |> mutate_if(is.character,as.numeric)

names(incid) = c("low", "moderate", "severe")
rownames(incid[["low"]]) = incid[["low"]][["plot"]]; incid[["low"]][["species"]] = NULL
rownames(incid[["moderate"]]) = incid[["moderate"]][["plot"]]; incid[["moderate"]][["species"]] = NULL
rownames(incid[["severe"]]) = incid[["severe"]][["plot"]]; incid[["severe"]][["species"]] = NULL

rare.inext = iNEXT(incid, datatype = "incidence_raw", endpoint = 56)

png("inext.speciesdiversity.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 1)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

png("inext.samplecoverage.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 2)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

# Format the data for plot analysis ----
plot.raw = read.csv("clean_data/5x5 subplot data.csv") |>
  select(plot, species, coverPercent) |>
  # Duplicate species in some subplots
  # But we don't care what value they have for these analyses
  mutate(value = case_when(
    coverPercent > 0 ~ '1',
    TRUE ~ NA
  ),
  plot = str_trim(plot)) |>
  select(-coverPercent) |>
  distinct() |>
  # Pivot species
  pivot_wider(names_from = species, values_from = value) |>
  # Transpose
  t() |>
  janitor::row_to_names(1) %>%
  # Turn binary
  replace(is.na(.), 0)

incid = list()

#Low dieback severity
incid[[1]] = as.data.frame(plot.raw) |> select('1':'10') |> mutate_if(is.character,as.numeric)

# Moderate dieback severity
incid[[2]] = as.data.frame(plot.raw) |> select('11':'12', '17':'18') |> mutate_if(is.character,as.numeric)

# High dieback severity
incid[[3]] = as.data.frame(plot.raw) |> select('13':'16', '19':'20') |> mutate_if(is.character,as.numeric)

names(incid) = c("low", "moderate", "severe")
rownames(incid[["low"]]) = incid[["low"]][["plot"]]; incid[["low"]][["species"]] = NULL
rownames(incid[["moderate"]]) = incid[["moderate"]][["plot"]]; incid[["moderate"]][["species"]] = NULL
rownames(incid[["severe"]]) = incid[["severe"]][["plot"]]; incid[["severe"]][["species"]] = NULL

rare.inext = iNEXT(incid, datatype = "incidence_raw", endpoint = 40)

png("inext.speciesdiversity.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 1)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

png("inext.samplecoverage.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 2)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()
