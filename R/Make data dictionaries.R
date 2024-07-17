# More info on making data dictionaries
# https://github.com/audhalbritter/dataDocumentation
# Note that we're using a script modified by HRD, not the originals by Aud

# load functions
source("R/functions/make_data_dic.R")
source("R/functions/get_started_data_dic.R")
library(tidyverse)
library(readxl)

# Raw data dictionaries for reference when I return
get_started(data = read_excel("raw_data/SOSG tree data 2024.xlsx", sheet = "data"))



# Cover data dic ----
# Start by creating a template CSV
get_started(data = cover.data)

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_fluxes <- make_data_dictionary(data = cover.data,
                                        description_table = read.csv("data_dic/description_table_cover.csv"),
                                        table_ID = "cover",
                                        keep_table_ID = FALSE)
write.csv(data_dic_fluxes, "data_dic/dataDic_cover.csv")

# Cover data dic ----
# Start by creating a template CSV
get_started(data = cover.data)

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_fluxes <- make_data_dictionary(data = cover.data,
                                        description_table = read.csv("data_dic/description_table_cover.csv"),
                                        table_ID = "cover",
                                        keep_table_ID = FALSE)
write.csv(data_dic_fluxes, "data_dic/dataDic_cover.csv")
