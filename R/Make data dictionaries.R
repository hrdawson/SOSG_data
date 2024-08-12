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
get_started(data = read.csv("clean_data/Permanent plot trees.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_treeData2023 <- make_data_dictionary(data = read.csv("clean_data/Tree data 2023.csv"),
                                        description_table = read.csv("data_dic/description_table_TreeData_2023.csv"),
                                        table_ID = "tree",
                                        keep_table_ID = FALSE)
write.csv(data_dic_treeData, "data_dic/dataDic_treeData_2023.csv")

# Cover data dic ----
# Start by creating a template CSV
get_started(data = read.csv("clean_data/5x5 subplot data.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Cover data dic ----
# Start by creating a template CSV
get_started(data = read.csv("clean_data/Permanent plot metadata.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

## 1x1 subplot data ----
# Make sure to save it as something other than its default name
data_dic_1x1 <- make_data_dictionary(data = read.csv("clean_data/1x1 cover data.csv"),
                                        description_table = read.csv("data_dic/description_table_1x1.csv"),
                                        table_ID = "1x1",
                                        keep_table_ID = FALSE)

write.csv(data_dic_1x1, "data_dic/dataDic_1x1.csv")

## 5x5 subplot data ----
# Make sure to save it as something other than its default name
data_dic_5x5 <- make_data_dictionary(data = read.csv("clean_data/5x5 subplot data.csv"),
                                     description_table = read.csv("data_dic/description_table_5x5.csv"),
                                     table_ID = "5x5",
                                     keep_table_ID = FALSE)

write.csv(data_dic_5x5, "data_dic/dataDic_5x5.csv")

## Permanent plot tree data ----
## This one is derived from the `Canopy health class.R` script
# Make sure to save it as something other than its default name
data_dic_permanentTrees <- make_data_dictionary(data = read.csv("clean_data/Permanent plot trees.csv"),
                                     description_table = read.csv("data_dic/description_table_permanentTrees.csv"),
                                     table_ID = "permanent",
                                     keep_table_ID = FALSE)

write.csv(data_dic_permanentTrees, "data_dic/dataDic_permanentTrees.csv")

## Permanent plot meta data ----
# Make sure to save it as something other than its default name
data_dic_plotMeta <- make_data_dictionary(data = read.csv("clean_data/Permanent plot metadata.csv"),
                                                description_table = read.csv("data_dic/description_table_plotMeta.csv"),
                                                table_ID = "meta",
                                                keep_table_ID = FALSE)

write.csv(data_dic_plotMeta, "data_dic/dataDic_plotMeta.csv")
