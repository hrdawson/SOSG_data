# Import tree-level data
# Read in files ----
# Make file list
filesTrees <- dir(path = "raw_data/fixed area plot (permanent plot) data", pattern = ".xlsx",
                  full.names = TRUE, recursive = TRUE)

# Read in data
tempTrees = map_dfr(filesCover, read_xlsx, sheet = "Tree-level", skip = 1,
                    col_names = FALSE)

# Clean data ----
