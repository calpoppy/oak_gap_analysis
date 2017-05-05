
## FUNCTIONS:

# matches up column headers, keeping all columns, not just matching ones [stacking]
# (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}

## BEGIN FILE COMPILATION:

# read in ex situ accessions CSV files from folder and create data frame for each
file_list <- list.files(path = "./exsitu_data/updated_csv_files", pattern = ".csv", full.names = TRUE)
file_dfs <- sapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length(file_dfs)

# call merge/stack function
    # 'Reduce' iterates through list and merges with previous dataframe in the list
all_data <- Reduce(rbind.all.columns, file_dfs)
str(all_data)
nrow(all_data)

# inst_short [nickname I've created for each institution] column added, based on file name
for(file in seq_along(file_dfs)){
  file_dfs[[file]]$inst_short <- rep(file_list[file], nrow(file_dfs[[file]]))
}

# update inst_short column to contain only garden name, not path
all_data$inst_short <- sub("./exsitu_data/updated_csv_files/", "", all_data$inst_short)
all_data$inst_short <- sub(".csv", "", all_data$inst_short)
unique(all_data$inst_short)

# call file merging/stacking function
all_data <- Reduce(rbind.all.columns, file_dfs) # 'Reduce' iterates through list and merges with previous
str(all_data) # shows schema
nrow(all_data)

# remove extra columns (created through Excel to CSV issues)
all_data <- all_data[, -grep("^X", names(all_data))]
str(all_data) # check schema to see if problems still exist

# export dataframe (CSV)
write.csv(all_data, file = "./exsitu_data/exsitu_all_data.csv")
