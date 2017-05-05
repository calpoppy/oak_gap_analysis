
# FUNCTION
# matches up column headers, keeping all columns not just matching ones [stacking]
# (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}

# read in ex situ accessions csv files from folder and create data frame for each
file_list <- list.files(path = "./exsitu_data/updated_csv_files", pattern = ".csv", full.names = TRUE)
file_dfs <- sapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length(file_dfs)

# inst_short col added, based on file name
for(file in seq_along(file_dfs)){
  file_dfs[[file]]$inst_short <- rep(file_list[file], nrow(file_dfs[[file]]))
}

# call function
# 'Reduce' iterates through list and merges with previous
all_data <- Reduce(rbind.all.columns, file_dfs)
str(all_data)
nrow(all_data)

# update inst_short column to contain only garden name, not path
all_data$inst_short <- sub("./exsitu_data/updated_csv_files/", "", all_data$inst_short)
all_data$inst_short <- sub(".csv", "", all_data$inst_short)
unique(all_data$inst_short)

# remove extra columns (created through Excel to CSV issues)
all_data <- all_data[, -grep("^X", names(all_data))]
str(all_data)

# export dataframe (CSV)
write.csv(all_data, file = "./exsitu_data/exsitu_all_data.csv")
