sp_of_concern <- read.csv('./sp_occ/species_lists/sp_of_concern.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
us_sp_rl <- read.csv('./sp_occ/species_lists/us_sp_rl.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)

new_list <- join(sp_of_concern, us_sp_rl, by = "species", type="right")

write.csv(new_list, file = "./sp_occ/species_lists/total_list.csv")


#plotting
acc_per_sp <- read.csv('./sp_occ/species_lists/total_acc_per_sp_edited.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
plot(acc_per_sp$threat_value, acc_per_sp$no_acc, main="Ex situ Collections: U.S. Native Oaks",
  xlab="Threat Rating; 1(lowest) - 6(highest)", ylab="Number of Accessions Globally", pch=19)
#add regression fit line
abline(lm(acc_per_sp$no_acc~acc_per_sp$threat_value), col="red")






###Join (stack) files

# FUNCTION
# matches up column headers, keeping all columns not just matching ones [stacking]
# (fills added columns with NAs)
    # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    #n <- max(length(x), length(y))
    #length(x) <- n
    #length(y) <- n
    return(rbind(x, y))
}

# read in csv files
occur <- read.csv('./sp_occ/threatened_occur_all.csv',
        as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
threats <- read.csv('./sp_occ/major_threats_USoaks_new.csv',
        as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
# join
library(plyr)
all <- join(occur, threats, by = "species", type = "full")

file_dfs <- c(occur1, occur2, occur3, occur4, occur5, occur6, occur7, occur8, occur10, occur11, occur12, occur13, occur14, occur15, occur16)
#file_dfs <- sapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length(file_dfs)

# call function
# 'Reduce' iterates through list and merges with previous
all_data <- Reduce(cbind, file_dfs)
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
