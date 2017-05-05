## bash entry
cd Desktop/Arboretum
R

# read in point data csv files
# '\t' when from Windows, ',' when from Mac
gbif_points <- read.csv(file='./sp_occ/datasets_to_merge/GBIF_Quercus_AutoFilter.csv',
    as.is=TRUE, sep =',',row.names=NULL, strip.white = TRUE);
nrow(gbif_points)
andrew_points <- read.csv(file='./sp_occ/datasets_to_merge/all.eco.data.exportedFromR.2016-02-03-noFIA.csv',
    as.is=TRUE, sep=',', row.names=NULL, strip.white = TRUE);
nrow(andrew_points)
# read in csv rl species point files from folder and create data frame for each. held within list
setwd("./sp_occ/datasets_to_merge/rl_sheets")
file_list <- list.files(pattern = ".csv")
gsub("Q_", "", file_list)
file_dfs <- sapply(file_list, read.csv, header = TRUE, fileEncoding="Latin1", colClasses = "character")
length <- length(file_dfs)
length
setwd("./../..")
setwd("./merged_data")
# FUNCTION to match up column headers and keep all columns of the larger file, stacking
  # added columns will be filled with NA
  # SOURCE: https://amywhiteheadresearch.wordpress.com/2013/05/13/combining-dataframes-when-the-columns-dont-match/
rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    return(rbind(x, y))
}
# call function. Reduce iterates through list and merges with previous df
us_rl <- Reduce(rbind.all.columns, file_dfs)
us_rl$Name <- gsub("Quercus\\s", "", us_rl$Name)
nrow(us_rl)
write.csv(us_rl, file = "us_rl.csv")

# create FIA dataset
setwd("./../..")
file_list <- list.files(path = "./sp_occ/datasets_to_merge/FIA_CSVs", pattern = ".csv", full.names = TRUE)
file_dfs <- lapply(file_list, read.csv, header = TRUE, fileEncoding="latin1", strip.white = TRUE, colClasses = "character")
length <- length(file_dfs)
length
fia_points <- data.frame()
for(file in seq_along(file_dfs)){
  fia_points <- rbind(fia_points, file_dfs[[file]])
}
fia_points$SCINAME <- gsub("Quercus\\s", "", fia_points$SCINAME)
nrow(fia_points)
# remove rows calculated by county centroid
fia_points <- subset(fia_points, fia_points$CENTROID != "county")
nrow(fia_points)
write.csv(fia_points, file = "./sp_occ/merged_data/fia_points.csv")
nrow(fia_points)
str(fia_points)

# add missing columns and fill with NA
andrew_points$year <- rep(1950)
andrew_points$basis <- rep("NA")
fia_points$basis <- rep("NA")
andrew_points$locality <- rep("NA")

str(gbif_points)
str(andrew_points)
str(us_rl)
str(fia_points)
#combine all datasets using only shared variables. see bottom of script for column name searches
occur_all <- data.frame(
    dataset = c(rep('gbif', nrow(gbif_points)), rep('andrew_hipp', nrow(andrew_points)), rep('redlist', nrow(us_rl)), rep('fia', nrow(fia_points))),
    species = c(gbif_points$species, andrew_points$species, us_rl$Name, fia_points$SCINAME),
    lat = c(gbif_points$latitude, andrew_points$latitude, us_rl$Lat, fia_points$YCoord),
    long = c(gbif_points$longitude, andrew_points$longitude, us_rl$Long, fia_points$XCoord),
    basis = c(gbif_points$basisofrecord, andrew_points$basis, us_rl$Basis, fia_points$basis),
    year = c(gbif_points$year, andrew_points$year, us_rl$Ev_Year, fia_points$YEAR),
    locality=c(gbif_points$locality, andrew_points$locality, us_rl$Local, fia_points$CALCCNTY),
    source = c(gbif_points$institutioncode, andrew_points$Source, us_rl$Source, fia_points$CALCSTATE))
nrow(occur_all)
#replace commas with periods in locality
occur_all$locality <- gsub(",", ".", occur_all$locality)

occur_all[] <- lapply(occur_all, as.character)
occur_all$year <- as.numeric(occur_all$year)
unique(occur_all$year)
occur_all$species <- as.factor(occur_all$species)
occur_all$dataset <- as.factor(occur_all$dataset)
str(occur_all)

# remove points with less than 2 digits after the decimal for lat and long
occur_dec <- occur_all[grep("\\.[0-9][1-9]", occur_all$lat), ]
nrow(occur_dec)
occur_dec2 <- occur_dec[grep("\\.[0-9][1-9]", occur_dec$long), ]
nrow(occur_dec2)
# remove points taken before 1950
occur_dec2 <- subset(occur_dec2,
      year >= 1950
      )
nrow(occur_dec2)
write.csv(occur_dec2, file = "./sp_occ/merged_data/occur_dec2.csv")
# remove duplicates
library(data.table)
library(dplyr)
count.dups <- function(DF){
  DT <- data.table(DF)
  DT[,.N, by = names(DT)]
}
occur_dec2_unq <- count.dups(occur_dec2) %>% distinct(species, lat, long, year, .keep_all = TRUE)
nrow(occur_dec2_unq)
write.csv(occur_dec2_unq, file = "./sp_occ/merged_data/occur_dec2_unq.csv")

# read in species list csv. the subset you want in final dataset
sp_of_concern <- read.csv('./sp_occ/species_lists/sp_of_concern.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
sp_us_all <- read.csv('./sp_occ/species_lists/sp_us_all.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
# FUNCTION to create subset of data based on sp name file just read in
sp_subset <- function(pointdata, sp, name){
  selected_rows <- (pointdata$species %in% sp$species)
  newdata <- pointdata[selected_rows,]
  write.table(newdata, name,
  append = FALSE, quote = FALSE,
  sep = ",", eol = "\n", na = "NULL",
  dec = ".", row.names = FALSE, col.names = TRUE)
  return(data.frame(newdata))
}
# call function
setwd("./sp_occ/merged_data")
all_us_2 <- sp_subset(occur_dec2_unq, sp_us_all, "all_us_dec2_unq.csv") # NEEDS FIXING - creating file for every species, not US sp_subset
all_us_2 <- droplevels(all_us_2)
nrow(all_us_2)
concern_us_2 <- sp_subset(occur_dec2_unq, sp_of_concern, "concern_us_dec2_unq.csv")
concern_us_2 <- droplevels(concern_us_2)
nrow(concern_us_2)
library(plyr)
concern_sp_count <- data.frame(count(concern_us_2$species))
concern_sp_count
write.csv(concern_sp_count, file = "./count_concern_us_dec2_unq.csv")

# create list of data frames separated by species name and create CSV for each
setwd("./..")
setwd("./merged_data/separated_by_sp/concern_us")
concern_us_2 <- split(concern_us_2, concern_us_2$species)
sapply(names(concern_us_2),
  function (x) write.csv(concern_us_2[[x]], file=paste(x, ".csv")))
setwd("./..")
## NOT WORKING
setwd("./all_us")
occur_dec2_unq_sp <- split(occur_dec2_unq, occur_dec2_unq$species)
sapply(names(occur_dec2_unq_sp),
  function (x) write.csv(occur_dec2_unq_sp[[x]], file=paste(x, ".csv")))

setwd("./..")








## just informational.. DONT RUN:
  # get all column names for each file (find relevant fields..)
str(us_rl)
Name
Year
Lat
Long
Basis
Ev_Year
Source
Local
Country
Coll_ID
Cat_No
Rec_No
Rec_By
Uncert
Sens
Source2
State
str(gbif_points)
species
latitude
longitude
taxonrank
scientificname
countrycode
locality
coordinateuncertaintyinmeters
year
basisofrecord
institutioncode
catalognumber
recordnumber
recordedby
lastinterpreted_year
str(andrew_points)
Source
collection
latitude
longitude
species
