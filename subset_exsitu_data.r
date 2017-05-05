setwd("./Desktop/Arboretum")


## IMPORT PACKAGES:

library(plyr); library(shiny); library(stringr); library(dplyr); library(data.table)

## FUNCTIONS :

# subset data and (optionally!) write a CSV
gen_subset <- function(orig_data, action, export_name){
  selected_rows <- (action)
  new <- orig_data[selected_rows,]
  if (missing(export_name)){
    return(data.frame(new))
    } else {
      write.csv(new, file = export_name)
      return(data.frame(new))
    }
}

# count records by unique values in given column, return in ascending order of frequency
  # and (optionally!) write a CSV
count_records <- function(dataset, col, export_name){
  new <- count(dataset, col)
  new <- new[with(new, order(freq)),]
  if (missing(export_name)){
    return(data.frame(new))
    } else {
      write.csv(new, file = export_name)
      return(data.frame(new))
    }
}

#  ------- not currently used! ---------
# use when you do not want to keep non-shared columns
# heres how it works:
  # 1. Specify the input dataframes
  # 2. Calculate which dataframe has the greatest number of columns
  # 3. Identify which columns in the smaller dataframe match the columns in the larger dataframe
  # 4. Create a vector of the column names that occur in both dataframes
  # 5. Combine the data from both dataframes matching the listed column names using rbind
  # 6. Return the combined data
rbind.match.columns <- function(input1, input2) {
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
    } else {
        TF.names <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
    }
    return(rbind(input1[, column.names], input2[, column.names]))
}

# BEGIN : Create Data Subsets

 # 1) Read in datasets from ***Dataiku***, with species column added
   # The analysis and creation of the "species" column is housed in Dataiku [Data Science Studio] right now,
   # but should probably be converted to R at some point!!
   # The initial input for Dataiku is the file created through "compile_exsitu_data.r"
     # species column has been added to this dataset:
all_data_scrub1 <- read.csv('./exsitu_data/exsitu_all_data_10_prepared.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
     # species column has been standardized, and rows REMOVED for hybrids, unknowns, etc.
all_data_spscrub <- read.csv('./exsitu_data/exsitu_all_data_10_prepared_species.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
     # print number of records for each insitution, before and after Dataiku species cleaning
#count(all_data_scrub1$inst_short) -- not working now??? was working before and I don't think I changed anything...
#count(all_data_spscrub$inst_short)
unique(all_data_scrub1$inst_short)

# 2) Join to [merge with] Institution Data Table
  # ----------- this is not quite right! for the institutions with the same "inst_short" name,
  # but unique inst_short2 [like in Quercus PCN], duplicates of every accession are made for every "inst_short2"
#inst_data <- read.csv('./exsitu_data/InstitutionDataTable.csv',
    #as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
#all_data_spscrub <- join(all_data_spscrub, inst_data, by = "inst_short", type = "full")

# 3) Subset by species name
  # read in species of interest list(s)
sp_of_concern <- read.csv('./sp_occ/species_lists/sp_of_concern.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
sp_us_all <- read.csv('./sp_occ/species_lists/sp_us_all.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE);
  # call subset function
    # only accessions for US oaks will remain :)
us_all <- gen_subset(all_data_spscrub, (all_data_spscrub$species %in% sp_us_all$species), "./exsitu_data/us_subsets/all_data_US.csv")
nrow(us_all)
unique(us_all$inst_short)
          #count(us_all$inst_short) --- not working.
          #count(us_all$species)
    # only accessions for US oaks of concern will remain :)
us_concern <- gen_subset(all_data_spscrub, (all_data_spscrub$species %in% sp_of_concern$species), "./exsitu_data/concern_subsets/all_data_USconcern.csv")
nrow(us_concern)
unique(us_concern$inst_short)
          #count(us_concern$inst_short) --- not working.
          #count(us_all$species)

## continue with species of concern subset only --->

# 4) Subset and clean up lat and long coordinates
  # add columns for determination of lat-long
us_concern$gps_det <- as.character(NA)
us_concern$gps_prec_place <- as.character(NA)
us_concern$gps_prec_value_km2 <- as.numeric(NA)
us_concern$lat <- as.numeric(us_concern$orig_lat)
us_concern$long <- as.numeric(us_concern$orig_long)
  # records with valid lat/long values
conc_has_latlong <- gen_subset(us_concern,!is.na(us_concern$lat))
conc_has_latlong$gps_det <- "G"
nrow(conc_has_latlong)
  # switch lat and long values if lat is greater than 50 or less than 0
for (r in seq_along(conc_has_latlong$lat)){
  temp <- conc_has_latlong$lat[r]
  if (temp > 50 || temp < 0){
    conc_has_latlong$lat[r] <- conc_has_latlong$long[r]
    conc_has_latlong$long[r] <- temp
  }
}
  # add a minus sign if the longitude is positive; write csv
for (r in seq_along(conc_has_latlong$long)){
  if (conc_has_latlong$long[r] > 0){
      conc_has_latlong$long[r] <- -1 * conc_has_latlong$long[r]
  }
}
write.csv(conc_has_latlong, file = "./exsitu_data/concern_subsets/has_latlong.csv")
  # records needing dms to dd conversion
conc_conv_ll <- gen_subset(us_concern, (us_concern$dd_conv_needed == "x"))
conc_conv_ll$gps_det <- "M"
write.csv(conc_conv_ll, file = "./exsitu_data/concern_subsets/needs_convert.csv")
nrow(conc_conv_ll)
  # remove rows already exported through 'has_latlong'
us_concern_loc <- gen_subset(us_concern, (us_concern$latlong_geopoint == ""))
nrow(us_concern_loc)
  # remove rows already exported through 'needs_convert'
us_concern_loc2 <- gen_subset(us_concern_loc, (us_concern_loc$dd_conv_needed == ""))
nrow(us_concern_loc2)
write.csv(us_concern_loc2, file = "./exsitu_data/concern_subsets/us_concern_loc2.csv")

# 5) Subset by locality info
  # remove rows with no locality info
us_concern_loc3 <- gen_subset(us_concern_loc2, !is.na(us_concern_loc2$all_locality))
nrow(us_concern_loc3)
write.csv(us_concern_loc3, file = "./exsitu_data/concern_subsets/us_concern_loc3.csv")
  # remove rows with duplicate locality descriptions, and sum no_alive [number of individuals alive] for these deleted rows
us_concern_unq <- aggregate(no_alive ~ all_locality+us_county+us_state+inst_short+inst_short2+species+specific, 
   data = us_concern_loc3, FUN = sum)
nrow(us_concern_unq)
str(us_concern_unq)
  # write a final CSV
write.csv(us_concern_unq, file = "./exsitu_data/concern_subsets/us_concern_unq_locality.csv")

## STATISTICS : institutions and accessions - saved to CSVs
  
  # all US oak species
    # number of accessions in each institutions' dataset
acc_per_inst_us <- count(us_all, inst_short)
write.csv(acc_per_inst_us, file = "./exsitu_data/us_subsets/acc_per_inst_us.csv")
    # number of accesssions of each species
acc_per_sp_us <- count(us_all, species)
write.csv(acc_per_sp_us, file = "./exsitu_data/us_subsets/acc_per_sp_us.csv")
    # load US oak species list that includes --threat-- data; join to accessions per species dataframe just created
total_list <- read.csv('./sp_occ/species_lists/total_list.csv',
    as.is=TRUE, sep=',',row.names=NULL, strip.white = TRUE)
joined <- join(acc_per_sp_us, total_list, by = "species", type="right")
write.csv(joined, file = "./sp_occ/species_lists/total_acc_per_sp.csv")
    # MOST HOLISTIC STAT (if you just want to do one)
acc_per_sp_per_inst <- ddply(us_all, .(us_all$species, us_all$inst_short, us_all$inst_short2, us_all$no_alive), nrow)
names(acc_per_sp_per_inst) <- c("species","inst_short","inst_short2","no_alive","Freq")
write.csv(acc_per_sp_per_inst, file = "./exsitu_data/us_subsets/acc_per_sp_per_inst2_us.csv")
  # species of concern
#acc_per_inst_conc <- count(us_concern, inst_short)
#acc_per_inst2_conc <- count(us_concern, inst_short2)
acc_per_sp_conc <- count(us_concern, species)
write.csv(acc_per_sp_conc, file = "./exsitu_data/concern_subsets/acc_per_sp.csv")
acc_per_sp_per_inst <- ddply(us_concern, .(us_concern$species, us_concern$inst_short, us_concern$inst_short2, us_concern$no_alive), nrow)
names(acc_per_sp_per_inst) <- c("species","inst_short","inst_short2","no_alive","Freq")
write.csv(acc_per_sp_per_inst, file = "./exsitu_data/concern_subsets/acc_per_sp_per_inst2_concern.csv")


########################## CURRENT END ##############################

## Was just playing with ShinyApp and summary tables; no longer works, used old variables

# Create a nice table of record numbers
  # create data frame of values to display
gen_subset_v <- c(rep("Total Records", times = 3), rep("Lat/Long Given", times = 3), rep("Wild Provenance", times = 3))
sp_subset_v <- rep(c("All Species", "All US", "US Concern"), times = 3)
no_records_v <- c(nrow(all_data_spscrub), nrow(us_all), nrow(us_concern),
  nrow(has_geo), nrow(us_geo), nrow(us_concern_geo),
  nrow(all_prov_w), nrow(us_prov_w), nrow(concern_prov_w))
table <- (data.frame("Values Subset" = gen_subset_v,
  "Species Subset" = sp_subset_v,
  "Num Records" = no_records_v))

# create app to display summary table

ui <- fluidPage(tableOutput("nrow_stats"))
server <- function(input, output){
  output$nrow_stats <- renderTable({
    title <- "Number of Records"
    table})
}
shinyApp(ui = ui, server = server)
  # Ctrl,C to exit

  # Count of US sp records by institution
ui <- fluidPage(tableOutput("inst_count"))
server <- function(input, output){
  output$inst_count <- renderTable({
    inst_with_us})
}
shinyApp(ui = ui, server = server)

  # Count of records by US species
ui <- fluidPage(tableOutput("sp_count"))
server <- function(input, output){
  output$sp_count <- renderTable({
    acc_per_sp_us})
}
shinyApp(ui = ui, server = server)
