####################
## Load libraries ##
####################

library(sf)        
library(ggplot2)   
library(dplyr)     
library(readxl)    
library(purrr)
library(PostcodesioR)
library(tidyr)
library(writexl)

####################
## Load Shapefile ##
####################

nuts3 <- st_read(".../.../Shapefiles/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")

st_crs(nuts3) # Check current CRS
nuts3 <- st_transform(nuts3, crs = 4326)  # Convert to WGS 84 (latitude/longitude)

nuts3 <- nuts3 %>% 
  filter(CNTR_CODE == "UK", LEVL_CODE == 3, NUTS_NAME != "Shetland Islands") # filter to UK and nuts level code 3 only

plot(st_geometry(nuts3), col = "#BFDAA4", border = "black") # visual check

###########################
## Load in postcode data ##
###########################

# By worksheet name
data <- read_excel("whosholdingthereins - datasets.xlsx", sheet = "Geographical coverage")

colnames(data)

## == NOTE THAT FULL POSTCODES HAVE BEEN REDACTED FOR GITHUB SO CODE BELOW MAY NEED EDITING == ##

## COUNT UNIQUE POSTCODES 

# Count the number of unique postcodes in the 'Client Full Postcode' column
unique_postcodes_count <- length(unique(data$postcode))

# Print the result
print(unique_postcodes_count)

###----- get nuts data from postcode -----###

# Function to simply lookup the postcode without extracting any specific data
get_postcode_data <- function(postcode) {
  return(postcode_lookup(postcode))
}

# Apply postcode_lookup() to each postcode in the dataframe
postcode_data <- map_df(data$postcode, get_postcode_data)

# Summarise postcode_data to ensure there's one unique row for each postcode, 
postcode_data_summary <- postcode_data %>%
  group_by(postcode) %>%
  summarise(nuts = first(nuts),n = n(),.groups = "drop") %>%
  mutate(totalreported = sum(n),percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup()

# Tidy table: format percent and CI
postcode_data_summary <- postcode_data_summary %>%
  mutate(percent = paste0(percent, "%"),CI = paste0(ci_lower, " - ", ci_upper))

postcode_data_summary <- postcode_data_summary %>%
  select(postcode, nuts, n, percent, CI)


# Create frequency count for each nuts region
nuts_frequency <- postcode_data_summary %>%
  group_by(nuts) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(totalreported = sum(n),percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup()

# Tidy table: format percent and CI
nuts_frequency <- nuts_frequency %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))

nuts_frequency <- nuts_frequency %>%
  select(nuts, n, percent, CI)

# Combine original data with lookup results, row-wise
data2 <- bind_cols(data, postcode_data)

# Select only the desired columns
data2 <- data2 %>%
  select(EIDSID,`veterinary practice`, `postcode...3`, country, nhs_ha, longitude,latitude, nuts)

## SUMMARY DATA FOR POSTCODE, COUNTRY AND REGION ##

## COUNTRY

country_summary <- data2 %>%
  group_by(country) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)
  ) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))

country_summary <- country_summary  %>%
  select(country, n, percent, CI)

## REGION

region_summary <- data2 %>%
  group_by(nhs_ha) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)
  ) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))

region_summary <- region_summary  %>%
  select(nhs_ha, n, percent, CI)


######################################################################
## Prep data to count and  match x y coords to shapefiles NUTS_NAME ##
######################################################################

# Count occurrences of each X-Y coordinate
data_counts <- data2 %>%
  group_by(`postcode...3`, longitude, latitude) %>%
  summarise(count = n(), .groups = "drop")  # Get frequency of each point

# Convert to an sf spatial object
data_sf <- st_as_sf(data_counts, coords = c("longitude", "latitude"), crs = 4326) #crs = coordinates reference system

#Assign Points to NUTS3 Regions Using Spatial Join
data_sf <- st_join(data_sf, nuts3, left = FALSE)  # Keep only matched points

# Create smaller df for mapping data
plotdata <- data_sf[c("postcode", "NUTS_NAME", "count", "geometry" )]

#sanity count check 
sum(plotdata$count)
# count unique counties
unique(plotdata$NUTS_NAME)

# Combine rows by NUTS_NAME and sum the 'count' column
plotdata <- plotdata %>%
  group_by(NUTS_NAME) %>%
  summarize(
    postcode = first(postcode),  # you can change this logic if you want to concatenate or select differently
    count = sum(count),  # sum counts
    geometry = st_union(geometry),  # combine geometries (adjust as needed)
    .groups = "drop")  # Ungroup the result

#sanity count check 
sum(plotdata$count)


## ADDITIONAL TABLE FOR CI DATA FOR RESULTS ##

# plotdata_summary <- plotdata %>%
#   group_by(NUTS_NAME) %>%
#   summarise(postcode = first(postcode), n = sum(count), geometry = st_union(geometry),.groups = "drop") %>%
#   mutate(totalreported = sum(n),
#          percent = round(n / totalreported * 100, 1)) %>%
#   rowwise() %>%
#   mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
#          ci_lower = round(ci[[1]], 1),
#          ci_upper = round(ci[[2]], 1)) %>%
#   select(-ci) %>%
#   arrange(desc(n)) %>%
#   ungroup() %>%
#   mutate(percent = paste0(percent, "%"),
#          CI = paste0(ci_lower, " - ", ci_upper))

# plotdata_summary <- plotdata_summary  %>%
#   select(NUTS_NAME, n, percent, CI)
# 
# write_xlsx(plotdata_summary, "XXXX.xlsx")


# Ensure the CRS of both datasets match
st_crs(nuts3) == st_crs(plotdata) # Should return TRUE

# Perform an attribute join using st_join()
nuts3a <- st_join(nuts3, plotdata %>% select(NUTS_NAME, count), left = TRUE)

# Replace NA values in 'count' with 0
nuts3a$count[is.na(nuts3a$count)] <- 0

#sanity count check 
sum(nuts3a$count)

#############################################
## create bins to categorised density plot ##
#############################################

# Define your bins and labels
breaks <- c(0, 1, 5, 10, 13, 19, Inf)  # Define the bin edges
labels <- c("1", "2-5", "6-10", "11-15", "19", "≥20")  # Labels for each bin

# Create a new column with the binned data
nuts3a$bins <- cut(nuts3a$count, breaks = breaks, labels = labels, right = TRUE)

View(nuts3a)

# Add "0" as a new level to the 'bins' factor
nuts3a$bins <- addNA(nuts3a$bins)               # Preserve existing NAs
levels(nuts3a$bins) <- c(levels(nuts3a$bins), "0")

# Replace NA values with "0"
nuts3a$bins[is.na(nuts3a$bins)] <- "0"

#################
## create plot ##  ## total respondents per county
#################

plot <- ggplot(nuts3a) +
  geom_sf(aes(fill = bins), color = "white", lwd = 0.1) +
  # Custom manual fill for categorical bins
  scale_fill_manual(
    values = c("0" = "#dcdcdc", "1" = "#cad6e7", "2-5" = "#96accf", "6-10" = "#6283b7", 
               "11-15" = "#2D5A9F", "19" = "#1c4380", "≥20" = "#19345B"),  # Map bins to colors
    na.value = "#dcdcdc",    # Color for missing values
    name = "submissions"
  ) +
  theme_void()

plot


####################
## Save workspace ##
####################

save.image(file = "XXXXX.RData")
