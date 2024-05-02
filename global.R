################################################################################
# This script is the global environment for the CFEMM app
# It stores objects needed in the sever and ui

################################################################################

# load packages
{
  library(tidyverse)
  #library(plyr)
  library(data.table)
  library(janitor)
  #library(doBy)
  
  library(spatial)
  library(sp)
  library(sf)
  library(spdep)
  library(sfdep)
  library(fields)
  library(terra)
  
  #library(rasterVis)
  #library(raster)
  
  library(RColorBrewer)
  library(viridis)
  
  library(leaflet)
  #library(kableExtra)
  library(shiny)
  #library(plotly)
  library(htmlwidgets)
  library(shinythemes)
  library(shinycustomloader)
  library(leaflet.extras2)
  #library(leafem)
  
  #library(rjson)
  #library(KernSmooth)
  #library(ks)
}

#cleanMem <- function(n=10) { for (i in 1:n) gc() }

# Most recent export
Data.In <- data.table::fread("data/allsharks.csv")

#My.Vessel <- "No Bull"

#Subset for time
My.Date.Start <- as.Date("2015-06-01", format = "%Y-%m-%d") 
My.Date.End <- as.Date("2024-12-31", format = "%Y-%m-%d")

#My.Months <- c(1,2,3,4,5,6,7,8,9,10,11,12)  

#Subset for fishery
My.Trip.Type <- c("Longline")  

# # Species Lookup Table
# Species.Lookup <- read.csv("data/EM Species Codes Lookup.csv")
# # Modify species lookup
# Species.Lookup = plyr::rename(Species.Lookup, c("Number"="Species.Number"))
# colnames(Species.Lookup)=gsub(".","_",colnames(Species.Lookup), fixed=T)
# Species.Lookup$Species_Lookup = gsub("/"," or ",Species.Lookup$Species_Lookup)

#My.species = Species.Lookup[Species.Lookup$Shark == "Y",]$Species.Number

# Combine filter conditions
All.Data.Final <- setDT(Data.In[Data.In$Retrieval_Begin_Date > My.Date.Start &
                            Data.In$Retrieval_Begin_Date < My.Date.End &
                            Data.In$Trip_Type %in% My.Trip.Type, ])

# Convert datetime columns
datetime_columns <- c("Retrieval_Begin_Date_Time", "Retrieval_End_Date_Time", "Catch_Begin_Date_Time")
All.Data.Final[, (datetime_columns) := lapply(.SD, as.POSIXct), .SDcols = datetime_columns]
All.Data.Final[, c("Retrieval_Begin_Date", "Retrieval_Begin_Time") := .(format(Retrieval_Begin_Date_Time, "%Y-%m-%d"), 
                                                                        format(Retrieval_Begin_Date_Time, "%H:%M:%S"))]
All.Data.Final[, c("Retrieval_End_Date", "Retrieval_End_Time") := .(format(Retrieval_End_Date_Time, "%Y-%m-%d"), 
                                                                    format(Retrieval_End_Date_Time, "%H:%M:%S"))]
All.Data.Final[, c("Catch_Date", "Catch_Time_Eastern") := .(format(Catch_Begin_Date_Time, "%Y-%m-%d"), 
                                                            format(Catch_Begin_Date_Time, "%H:%M:%S"))]
All.Data.Final[, Catch_Time_GMT := format(Catch_Begin_Date_Time, "%H:%M:%S", tz = "GMT")]

# Summarize data using data.table
All.Data.Summary <- setDT(All.Data.Final)[, .(Row_Count = .N), by = Dataset_ID]

# Subset for top ten species
top_species <- setDT(All.Data.Final)[, .N, by = .(Retrieval_Year, Common_Name)][order(-N)]
top_species_all_years_allsharks <- names(sort(tapply(top_species$N, top_species$Common_Name, sum), decreasing = TRUE))[1:10]
top.sub <- All.Data.Final[Common_Name %in% top_species_all_years_allsharks]

# Create a copy of top.sub for further processing
#top.sub2 <- copy(top.sub)

# species caught table
#Species.Count <- summaryBy(Species_Number ~ Common_Name, data=unique(All.Data.Final[,c("Species_Number","Common_Name")]), FUN=c(length)) %>%
  #dplyr::summarise(Number_of_Unique_Species_Groups = sum(Species_Number.length))

All.Species <- All.Data.Final %>%
  dplyr::group_by(Common_Name) %>%
  dplyr::summarise(`Number Caught` = n()) %>%
  arrange(desc(`Number Caught`)) %>%
  mutate(`%` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
  adorn_totals("row") %>%
  mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
         `%` = ifelse(as.numeric(`%`) >= 99.8, "100.00", sprintf("%.2f", `%`)))
setnames(All.Species, old = "Common_Name", new = "Common Name")

# Convert to data.table if needed
if (!is.data.table(All.Species)) {
  setDT(All.Species)
}

# subset to the unique set-haul event level (for cpues and hotspots)
species.sub.SHEs = unique(top.sub[,c("Unique_Retrieval","Species_CPU_Hook_Hours_BLL1000","Indiv_CPU_Hook_Hours_VL","Indiv_CPU_Hook_Hours_BLL1000","Shark",
                                     "Species_CPU_km","Proportion_Retained","Proportion_Discarded","Species_CPU_Hook_Hours_VL","Species_CPU_Fishing_Time",
                                     "Species_Ret_CPU_Fishing_Time","Grid_Name","Trip_Type","Retrieval_Year","Common_Name", "Retrieval_Season", "Depth",
                                     "Catch_Longitude", "Catch_Latitude")])

# grid shape needed
gridshp <- st_read(dsn="shapefiles", layer = "GOM_GRID_10MIN_fullgulf")
gridshp=gridshp[1]
names(gridshp)[names(gridshp) == "Id"] <- "GRID_ID"

#gearrest <- st_read(dsn="shapefiles", layer="shapefile_reef_fish_longline_buoy_gear_po_GOMx_SERO")
#gearrest <- st_transform(gearrest, crs=4326)
#seasonalclose <- st_read(dsn="shapefiles", layer="Gulf_ReefLL_seasonal_po")
#seasonalclose <- st_transform(seasonalclose, crs=4326)

# home port
lats <- c(27.332160)
lons <- c(-82.577740)
port <- c("Mote Marine Laboratory")
city <- c("Sarasota, FL")
homeport <- as.data.frame(list(Latitude=lats, Longitude=lons, Home_Port=port, City_State=city))
port_icon <- makeIcon("www/MoteLogomark.svg",
                      iconWidth=30, iconHeight=30,
                      iconAnchorX=15, iconAnchorY=15)

# Vessel activity over time
# Calculate the number of unique retrievals per unique trip
unique_retrievals <- All.Data.Final %>%
  dplyr::group_by(Retrieval_Year) %>%
  dplyr::summarise(Number_Hauls = n_distinct(Unique_Retrieval)) 

# Summarize the data by year and calculate other summary statistics
summary_data <- All.Data.Final %>%
  dplyr::group_by(Retrieval_Year, Unique_Trip) %>%
  dplyr::summarise(Sea_Days = mean(Sea_Days)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Retrieval_Year) %>%
  dplyr::summarise(Number_Trips = n_distinct(Unique_Trip),
            Sea_Days_sum = sum(Sea_Days))

# Merge the summary data with the number of unique retrievals
summary_data <- left_join(summary_data, unique_retrievals, by = "Retrieval_Year")
  

summarylinechart <- ggplot(summary_data) +
  geom_point(aes(x = Retrieval_Year, y = Sea_Days_sum, color = "Sea Days"), size = 2) +
  geom_point(aes(x = Retrieval_Year, y = Number_Trips, color = "Trips"), size = 2) +
  geom_point(aes(x = Retrieval_Year, y = Number_Hauls, color = "Hauls"), size = 2) +
  geom_line(aes(x = Retrieval_Year, y = Sea_Days_sum, color = "Sea Days"), linewidth = 1.25) +
  geom_line(aes(x = Retrieval_Year, y = Number_Trips, color = "Trips"), linewidth = 1.25) +
  geom_line(aes(x = Retrieval_Year, y = Number_Hauls, color = "Hauls"), linewidth =1.25) +
  theme_minimal() +
  scale_x_continuous(n.breaks = 8) +
  scale_color_manual(name = "Legend", 
                     values = c("Sea Days" = "#0d1687", 
                                "Trips" = "#c5407e", 
                                "Hauls" = "#f0f920")) +
  labs(title = "Trips, Hauls, and Sea Days",
       x = " ",
       y = "Total") +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5), 
        axis.title.x = element_text(size = 18, vjust = 0.5),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 14))

# Top species caught over time
bar <- All.Data.Final %>%
  group_by(Retrieval_Year, Common_Name) %>%
  dplyr::summarise(Total_Sp_Yr = n()) %>%
  arrange(desc(Total_Sp_Yr))

top_species_all_years <- bar %>%
  group_by(Common_Name) %>%
  dplyr::summarise(Total_Sp = sum(Total_Sp_Yr)) %>%
  top_n(5, Total_Sp) %>%
  pull(Common_Name)

bar_modified <- bar %>%
  mutate(Common_Name = if_else(Common_Name %in% top_species_all_years, as.character(Common_Name), "Other")) %>%
  mutate(Common_Name = factor(Common_Name, levels = c("Other", top_species_all_years)))

topspeciesbar <- ggplot(bar_modified, aes(fill = Common_Name, y = Total_Sp_Yr, x = as.character(Retrieval_Year))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(name = "Common Name", 
                     breaks = c(top_species_all_years, "Other"), 
                     direction = -1, 
                     discrete = TRUE, 
                     option = "C") +
  labs(title = "Top Species Catch Events",
       x = " ",
       y = "Catch Events") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5), 
        axis.title.x = element_text(size = 18, vjust = 0.5),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 14))

