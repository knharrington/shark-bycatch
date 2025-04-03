################################################################################
# This script is the data processor for the CFEMM app
# It precomputes objects needed in the server and ui for use in the global.R script

################################################################################

# Load necessary libraries
library(tidyverse)
library(data.table)
library(janitor)
library(sf)
library(leaflet)
library(plotly)

# Most recent export
Data.In <- fread("data/Annotations-2025-03-19.csv")

# Filter for longline shark interactions
shark_data <- Data.In %>%
  filter(Trip_Type == "Longline" & Shark == "Y")

# Convert datetime columns
{
datetime_columns <- c("Retrieval_Begin_Date_Time", "Retrieval_End_Date_Time", "Catch_Begin_Date_Time")
shark_data[, (datetime_columns) := lapply(.SD, as.POSIXct), .SDcols = datetime_columns]
shark_data[, c("Retrieval_Begin_Date", "Retrieval_Begin_Time") := .(format(Retrieval_Begin_Date_Time, "%Y-%m-%d"), 
                                                                        format(Retrieval_Begin_Date_Time, "%H:%M:%S"))]
shark_data[, c("Retrieval_End_Date", "Retrieval_End_Time") := .(format(Retrieval_End_Date_Time, "%Y-%m-%d"), 
                                                                    format(Retrieval_End_Date_Time, "%H:%M:%S"))]
shark_data[, c("Catch_Date", "Catch_Time_Eastern") := .(format(Catch_Begin_Date_Time, "%Y-%m-%d"), 
                                                            format(Catch_Begin_Date_Time, "%H:%M:%S"))]
shark_data[, Catch_Time_GMT := format(Catch_Begin_Date_Time, "%H:%M:%S", tz = "GMT")]
}
# Summarize data using data.table
shark_summary <- setDT(shark_data)[, .(Row_Count = .N), by = Dataset_ID]

# Subset for top ten species
top_species <- setDT(shark_data)[, .N, by = .(Retrieval_Year, Common_Name)][order(-N)]
top_species_all_years_allsharks <- names(sort(tapply(top_species$N, top_species$Common_Name, sum), decreasing = TRUE))[1:15]
top_sharks <- shark_data[Common_Name %in% top_species_all_years_allsharks]

# Shark species catch table 
shark_species <- shark_data %>%
  group_by(Common_Name) %>%
  summarise(`Number Caught` = n()) %>%
  arrange(desc(`Number Caught`)) %>%
  mutate(`%` = round(`Number Caught` / sum(`Number Caught`) * 100, 2)) %>%
  adorn_totals("row") %>%
  mutate(`Number Caught` = format(`Number Caught`, big.mark = ","),
         `%` = ifelse(as.numeric(`%`) >= 99.8, "100.00", sprintf("%.2f", `%`)))
setnames(shark_species, old = "Common_Name", new = "Common Name")
setDT(shark_species)

# subset to the unique set-haul event level (for cpues and hotspots)
# species.sub.SHEs = unique(top.sub[,c("Unique_Retrieval","Species_CPU_Hook_Hours_BLL1000","Indiv_CPU_Hook_Hours_VL","Indiv_CPU_Hook_Hours_BLL1000","Shark",
#                                      "Species_CPU_km","Proportion_Retained","Proportion_Discarded","Species_CPU_Hook_Hours_VL","Species_CPU_Fishing_Time",
#                                      "Species_Ret_CPU_Fishing_Time","Catch_Grid_Name","Trip_Type","Retrieval_Year","Common_Name", "Retrieval_Season", "Depth",
#                                      "Catch_Longitude", "Catch_Latitude", "Centroid_Longitude", "Centroid_Latitude")])

# grid shape needed
{
gridshp <- st_read(dsn="shapefiles", layer = "GOM_GRID_10MIN_smooth")
gridshp=gridshp[1]
names(gridshp)[names(gridshp) == "FID_1"] <- "GRID_ID"
gridshp <- st_make_valid(gridshp)
gridshp <- gridshp[st_is_valid(gridshp), ]
}

# Port locations
{
lats <- c(27.332160)
lons <- c(-82.577740)
port <- c("Mote Marine Laboratory")
city <- c("Sarasota, FL")
moteport <- as.data.frame(list(Latitude=lats, Longitude=lons, Home_Port=port, City_State=city))
mote_icon <- makeIcon("www/MoteLogomark.svg",
                      iconWidth=30, iconHeight=30,
                      iconAnchorX=15, iconAnchorY=15)

homeport <- fread("data/cities.csv")
port_icon <- makeIcon("www/port-loc.svg",
                      iconWidth=30, iconHeight=30,
                      iconAnchorX=15, iconAnchorY=30)
}

# Summarize the data by year and calculate other summary statistics
summary_data <- shark_data %>%
  group_by(Retrieval_Year, Unique_Trip) %>%
  summarise(Avg_Sea_Days = mean(Sea_Days), .groups = "drop") %>%
  group_by(Retrieval_Year) %>%
  summarise(
    Trips = n_distinct(Unique_Trip),
    `Sea Days` = sum(Avg_Sea_Days),
    Hauls = n_distinct(shark_data$Unique_Retrieval[shark_data$Retrieval_Year == unique(Retrieval_Year)])
  ) %>%
  rename(Year = Retrieval_Year) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Data_Type",
    values_to = "Count"
  )

# Custom brand palette
sum_pal <- c("Trips" = "#f37163", "Sea Days" = "#0054a6", "Hauls" = "#00aae7")

# Plotly of trips, hauls, and sea days
trips_chart <- plot_ly(
  data = summary_data, x = ~Year, y = ~Count,
  color = ~Data_Type, colors = sum_pal,
  type = 'scatter', mode = 'lines+markers',
  text = ~paste(
    "<b>Metric:</b>", Data_Type,
    "<br><b>Year:</b>", Year,
    "<br><b>Total:</b>", Count),
  hoverinfo = 'text',
  line = list(width = 2),
  marker = list(size = 6, symbol = 'circle')
  ) %>%
  layout(
    xaxis = list(visible = T, showgrid = T, title = "", tickformat = "d"),
    yaxis = list(visible = T, showgrid = T, title = "Total Counts", tickformat = ",d"),
    legend = list(title = list(text = "Metric"), orientation = "h")
  ) %>%
  config(displaylogo = FALSE,
    toImageButtonOptions = list(
      format = "png",
      filename = "fishing_summary_chart",
      height = 600,
      width = 900,
      scale = 2
    )
  ) 

# Top species caught over time
bar_modified <- shark_data %>%
  group_by(Retrieval_Year, Common_Name) %>%
  summarise(Total_Sp_Yr = n(), .groups = "drop") %>%
  mutate(Common_Name = fct_lump_n(Common_Name, n = 5, w = Total_Sp_Yr, other_level = "Other")) %>%
  mutate(Common_Name = fct_relevel(Common_Name, "Other")) %>%
  group_by(Retrieval_Year, Common_Name) %>%
  summarise(Total_Sp_Yr = sum(Total_Sp_Yr), .groups = "drop")

num_colors <- nlevels(bar_modified$Common_Name)
ramp_pal <- colorRampPalette(c("#f37163", "#0054a6", "#00aae7"))(num_colors)
color_mapping <- setNames(custom_colors, levels(bar_modified$Common_Name))

species_chart <- plot_ly(
  data = bar_modified,
  x = ~Retrieval_Year,
  y = ~Total_Sp_Yr,
  color = ~Common_Name,
  colors = color_mapping,
  type = 'bar',
  text = ~paste(
    "<b>Species:</b>", Common_Name,
    "<br><b>Year:</b>", Retrieval_Year,
    "<br><b>Catch Events:</b>", Total_Sp_Yr),
  hoverinfo = 'text',
  textposition = 'none'
) %>%
  layout(
    barmode = "stack",
    xaxis = list(title = "", tickformat = "d"),
    yaxis = list(title = "Total Catch Events", tickformat = ",d"),
    legend = list(
      title = list(text = "Common Name"),
      orientation = "v",
      x = 1.02,
      y = 1
    )#,
    #margin = list(r = 130)  # extra room for long legend
  ) %>%
  config(
    displaylogo = FALSE,
    toImageButtonOptions = list(
      format = "png",
      filename = "top_species_bar_chart",
      height = 600,
      width = 900,
      scale = 2
    )
  )

# text for ui time range
mindate <- format(as.Date(min(shark_data$Retrieval_Begin_Date)), format = "%m/%Y")
maxdate <- format(as.Date(max(shark_data$Retrieval_Begin_Date)), format = "%m/%Y")
datetext <- paste0("Time Range (", mindate, " - ", maxdate, ")")

# Save all necessary objects to an .RData file
save(top_sharks, gridshp, shark_species, moteport, mote_icon, homeport, port_icon, trips_chart, species_chart, datetext, file = "data/preprocess.RData")
