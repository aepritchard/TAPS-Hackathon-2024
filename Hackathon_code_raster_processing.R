library(raster)
library(sf)

# Import plot boundaries
setwd("~/")
plot_shapes = st_read("2024_Colby_TAPS_Harvest_Area.shp")
plot_shapes = plot_shapes[,c("Block_ID", "TRT_ID",   "Plot_ID",  "geometry")]
colnames(plot_shapes)[2] = "Team"
plot_shapes <- st_transform(plot_shapes, crs = '+proj=longlat +datum=WGS84')

# Define function to average raster data over plots
average_raster_plots = function(raster, plots, date, image_type){
  avg_values <- extract(raster, plots, fun = mean, na.rm = TRUE)
  plots = plots %>%
    mutate(raster_value := avg_values)
  plots = plots %>%
    mutate(date := date)
  plots = plots %>%
    mutate(image_type := image_type)
  return(plots)
}

# Import ceres data and combine with plots
setwd("~/CERES_data")
dates = c("2024-06-17", "2024-06-17",
          "2024-07-09", "2024-07-09", 
          "2024-07-25", "2024-07-25", 
          "2024-07-29", 
          "2024-08-15", "2024-08-15",
          "2024-08-30", "2024-08-30")
image_type = c("NDVI", "MCARI2",
               "NDVI", "MCARI2",
               "NDVI", "MCARI2",
               "MCARI2",
               "NDVI", "MCARI2",
               "NDVI", "MCARI2")
for(i in seq(1:length(dates))){
  file_name = paste(dates[i], " 188633 taps corn ", image_type[i], ".tif", sep="")
  print(file_name)
  raster_file = raster(file_name)
  temp =  average_raster_plots(raster_file, plot_shapes, dates[i], paste("CERES", image_type[i], sep="_"))
  if(i == 1){
    combined = temp
  } else {
    combined = rbind(combined, temp)
  }
}

# Drone images
dates = c("2024-05-16","2024-05-16",
          "2024-06-11","2024-06-11",
          "2024-07-10","2024-07-10",
          "2024-07-23","2024-07-23",
          "2024-08-01","2024-08-01",
          "2024-08-22","2024-08-22",
          "2024-09-23","2024-09-23")
image_type = c("NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb",
               "NDVI_rgb", "Orthomosaic_rgb")
setwd("~/Drone Flights")
for(i in seq(1:length(dates))){
  file_name = paste(image_type[i], "-",dates[i],".tif", sep="")
  print(file_name)
  raster_file = raster(file_name)
  temp =  average_raster_plots(raster_file, plot_shapes, dates[i], paste("Drone", image_type[i], sep="_"))
  if(i == 1){
    combined_drones = temp
  } else {
    combined_drones = rbind(combined_drones, temp)
  }
}
