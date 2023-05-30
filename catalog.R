#======================================================================
# Adapted from Linda Flade's fabulous R code from GeogX850. Thanks Linda!


# Preamble - Install and load packages, set working directory, load data into variables
#=======================================================================

install.packages("dplyr")
install.packages("raster")
install.packages("terra")
install.packages("RCSF")
install.packages("lidR")

#for color visualization:
install.packages("RColorBrewer")
install.packages("viridis") 

install.packages("caret")
install.packages("randomForest") 

install.packages("car")
install.packages("carData")

library(dplyr)
library(raster)
library(terra)
library(RColorBrewer)
library(viridis) 

#load again if R was closed since last session
library(rgdal)
library(sf)
library(gstat)
library(automap)
library (ggplot2)
library(maptools)
library(openxlsx)
library(lidR)
library(RCSF)
library(randomForest)
library(caret)
library(car)
library(carData)
#====================================================================

# set working directory
setwd("C:/Users/bakalarczyk/Documents/JNP/figs/")

# load XYZ,intensity, and classification only
ctg <- readLAS("C:/Users/bakalarczyk/Documents/JNP/lazrgb_refined_P02_JasperSSE/tiles/norm/norm/buffrem/tile_424000_5854000_norm.laz", select =  "xyzic") 
ctg = filter_poi(ctg, Z>=0, Z<=50)
las_check(ctg)

#====================================================================
# DEM
#====================================================================
dem <- rasterize_terrain(ctg, 2, tin(), pkg = "terra")
dem_prod <- terra::terrain(dem, v = c("slope", "aspect"), unit = "radians")
dem_hillshade <- terra::shade(slope = dem_prod$slope, aspect = dem_prod$aspect)
plot(dem_hillshade, col = gray(0:50/50), legend = FALSE)

writeRaster(dem_hillshade, "DEM_HS.tif", NAflag = NA, gdal =c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))
writeRaster(dem, "DEM.tif", NAflag = NA, gdal =c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))

#====================================================================
# CHM
#====================================================================================

ctg <- readLAScatalog("C:/Users/bakalarczyk/Documents/JNP/lazrgb_refined_P02_JasperSSE/tiles/norm/norm/buffrem/", select =  "xyzic") 
opt_filter(ctg) <- "-drop_z_above 50 -drop_z_below 0"
las_check(ctg)

# create canopy height model
chm <- rasterize_canopy(ctg, res = 1, algorithm = p2r(0.2, na.fill = tin()), pkg = "terra")
smoothed <- terra::focal(chm, w, fun = mean, na.rm = TRUE)
col <- height.colors(25)
plot(chm, col = col)

writeRaster(chm, "CHM.tif", NAflag = NA, gdal =c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))

#====================================================================================
# Analysis
#====================================================================================

plots <- st_read("C:/Users/bakalarczyk/Documents/JNP/shp/plot_points.shp")

png("ctg.png", width = 6, height = 6, units = "in", res = 300)
plot(ctg)
plot(plots, add = TRUE, col = "red")
# Close the PNG device
dev.off()

D <- plot_metrics(ctg, .stdmetrics_z, plots, radius = 11.3)

# Save the csv to use for feature selection in python... because I'm better at python
write.csv(D, file = "C:/Users/bakalarczyk/Documents/JNP/r_metrics.csv", row.names = FALSE)

#====================================================================================
# Canopy Fuel Load (CFL)
#====================================================================================

m <- lm(CFL_Phelps ~zq45 + zq50 + zq65, data = D)

# Get the summary of the model
summary_text <- capture.output(summary(m))

# Extract the equation, RMSE, MSE, and R-squared from the summary
equation <- summary(m)$call
rmse <- sqrt(summary(m)$sigma^2)
mse <- summary(m)$sigma^2
r_squared <- summary(m)$r.squared

# Print the equation, RMSE, MSE, and R-squared
cat("Equation: ", equation, "\n")
cat("RMSE: ", rmse, "\n")
cat("MSE: ", mse, "\n")
cat("R-squared: ", r_squared, "\n")

# Print the summary to a text file
cat(summary_text, file = "CFL_summary.txt", sep = "\n")

# Plot actual vs predicted values
png("CFL_comparison.png", width = 6, height = 6, units = "in", res = 300)

plot(D$CFL_Phelps, predicted_values, 
     xlab = bquote("Actual CFL " ~ (kg/m^2)),
     ylab = bquote("Predicted CFL " ~ (kg/m^2)),
     xlim = c(0, max(D$CFL_Phelps)),
     ylim = c(0, max(predicted_values)))

abline(a = 0, b = 1)

# Close the PNG device
dev.off()

# Estimate Canopy Fuel Load 
metrics_w2w <- pixel_metrics(ctg, .stdmetrics_z, res = 23, pkg = "terra")
CFL <- predict(metrics_w2w, m) # wall-to-wall raster of predicted CFL

# Save the plot as a PNG file
png("CFL_plot.png")
plot(CFL)
dev.off()

ras <- raster(CFL)
NAvalue(ras) = -9999								#set value for NAs
proj4string(ras)<- CRS(paste("+init=epsg:",32611,sep="")) 		#define coordinate system
writeRaster(ras, "CFL.tif", NAflag = NA, options = c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))

#====================================================================================
# Canopy Base Height (CBH)
#====================================================================================

m <- lm(avg_cbh_m ~zq80 + zq90 + zpcum9, data = D)

# Get the summary of the model
summary_text <- capture.output(summary(m))

# Extract the equation, RMSE, MSE, and R-squared from the summary
equation <- summary(m)$call
rmse <- sqrt(summary(m)$sigma^2)
mse <- summary(m)$sigma^2
r_squared <- summary(m)$r.squared

# Print the equation, RMSE, MSE, and R-squared
cat("Equation: ", equation, "\n")
cat("RMSE: ", rmse, "\n")
cat("MSE: ", mse, "\n")
cat("R-squared: ", r_squared, "\n")

# Print the summary to a text file
cat(summary_text, file = "CBH_summary.txt", sep = "\n")

# Calculate predicted values
predicted_values <- predict(m)

# Plot actual vs predicted values
png("CBH_comparison.png", width = 6, height = 6, units = "in", res = 300)

plot(D$avg_cbh_m, predicted_values, 
     xlab = "Actual CBH (m)",
     ylab = "Predicted CBH (m)",
     xlim = c(0, max(D$avg_cbh_m)),
     ylim = c(0, max(predicted_values)))

abline(a = 0, b = 1)

# Close the PNG device
dev.off()

# Estimate Canopy Base Height
#metrics_w2w <- pixel_metrics(ctg, .stdmetrics_z, res = 23, pkg = "terra")
CBH <- predict(metrics_w2w, m) # wall-to-wall raster of predicted CBH

# Save the plot as a PNG file
png("CBH.png")
plot(CBH)
dev.off()

ras <- raster(CBH)
NAvalue(ras) = -9999								#set value for NAs
proj4string(ras)<- CRS(paste("+init=epsg:",32611,sep="")) 		#define coordinate system
writeRaster(ras, "CBH.tif", NAflag = NA, options =c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))

#====================================================================================
# Canopy Bulk Density (CBD) -  CBD = CFL /  CH - CBH
#====================================================================================

f = ~list(mean = mean(Z))                    	 

opt_filter(ctg) <- "-keep_class 5" # high vegetation
CH = pixel_metrics(ctg, func1, res = 23) # Take mean z of high vegetation

CBD = CFL / (CH - CB)

# Save the plot as a PNG file
png("CBD_plot.png")
col <- height.colors(25)
plot(CBD, col = col)
dev.off()

ras <- raster(CBD)
NAvalue(ras) = -9999								#set value for NAs
proj4string(ras)<- CRS(paste("+init=epsg:",32611,sep="")) 		#define coordinate system
writeRaster(ras, "CBD.tif", NAflag = NA, options =c("COMPRESS=NONE", "TFW=YES", "overwrite=TRUE"))


















