## Load package
library(googletraffic)

## Load additional packages to run below examples
library(ggplot2)
library(dplyr)
library(raster)

## Grab shapefile of Manhattan
roi_sf <- gadm(country = "KEN", level=1, path = tempdir()) %>% st_as_sf()
roi_sf <- roi_sf[roi_sf$NAME_1 == "Nairobi",]

grid_sf <- gt_make_grid(
  polygon = roi_sf,
  zoom = 16,
  height_width_max = 2000,
  height = NULL,
  width = NULL,
  reduce_hw = 10
)

a <- readRDS("~/Dropbox/nairobi_grid.Rds")

