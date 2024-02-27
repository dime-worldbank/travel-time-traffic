# Load sample NetCDF files
x1 <- system.file(package = "S5Processor", "extdata", "S5P_NRTI_L2__NO2_1.nc")
x2 <- system.file(package = "S5Processor", "extdata", "S5P_NRTI_L2__NO2_2.nc")

# Load sample shapefile including the borders of Vietnam
vnm_shp <- raster::shapefile(system.file(package = "S5Processor",
                                         "extdata", "vietnam_borders.shp"))

# Create vector from both NetCDF files.
my_files <- c(x1,x2)

# Most basic case: provide path to single NetCDF file.
# The user will be promted to choose from a list of product names.
S5P_1 <- S5P_process(input = my_files[1])

S5P_2 <- S5P_process(input = my_files, product = 39)
