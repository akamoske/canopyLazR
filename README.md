
# canopyLazR

R package to estimate leaf area density (LAD) and leaf area index (LAI) from airborne LiDAR point clouds.

## Information

For theory behind the package please see the citation below. Please cite with use.   

*Kamoske A.G., Dahlin K.M., Stark S.C., and Serbin S.P. 2019. Leaf area density from airborne LiDAR: Comparing sensors and resolutions in a forest ecosystem. Forest Ecology and Management 433, 364-375.*

### Corresponding Author

Aaron G. Kamoske, PhD Candidate
   
  + [Michigan State University, Department of Geography, Environment, and Spatial Sciences](http://geo.msu.edu/)      
  + [ERSAM Lab](https://www.ersamlab.com/)   
  + akamoske@gmail.com

### Contributing Authors

Dr. Scott C. Stark
   
  + [Michigan State University, Department of Forestry](https://www.canr.msu.edu/for/)      
  + [Tropical Forestry Ecology Lab](https://sites.google.com/site/scottcstarktropicalforest/)   
  + scott.c.stark@gmail.com  
  
Dr. Shawn P. Serbin

  + [Brookhaven National Laboratory, Environmental and Climate Sciences Department](https://www.bnl.gov/envsci/)
  + [Terrestrial Ecosystem Science and Technology (TEST) group](https://www.bnl.gov/testgroup)
  + sserbin@bnl.gov
  
Dr. Kyla M. Dahlin
  + [Michigan State University, Department of Geography, Environment, and Spatial Sciences](http://geo.msu.edu/)
  + [Michigan State University, Ecology, Evolutionary Biology, and Behavior Program](https://eebb.msu.edu/)
  + [ERSAM Lab](https://www.ersamlab.com/)
  + kdahlin@msu.edu
  
## Installation

The easiest way to install `canopyLazR` is via `install_github` from the `devtools` package:

```
# If you haven't already installed this package and its dependencies
install.packages("devtools")

# If you alread have devtools installed or just installed it
library(devtools)

# Install canopyLazR from GitHub
install_github("akamoske/canopyLazR")

# Load the library
library(canopyLazR)
```

Now all functions should be available.

## Downloading example data

[NEON](https://www.neonscience.org/) provides a teaching LiDAR dataset that is easy to download via R. We can use this file as a test dataset here. Code to download this .las file follows:

```
# Install missing R package if needed
list.of.packages <- c("uuid","rlas","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos = "http://cran.rstudio.com/", dependencies = TRUE)
}

# Create a scratch folder to contain example LiDAR dataset
scratch_folder <- file.path("~/scratch/neon_data/")
if (! file.exists(scratch_folder)) dir.create(scratch_folder,recursive=TRUE)
setwd(file.path(scratch_folder))
getwd()

# Download NEON example .las file
download.file(url = "https://ndownloader.figshare.com/files/7024955",
              destfile = file.path(scratch_folder,"neon_lidar_example.las"),
              method = "auto",
              mode = "wb")

```

## Example of usage (after installation)

Once the package is loaded into your R session, this is the an example of how to use the functions in this package
to estimate LAD and LAI:

```
# Convert .laz or .las file into a voxelized lidar array
laz.data <- laz.to.array(laz.file.path = file.path(scratch_folder,"neon_lidar_example.las"), 
                         voxel.resolution = 10, 
                         z.resolution = 1,
                         use.classified.returns = TRUE)

# Level the voxelized array to mimic a canopy height model
level.canopy <- canopy.height.levelr(lidar.array = laz.data)

# Estimate LAD for each voxel in leveled array
lad.estimates <- machorn.lad(leveld.lidar.array = level.canopy, 
                           voxel.height = 1, 
                           beer.lambert.constant = NULL)

# Convert the LAD array into a single raster stack
lad.raster <- lad.array.to.raster.stack(lad.array = lad.estimates, 
                                      laz.array = laz.data, 
                                      epsg.code = 32611)

# Create a single LAI raster from the LAD raster stack
lai.raster <- raster::calc(lad.raster, fun = sum, na.rm = TRUE)

# Convert the list of LAZ arrays into a ground and canopy height raster
grd.can.rasters <- array.to.ground.and.canopy.rasters(laz.data, 32611)

# Calculate max LAD and height of max LAD
max.lad <- lad.ht.max(lad.array = lad.estimates, 
                      laz.array = laz.data, 
                      ht.cut = 5, 
                      epsg.code = 32618)

# Calculate the ratio of filled and empty voxels in a given column of the canopy
empty.filled.ratio <- canopy.porosity.filled.ratio(lad.array = lad.estimates,
                                                   laz.array = laz.data,
                                                   ht.cut = 5,
                                                   epsg.code = 32618)

# Calculate the volume of filled and empty voxles in a given column of the canopy
empty.filled.volume <- canopy.porosity.filled.volume(lad.array = lad.estimates,
                                                     laz.array = laz.data,
                                                     ht.cut = 5,
                                                     xy.res = 10,
                                                     z.res = 1,
                                                     epsg.code = 32618)

# Calculate the within canopy rugosity
within.can.rugosity <- rugosity.within.canopy(lad.array = lad.estimates,
                                              laz.array = laz.data,
                                              ht.cut = 5,
                                              epsg.code = 32618)

# Calculate the heights of various LAD quantiles
ht.quantiles <- lad.quantiles(lad.array = lad.estimates,
                              laz.array = laz.data,
                              ht.cut = 5,
                              epsg.code = 32618)

# Calculate various canopy volume metrics from Lefsky
can.volume <- canopy.volume(lad.array = lad.estimates,
                            laz.array = laz.data,
                            ht.cut = 5,
                            xy.res = 10,
                            z.res = 1,
                            epsg.code = 32618)

# We can calculate the depth of the euphotic zone by dividing by the volume of the voxel
euphotic.depth <- can.volume$euphotic.volume.column.raster / ( 10 * 10 * 1)

# Calculate the top of canopy rugosity volume
toc.rugos <- toc.rugosity(chm.raster = grd.can.rasters$chm.raster,
                          xy.res = 10,
                          z.res = 1)

# Plot the lai raster
plot(lai.raster)

# Plot the ground raster
plot(grd.can.rasters$ground.raster)

# Plot the canopy height raster
plot(grd.can.rasters$canopy.raster)

# Plot the canopy height model raster
plot(grd.can.rasters$chm.raster)

# Plot the max LAD raster
plot(max.lad$max.lad.raster)

# Plot the height of max LAD raster
plot(max.lad$max.lad.ht.raster)

# Plot filled voxel ratio raster
plot(empty.filled.ratio$filled.raster)

# Plot porosity voxel ratio raster
plot(empty.filled.ratio$porosity.raster)

# Plot filled voxel volume raster
plot(empty.filled.volume$filled.raster)

# Plot porosity voxel volume raster
plot(empty.filled.volume$porosity.raster)

# Plot the standard deviation of LAD within a vertical column raster
plot(within.can.rugosity$vertical.sd.lad.raster)

# Plot within canopy rugosity
plot(within.can.rugosity$rugosity.raster)

# Plot the height of the 10th quantile
plot(ht.quantiles$quantile.10.raster)

# Plot the height of the 25th quantile
plot(ht.quantiles$quantile.25.raster)

# Plot the height of the 50th quantile
plot(ht.quantiles$quantile.50.raster)

# Plot the height of the 75th quantile
plot(ht.quantiles$quantile.75.raster)

# Plot the height of the 90th quantile
plot(ht.quantiles$quantile.90.raster)

# Plot the height of the mean LAD
plot(ht.quantiles$mean.raster)

# Plot the volume of the euphotic zone for each column
plot(can.volume$euphotic.volume.column.raster)

# Plot the total leaf area in the euphotic zone for each column
plot(can.volume$euphotic.tla.column.raster)

# Plot the depth of the euphotic zone
plot(euphotic.depth)

# Plot the volume of the oligophotic zone for each column
plot(can.volume$oligophotic.volume.column.raster)

# Plot the total leaf area in the oligophotic zone for each column
plot(can.volume$oligophotic.tla.column.raster)

# Plot the volume of the empty space within a given colume
plot(can.volume$empty.volume.column.raster)

# Plot the volume of the empty space within a 3x3 moving window
plot(can.volume$empty.canopy.volume.raster)

# Plot the volume of the euphotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.euphotic.raster)

# Plot the volume of the oligophotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.oligophotic.raster)

# Plot the total leaf area of the euphotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.euphotic.tla.raster)

# Plot the total leaf area of the oligophotic zone within a 3x3 moving window
plot(can.volume$filled.canopy.oligophotic.tla.raster)

# Plot the top of canopy rugosity volume
plot(toc.rugos)

```

## License

This project is licensed under the GNU GPUv2 License - see the [LICENSE.md](LICENSE.md) file for details

