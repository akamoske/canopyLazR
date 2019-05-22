
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
# List all the files in the path
laz.files.list <- list.files("./", pattern=c("\\.laz$|.las$"), full.names = TRUE)

# Remove any files that are less than 500kb - because these do not contain data
laz.files.list <- laz.files.list[sapply(laz.files.list, file.size) > 5000]

#-----------------------------------------------------------------------------------
# For memory storage these is the best way to run the package
#-----------------------------------------------------------------------------------

For (i in laz.files.list) {
  # Convert .laz or .las file into a voxelized lidar array
  laz.data <- laz.to.array(laz.file.path = i, 
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
  
  # Generate a quick raster plot of the resulting total canopy LAI values for each pixel
  plot(lai.raster)
  
  # Convert the list of LAZ arrays into a ground and canopy height raster
  grd.can.rasters <- array.to.ground.and.canopy.rasters(laz.data, 32611)
  
  # Plot the ground raster
  plot(grd.can.raster$ground.raster)
  
  # Plot the canopy height raster
  plot(grd.can.raster$canopy.raster)
  
  # Plot the canopy height model raster
  (plot grd.can.raster$chm.raster)
  
  # All files will need to be saved before starting the next loop, write those to disc
}


```

## License

This project is licensed under the GNU GPUv2 License - see the [LICENSE.md](LICENSE.md) file for details

