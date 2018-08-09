
# LiDARforestR

R package to estimate leaf area density (LAD) and leaf area index (LAI) from airborne LiDAR point clouds

### Installation

The easiest way to install `LiDARforestR` is via `install_github` from the `devtools` package:

```
# If you haven't already installed this package and its dependencies
install.packages("devtools")

# If you alread have devtools installed or just installed it
library(devtools)

# Install LiDARforestR from GitHub
install_github("akamoske/LiDARforestR")
```

Once the package is installed in your R session, you should be able to load it like this:

```
library(LiDARforestR)
```

## Example of usage (after installation)

Once the pacakge is loaded in your R session, this is the an example of how to use the functions in this package
to estimate LAD and LAI:

#### Convert .laz or .las files into a list of voxelized lidar arrays
`laz.data <- laz.to.array("./Data/laz_files", 10, 1)`

#### Level each voxelized array in the list to mimic a canopy height model
`level.canopy <- canopy.height.levelr(laz.data)`

#### Estimate LAD for each voxel in leveled array in the list 
`lad.estimates <- MacHorn.LAD(level.canopy, 1, NULL)`

#### Convert the list of LAD arrays into a single raster stack
`lad.raster <- lad.array.to.raster.stack(lad.estimates, 32618)`

#### Create a single LAI raster from the LAD raster stack
`lai.raster <- raster::calc(lad.raster, fun = sum, na.rm = TRUE)`

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc

