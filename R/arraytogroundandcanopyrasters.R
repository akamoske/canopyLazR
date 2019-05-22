#' Convert a list of voxelized arrays into two rasters - ground and canopy height
#'
#' This function reads in a list of voxelized arrays, converts each one to ground and canopy height rasters, and then merges them all together
#' to form a ground and canopy height raster of the entire study area.
#'
#' @param laz.array Original LiDAR pulse count array that was generated using the laz.to.array function. This contains
#' spatial information for the array.
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list of a ground and canopy height raster.
#' @export

array.to.ground.and.canopy.rasters <- function(laz.array, epsg.code) {

  #lets create an empty list so that we can store the rasters in
  ground.list <- list()
  canopy.list <- list()

  #lets pull out the first array
  laz.testr <- laz.array$array

  #create an empty list so that we can store the new matrix in it
  rast.mat <- list()

  #lets loop through all the rows of data in the array
  for (i in seq_along(1:dim(laz.testr)[1])) {

    #Lets create an empty matrix that corresponses with this row of data
    m.lad <- matrix(data = NA, nrow = dim(laz.testr)[2], ncol = dim(laz.testr)[3])

    #lets loop through all the slices of data in the array
    for (q in seq_along(1:dim(laz.testr)[3])) {

      #lets assign a new column in the matrix as the row of data in that slice
      #this will end up representing an indivdual column of of data at a given height in the canopy
      #we will eventually end up with a matrix for each vertical interval of the canopy
      m.lad[,q] <- laz.testr[i,,q]
    }

    #save the complete matrix to the list and go up to the next level in the canopy
    rast.mat[[i]] <- m.lad
  }

  #lets create a new list to store the rasters in
  raw.lad.rasters <- list()

  #lets loop through the list of matrixes that we just created
  for(f in seq_along(1:length(rast.mat))) {

    #this is the projection code for your site, change it as needed.
    crs.proj <- base::paste0("+init=epsg:", epsg.code)

    #now we can create a raster for each level of the canopy using the original x,y data from the laz data
    lad.raster <- raster::raster(rast.mat[[f]],
                                 xmn = laz.array$x.bin[1],
                                 xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                 ymn = laz.array$y.bin[1],
                                 ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                 crs = crs.proj)

    #we have to flip these rasters so that they are orientated the correct direction
    #this is standard when converting from an array to a raster
    lad.raster.flip <- flip(lad.raster, direction = "y")

    #lets save this completed raster to our list
    raw.lad.rasters[[f]] <- lad.raster.flip

  }

  #now that we have all the rasters for one tile, lets stack those all together
  lad.rasters <- do.call(raster::stack, raw.lad.rasters)

  ground.raster <- lad.rasters$layer.1
  canopy.raster <- lad.rasters$layer.2

  #lets make a chm
  chm.raster <- canopy.raster - ground.raster

  #return the final rasters
  final.data <- list("ground.raster" = ground.raster, "canopy.raster" = canopy.raster, "chm.raster" = chm.raster)
  return(final.data)
}
