#' Convert a list of voxelized arrays into a single raster stack
#'
#' This function reads in a list of voxelized arrays, converts each one to a raster stack, and then merges them all together
#' to form a raster stack of the entire study area. This function also creats a lot of extra NA layers in the raster stack so that
#' the individual rasters can easily be merged together.
#'
#' @param lad.array Voxelized LAD array that was created with the machorn.lad function
#' @param laz.array Original LiDAR pulse count array that was generated using the laz.to.array function. This contains
#' spatial information for the array.
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A raster stack of LAD estimates. Each raster is associated with a slice of the canopy. The Z resolution was determined
#' in the laz.to.array function
#' @export

lad.array.to.raster.stack <- function(lad.array, laz.array, epsg.code) {

  #lets pull out the first array in the list
  lad.testr <- lad.array$rLAD

  #create an empty list so that we can store the new matrix in it
  lad.mat <- list()

  #lets loop through all the rows of data in the array
  for (i in seq_along(1:dim(lad.testr)[1])) {

    #Lets create an empty matrix that corresponses with this row of data
    m.lad <- matrix(data = NA, nrow = dim(lad.testr)[2], ncol = dim(lad.testr)[3])

    #lets loop through all the slices of data in the array
    for (q in seq_along(1:dim(lad.testr)[3])) {

      #lets assign a new column in the matrix as the row of data in that slice
      #this will end up representing an indivdual column of of data at a given height in the canopy
      #we will eventually end up with a matrix for each vertical interval of the canopy
      m.lad[,q] <- lad.testr[i,,q]
    }

    #save the complete matrix to the list and go up to the next level in the canopy
    lad.mat[[i]] <- m.lad
  }

  #lets create a new list to store the rasters in
  raw.lad.rasters <- list()

  #lets loop through the list of matrixes that we just created
  for(f in seq_along(1:length(lad.mat))) {

    #this is the projection code for your site, change it as needed.
    crs.proj <- base::paste0("+init=epsg:", epsg.code)

    #now we can create a raster for each level of the canopy using the original x,y data from the laz data
    lad.raster <- raster::raster(lad.mat[[f]],
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

  #since all the raster stacks will have a different number of layers depending on pulse return locations, we need to normalize them.
  #the easiest way to do this was to assume that none of the tiles will have more than 100 layers in the raster stack. So we can just add
  #a certain number of layers to the stack so that they all will have 100 layers. This will allow us to merge them together.
  new.layers.num <- 250 - nlayers(lad.rasters)

  #to do this, we need to make a copy of one of the layers so that it has the same extent and resolution values
  new.raster <- lad.rasters[[1]]

  #lets assign all the cells in this raster as NA
  new.raster[] <- NA

  #now lets add the needed number of rasters to the top of the stack so that they will all eventually be equal
  for (k in seq_along(1:new.layers.num)) {
    lad.rasters <- addLayer(lad.rasters, new.raster)
  }

  #lets remove the NA layer from the raster stack that was used to initialize the raster - this way the first layer will be
  #0-1 meters, the second 1-2 meters, etc.
  lad.ras <- raster::dropLayer(lad.rasters, 1)

  #lets rename the layers now to make sense - first layer is 0-1 meters, etc.
  names(lad.ras) <- paste0("m.", rep(1:nlayers(lad.ras)))

  #return the final raster
  return(lad.ras)
}
