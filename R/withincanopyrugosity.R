#' Convert LAD estimates into two rasters - standard deviation of LAD in a given column and canopy rugosity
#'
#' This function reads in a the LAD estimates that were previously calculated, 
#' finds the standard deviation of LAD estimates within a given column, then uses a 3x3 moving window
#' to find the standard deviation of the previously calculated vertical standard deviations in the moving window.
#' This value is then saved in a new matrix, so that the value calculated by the moving window is not used in
#' subsequent calcluations. This is the Hardiman approach to calcluating canopy rugosity to look at within
#' canopy variation of LAD.
#' 
#' Because the second half of this funtion uses a 3x3 moving window, the outside rows and columns are lost due to
#' insufficient data. If you are mosaicing multiple rasters together in later steps, this needs to be taken into 
#' account. I am working on a function that will pass a moving window over a large raster, but it is still in 
#' development.
#' 
#' These forest structure attributes are based off calculations from: 
#' 
#' Hardiman, B. S., Bohrer, G., Gough, C. M., Vogel, C. S., & Curtis, P. S. (2011). 
#' The role of canopy structural complexity in wood net primary production of a maturing northern deciduous forest. 
#' Ecology, 92, 1818-1827. https://doi.org/10.1890/10-2192.1
#' 
#' @param lad.array LAD estimate array that was generated using the machorn.lad function. 
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. This contains
#' spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
#' you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing max LAD and height of max LAD rasters.
#' @export

within.canopy.rugosity <- function(lad.array, laz.array, ht.cut, epsg.code) {
  
  #Lets create an empty matrix that corresponses with each final raster
  sd.lad.col.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  rugosity.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  
  #loop through the lad arrray and calculate the standard deviation of each vertical column
  #within the canopy
  for (r in 1:dim(lad.array$rLAD)[2]) {
    for (c in 1:dim(lad.array$rLAD)[3]) {
      
      canopy.column <- lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c]
      
      sd.lad.col <- round(sd(canopy.column, na.rm = TRUE), digits = 4)
      
      sd.lad.col.mat[r,c] <- sd.lad.col
    }
  }
  
  #lets use a moving window average to take the 3x3 sd of each cell

  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(rugosity.mat) - 1)
  col.ind <- 2:(ncol(rugosity.mat) - 1)
  
  #loop through the matrix using a 3x3 moving window approach
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- sd.lad.col.mat[(r+1), (c-1)],
        nn <- sd.lad.col.mat[(r+1), (c+0)],
        ne <- sd.lad.col.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- sd.lad.col.mat[(r+0), (c-1)],
        cc <- sd.lad.col.mat[(r+0), (c+0)],
        ce <- sd.lad.col.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- sd.lad.col.mat[(r-1), (c-1)],
        ss <- sd.lad.col.mat[(r-1), (c+0)],
        se <- sd.lad.col.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      canopy.rugosity <- sd(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      rugosity.mat[r,c] <- canopy.rugosity
    }
  }
  
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster for each level of the canopy using the original x,y data from the laz data
  sd.lad.raster <- raster::raster(sd.lad.col.mat,
                                  xmn = laz.array$x.bin[1],
                                  xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                  ymn = laz.array$y.bin[1],
                                  ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                  crs = crs.proj)
  
  rugosity.raster <- raster::raster(sd.lad.col.mat,
                                    xmn = laz.array$x.bin[1],
                                    xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                    ymn = laz.array$y.bin[1],
                                    ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                    crs = crs.proj)
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  sd.lad.raster.flip <- flip(sd.lad.raster, direction = "y")
  rugosity.raster.flip <- flip(rugosity.raster, direction = "y")
  
  #return the final rasters
  final.data <- list("vertical.sd.lad.raster" = sd.lad.raster.flip, 
                     "rugosity.raster" = rugosity.raster.flip)
  return(final.data)
}
