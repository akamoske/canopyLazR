#' Convert LAD estimates into two rasters - height of maximum LAD and maximum LAD within a column
#'
#' This function reads in a the LAD estimates that were previously calculated, 
#' finds the maximum LAD value within each column of voxels, and then finds the height where that value
#' occurs. The output is a list containing two rasters, one for each calculation.
#' 
#' These forest structure attributes are based off calculations from: 
#' 
#' Hardiman, B., Bohrer, G., Gough, C., & Curtis, P. (2013). 
#' Canopy structural changes following widespread mortality of canopy dominant trees. 
#' Forests, 4, 537-552. https://doi.org/10.3390/f4030537
#'
#' @param lad.array LAD estimate array that was generated using the machorn.lad function. 
#' @param laz.arraay Voxelized LiDAR array that was generated using the laz.to.array function. This contains
#' spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
#' you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing max LAD and height of max LAD rasters.
#' @export

max.lad.ht <- function(lad.array, laz.array, ht.cut, epsg.code) {
  
  #Lets create an empty matrix that corresponses with the input data
  max.lad.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  max.lad.ht.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  
  #lets loop through the array and do some calculations. there is probably a faster way to do this 
  #but it works really well as is.
  for (r in 1:dim(lad.array$rLAD)[2]) {
    for (c in 1:dim(lad.array$rLAD)[3]) {
      
      #lets put in a check to see if this is a NA column or not
      if (all(is.na(lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c]))) {
        max.lad.mat[r,c] <- NA
        max.lad.ht.mat[r,c] <- NA
      } else {
        #we have to add 1 to the ht.cut so that it accounts for the NA value used to initilize the 
        #matrix - we then subtract 1 at the end to account all voxel shifting 1 meter down due to the NA value
        max.lad <- max(lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c], na.rm = TRUE)
        max.lad.ht <- which(lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c] == max.lad) + (ht.cut - 1)
        
        if (max.lad > 0) {
          max.lad.mat[r,c] <- max.lad
          max.lad.ht.mat[r,c] <- max(max.lad.ht)
        } else {
          max.lad.mat[r,c] <- max.lad
          max.lad.ht.mat[r,c] <- min(max.lad.ht)
        }
      }
    }
  }
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster for each level of the canopy using the original x,y data from the laz data
  max.lad.raster <- raster::raster(max.lad.mat,
                                   xmn = laz.array$x.bin[1],
                                   xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                   ymn = laz.array$y.bin[1],
                                   ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                   crs = crs.proj)
  
  max.lad.ht.raster <- raster::raster(max.lad.ht.mat,
                                      xmn = laz.array$x.bin[1],
                                      xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                      ymn = laz.array$y.bin[1],
                                      ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                      crs = crs.proj)
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  max.lad.raster.flip <- flip(max.lad.raster, direction = "y")
  max.lad.ht.raster.flip <- flip(max.lad.ht.raster, direction = "y")
  
  #return the final rasters
  final.data <- list("max.lad.raster" = max.lad.raster.flip, 
                     "max.lad.ht.raster" = max.lad.ht.raster.flip)
  return(final.data)
}
