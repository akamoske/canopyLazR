#' Convert LAD estimates into two rasters - ratio of filled canopy and ratio of canopy porosity
#'
#' This function reads in a the LAD estimates that were previously calculated, 
#' finds the ratio of voxels in a given column that contain a LAD estimate and the ratio of 
#' voxels in a given column that are empty (i.e. no LAD estimates). The output is a list 
#' containing two rasters, one for each calcuation.
#' 
#' These forest structure attributes are based off calculations from: 
#' 
#' Hardiman, B., Bohrer, G., Gough, C., & Curtis, P. (2013). 
#' Canopy structural changes following widespread mortality of canopy dominant trees. 
#' Forests, 4, 537-552. https://doi.org/10.3390/f4030537
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

canopy.porosity.filled.ratio <- function(lad.array, laz.array, ht.cut, epsg.code) {
  
  #Lets create an empty matrix that corresponses with the input data
  porosity.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  filled.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  
  #Lets loop through the lad array
  for (r in 1:dim(lad.array$rLAD)[2]) {
    for (c in 1:dim(lad.array$rLAD)[3]) {
      
      #lets pull out a column based on the ht.cut value. we need to add 1 to account for the NA
      #layer used to initilize the matrix
      canopy.column <- lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c]
      
      #calculate the filled and empty voxels ratios
      filled <- sum(canopy.column > 0, na.rm = TRUE)
      porosity <- sum(canopy.column == 0, na.rm = TRUE)
      
      filled.ratio <- round(filled / (filled + porosity), digits = 4)
      porosity.ratio <- round(porosity / (filled + porosity), digits = 4)
      
      #assign these to the original matrix
      filled.mat[r,c] <- filled.ratio
      porosity.mat[r,c] <- porosity.ratio
    }
  }
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster using the original x,y data from the laz data
  filled.raster <- raster::raster(filled.mat,
                                  xmn = laz.array$x.bin[1],
                                  xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                  ymn = laz.array$y.bin[1],
                                  ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                  crs = crs.proj)
  
  empty.raster <- raster::raster(filled.mat,
                                 xmn = laz.array$x.bin[1],
                                 xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                 ymn = laz.array$y.bin[1],
                                 ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                 crs = crs.proj)
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  filled.raster.flip <- flip(filled.raster, direction = "y")
  
  empty.raster.flip <- flip(empty.raster, direction = "y")
  
  #return the final rasters
  final.data <- list("filled.raster" = filled.raster.flip, 
                     "empty.raster" = empty.raster.flip)
  return(final.data)
}
