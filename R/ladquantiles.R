#' Convert LAD estimates into six rasters explaining the height distribution of LAD within the canopy
#'  - the heigth of the 10th, 25th, 50th, 75th, 90th quantiles aas well as the mean
#'
#' This function reads in a the LAD estimates that were previously calculated, 
#' calculates the height where a host of different quantiles and the mean occcur and return a raster
#' showing the height where each quantile occurs within a given vertical column. 
#' 
#' These forest structure attributes are based off calculations from: 
#' 
#' Shi, Y., Wang, T., Skidmore, A.K., and Heurich, M. (2018). Important LiDAR metrics for discriminating 
#' forest tree species in Central Europe. ISPRS Journal of Photogrammetry and Remote Sensing, 137, 
#' 163-174. https://doi.org/10.1016/j.isprsjprs.2018.02.002
#' 
#' @param lad.array LAD estimate array that was generated using the machorn.lad function. 
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. This contains
#' spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
#' you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing the quantile and mean rasters.
#' @export

lad.quantiles <- function(lad.array, laz.array, ht.cut, epsg.code) {
  
  #create a bunch of empty matrixes to store the data in
  foliage.10.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  foliage.25.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  foliage.50.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  foliage.75.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  foliage.90.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  foliage.mean.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  
  #loop through the lad array
  for (r in 1:dim(lad.array$rLAD)[2]) {
    for (c in 1:dim(lad.array$rLAD)[3]) {
      
      #pull out an individual column
      canopy.column <- lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c]
      
      #check to see if there is data in the column
      if (all(is.na(canopy.column)) == TRUE) {
        
        #if there is not then give it an NA value
        foliage.10.mat[r,c] <- NA
        foliage.25.mat[r,c] <- NA
        foliage.50.mat[r,c] <- NA
        foliage.75.mat[r,c] <- NA
        foliage.90.mat[r,c] <- NA
        foliage.mean.mat[r,c] <- NA
        
      } else { 
        
        #find the sum of the values in the column, not including NA values (LAI)
        foliage.sum <- round(sum(canopy.column, na.rm = TRUE), digits = 4)
        
        #remove the NA values from the column
        canopy.column.no.na <- canopy.column[!is.na(canopy.column)]
        
        #make an empty list to store the data in
        foliage.list <- list()
        
        #loop through the column we pulled out
        for(i in 1:length(canopy.column.no.na)) {
          
          #we want to find the cumulative lad at a given point in the canopy so this does that
          if (i == 1) {
            foliage.list[[i]] <- canopy.column.no.na[i]
          } else {
            foliage.list[[i]] <- sum(canopy.column.no.na[1:i])
          }
        }
        
        #calculate these quantiles
        foliage.quantile <- quantile(unlist(foliage.list), probs = c(0.1, 0.25, 0.5, 0.75, 0.90))
        
        #calculate the mean
        foliage.mean <- mean(unlist(foliage.list), na.rm = TRUE)
        
        #find the height at which these values occur
        foliage.10.ht <- which(abs(unlist(foliage.list) - foliage.quantile[[1]]) == 
                                 min(abs(unlist(foliage.list) - foliage.quantile[[1]])))
        
        foliage.25.ht <- which(abs(unlist(foliage.list) - foliage.quantile[[2]]) == 
                                 min(abs(unlist(foliage.list) - foliage.quantile[[2]])))
        
        foliage.50.ht <- which(abs(unlist(foliage.list) - foliage.quantile[[3]]) == 
                                 min(abs(unlist(foliage.list) - foliage.quantile[[3]]))) 
        
        foliage.75.ht <- which(abs(unlist(foliage.list) - foliage.quantile[[4]]) == 
                                 min(abs(unlist(foliage.list) - foliage.quantile[[4]])))
        
        foliage.90.ht <- which(abs(unlist(foliage.list) - foliage.quantile[[5]]) == 
                                 min(abs(unlist(foliage.list) - foliage.quantile[[5]])))
        
        foliage.mean.ht <- which(abs(unlist(foliage.list) - foliage.mean) == 
                                   min(abs(unlist(foliage.list) - foliage.mean)))
        
        #save these values to the matrix - if the value occurs between two points then take the average
        if (length(foliage.10.ht) == 1) {
          foliage.10.mat[r,c] <- foliage.10.ht
        } else {
          foliage.10.mat[r,c] <- sum(foliage.10.ht) / length(foliage.10.ht)
        }
        
        if (length(foliage.25.ht) == 1) {
          foliage.25.mat[r,c] <- foliage.25.ht
        } else {
          foliage.25.mat[r,c] <- sum(foliage.25.ht) / length(foliage.25.ht)
        }
        
        if (length(foliage.50.ht) == 1) {
          foliage.50.mat[r,c] <- foliage.50.ht
        } else {
          foliage.50.mat[r,c] <- sum(foliage.50.ht) / length(foliage.50.ht)
        }
        
        if (length(foliage.75.ht) == 1) {
          foliage.75.mat[r,c] <- foliage.75.ht
        } else {
          foliage.75.mat[r,c] <- sum(foliage.75.ht) / length(foliage.75.ht)
        }
        
        if (length(foliage.90.ht) == 1) {
          foliage.90.mat[r,c] <- foliage.90.ht
        } else {
          foliage.90.mat[r,c] <- sum(foliage.90.ht) / length(foliage.90.ht)
        }
        
        if (length(foliage.mean.ht) == 1) {
          foliage.mean.mat[r,c] <- foliage.mean.ht
        } else {
          foliage.mean.mat[r,c] <- sum(foliage.mean.ht) / length(foliage.mean.ht)
        }
      }
    }
  }
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster for each level of the canopy using the original x,y data from the laz data
  quantile.10.raster <- raster::raster(foliage.10.mat,
                                       xmn = laz.array$x.bin[1],
                                       xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                       ymn = laz.array$y.bin[1],
                                       ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                       crs = crs.proj)
  
  quantile.25.raster <- raster::raster(foliage.25.mat,
                                       xmn = laz.array$x.bin[1],
                                       xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                       ymn = laz.array$y.bin[1],
                                       ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                       crs = crs.proj)
  
  quantile.50.raster <- raster::raster(foliage.50.mat,
                                       xmn = laz.array$x.bin[1],
                                       xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                       ymn = laz.array$y.bin[1],
                                       ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                       crs = crs.proj)
  
  quantile.75.raster <- raster::raster(foliage.75.mat,
                                       xmn = laz.array$x.bin[1],
                                       xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                       ymn = laz.array$y.bin[1],
                                       ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                       crs = crs.proj)
  
  quantile.90.raster <- raster::raster(foliage.90.mat,
                                       xmn = laz.array$x.bin[1],
                                       xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                       ymn = laz.array$y.bin[1],
                                       ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                       crs = crs.proj)
  
  mean.raster <- raster::raster(foliage.mean.mat,
                                xmn = laz.array$x.bin[1],
                                xmx = laz.array$x.bin[length(laz.array$x.bin)],
                                ymn = laz.array$y.bin[1],
                                ymx = laz.array$y.bin[length(laz.array$y.bin)],
                                crs = crs.proj)
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  quantile.10.raster.flip <- flip(quantile.10.raster, direction = "y")
  quantile.25.raster.flip <- flip(quantile.25.raster, direction = "y")
  quantile.50.raster.flip <- flip(quantile.50.raster, direction = "y")
  quantile.75.raster.flip <- flip(quantile.75.raster, direction = "y")
  quantile.90.raster.flip <- flip(quantile.90.raster, direction = "y")
  mean.raster.flip <- flip(mean.raster, direction = "y")
  
  #return the final rasters
  final.data <- list("quantile.10.raster" = quantile.10.raster.flip, 
                     "quantile.25.raster" = quantile.25.raster.flip,
                     "quantile.50.raster" = quantile.50.raster.flip,
                     "quantile.75.raster" = quantile.75.raster.flip,
                     "quantile.90.raster" = quantile.90.raster.flip,
                     "mean.raster" = mean.raster.flip)
  return(final.data)
}
