#' Convert LAD estimates into fives rasters explaining the volume and total leaf area of the euphotic (portion of the
#' canopy where 65% of the leaf material is located) and the oligophotic zone (the remaining canopy) and the volume
#' of empty space within the canopy (from top of the canopy the ground)
#'
#' This function reads in a the LAD estimates that were previously calculated, 
#' calculates the height cutoff for the euphotic zone, calculates the volume and total leaf area of this 
#' portion of the canopy and then does the same for the remaining canopy. It then returns 5 rasters
#' 
#' This function also uses a 3x3 moving window to calculate the volume of the euphotic, oligophotic, and empty
#' zones within the moving window. This returns 5 additional rasters, one for each variable.
#' 
#' These forest structure attributes are based off calculations from: 
#' 
#' Lefsky, M.A., Cohen, W.B., Acker, S.A., Parker, G.G., Spies, T.A., and Harding, D. (1999).
#' Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of Douglas-Fir Western Hemlock Forests.
#' Remote Sensing of the Environment, 70, 339-361. https://doi.org/10.1016/S0034-4257(99)00052-8
#' 
#' @param lad.array LAD estimate array that was generated using the machorn.lad function. 
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. This contains
#' spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
#' you want to include all calculations
#' @param xy.res Horizontal resolution of each voxel - if it is 10x10 meters then just enter 10
#' @param z.res Vertical resolution of each voxel - if it is 1 meter then just enter 1
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing the quantile and mean rasters.
#' @export

canopy.volume <- function(lad.array, laz.array, ht.cut, xy.res, z.res, epsg.code) {
  
  #empty matrices for individual columns
  euphotic.volume.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  euphotic.tla.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  oligophotic.volume.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  oligophotic.tla.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  empty.volume.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  
  #loop throug the lad estimates
  for (r in 1:dim(lad.array$rLAD)[2]) {
    for (c in 1:dim(lad.array$rLAD)[3]) {
      
      #pull out a slice of the column
      canopy.column <- lad.array$rLAD[(ht.cut + 1):dim(lad.array$rLAD)[1],r,c]
      
      #check to see if there is actually data in the column
      if (all(is.na(canopy.column)) == TRUE) {
        
        euphotic.volume.mat[r,c] <- NA
        euphotic.tla.mat[r,c] <- NA
        oligophotic.volume.mat[r,c] <- NA
        oligophotic.tla.mat[r,c] <- NA
        
      } else {
        
        #find the lai
        foliage.sum <- round(sum(canopy.column, na.rm = TRUE), digits = 4)
        
        #calculate what 65% of the total leaf area is in the canopy
        foliage.65 <- round(foliage.sum * 0.65, digits = 4)
        
        #remove the na values
        canopy.column.no.na <- canopy.column[!is.na(canopy.column)]
        
        #this is put here incase the cutoff value cuts off the foliage below the cutoff but there is still
        #an empty voxel at the top - this means that there are actually LAD returns in this column and it is
        #not NA - thus it still has an empty volume
        if (foliage.sum == 0) {
          
          euphotic.volume.mat[r,c] <- 0
          euphotic.tla.mat[r,c] <- 0
          oligophotic.volume.mat[r,c] <- 0
          oligophotic.tla.mat[r,c] <- 0
          empty.volume.mat[r,c] <- length(canopy.column.no.na) * xy.res * xy.res * z.res
          
        } else {
          
          #we need to reverse the order of this list since we want the upper most 65% so we need to start
          #our count from the top rather than the bottom
          canopy.lad.top.to.bottom <- rev(canopy.column.no.na)
          
          #we need to remove all voxels that have a value of zero because those are empty zones and are not used
          #in the calculation of euphotic or oligophotic zones - but we will save that information
          canopy.filled.voxels <- canopy.lad.top.to.bottom[canopy.lad.top.to.bottom != 0]
          
          empty.voxels <- length(canopy.lad.top.to.bottom) - length(canopy.filled.voxels)
          
          foliage.list <- list()
          
          for(i in 1:length(canopy.filled.voxels)) {
            
            if (i == 1) {
              foliage.list[[i]] <- canopy.filled.voxels[i]
            } else {
              foliage.list[[i]] <- sum(canopy.filled.voxels[1:i])
            }
          }
          
          #find the depth where 65% of tla is located
          foliage.depth.65 <- which(abs(unlist(foliage.list) - foliage.65) == min(abs(unlist(foliage.list) - foliage.65)))
          
          #can get canopy depth by dividing by xy.res squared
          euphotic.volume <- foliage.depth.65 * xy.res * xy.res * z.res
          euphotic.tla <- foliage.65
          
          oligophotic.volume <- (length(canopy.column.no.na) - foliage.depth.65) * xy.res * xy.res * z.res
          oligophotic.tla <- round(foliage.sum - foliage.65, digits = 4)
          
          empty.volume <- empty.voxels * xy.res * xy.res * z.res
          
          euphotic.volume.mat[r,c] <- euphotic.volume
          euphotic.tla.mat[r,c] <- euphotic.tla
          oligophotic.volume.mat[r,c] <- oligophotic.volume
          oligophotic.tla.mat[r,c] <- oligophotic.tla
          empty.volume.mat[r,c] <- empty.volume
          
        }
      }
    }
  }
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster for each level of the canopy using the original x,y data from the laz data
  euphotic.volume.raster <- raster::raster(euphotic.volume.mat,
                                           xmn = laz.data$x.bin[1],
                                           xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                           ymn = laz.data$y.bin[1],
                                           ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                           crs = crs.proj)
  
  euphotic.tla.raster <- raster::raster(euphotic.tla.mat,
                                        xmn = laz.data$x.bin[1],
                                        xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                        ymn = laz.data$y.bin[1],
                                        ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                        crs = crs.proj)
  
  oligophotic.volume.raster <- raster::raster(oligophotic.volume.mat,
                                              xmn = laz.data$x.bin[1],
                                              xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                              ymn = laz.data$y.bin[1],
                                              ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                              crs = crs.proj)
  
  oligophotic.tla.raster <- raster::raster(oligophotic.tla.mat,
                                           xmn = laz.data$x.bin[1],
                                           xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                           ymn = laz.data$y.bin[1],
                                           ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                           crs = crs.proj)
  
  empty.volume.raster <- raster::raster(empty.volume.mat,
                                        xmn = laz.data$x.bin[1],
                                        xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                        ymn = laz.data$y.bin[1],
                                        ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                        crs = crs.proj)
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  euphotic.volume.raster.flip <- flip(euphotic.volume.raster, direction = "y")
  euphotic.tla.raster.flip <- flip(euphotic.tla.raster, direction = "y")
  oligophotic.volume.raster.flip <- flip(oligophotic.volume.raster, direction = "y")
  oligophotic.tla.raster.flip <- flip(oligophotic.tla.raster, direction = "y")
  empty.volume.raster.flip <- flip(empty.volume.raster, direction = "y")

  #------------------------------------------------------------------------------------------
  #lets use a moving window average to take the 3x3 sd of each cell
  #------------------------------------------------------------------------------------------
  
  #empty matrices for moving window
  empty.canopy.volume.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  filled.canopy.euphotic.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  filled.canopy.oligophotic.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  filled.canopy.oligophotic.tla.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])
  filled.canopy.euphotic.tla.mat <- matrix(data = NA, nrow = dim(lad.array$rLAD)[2], ncol = dim(lad.array$rLAD)[3])

  #EMPTY CANOPY VOLUME
  
  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(empty.canopy.volume.mat) - 1)
  col.ind <- 2:(ncol(empty.canopy.volume.mat) - 1)
  
  #loop through these values 
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- empty.volume.mat[(r+1), (c-1)],
        nn <- empty.volume.mat[(r+1), (c+0)],
        ne <- empty.volume.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- empty.volume.mat[(r+0), (c-1)],
        cc <- empty.volume.mat[(r+0), (c+0)],
        ce <- empty.volume.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- empty.volume.mat[(r-1), (c-1)],
        ss <- empty.volume.mat[(r-1), (c+0)],
        se <- empty.volume.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      empty.canopy.volume <- sum(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      empty.canopy.volume.mat[r,c] <- empty.canopy.volume
    }
  }
  
  # FILLED CANOPY EUPHOTIC VOLUME
  
  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(filled.canopy.euphotic.mat) - 1)
  col.ind <- 2:(ncol(filled.canopy.euphotic.mat) - 1)
  
  #loop through these values 
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- euphotic.volume.mat[(r+1), (c-1)],
        nn <- euphotic.volume.mat[(r+1), (c+0)],
        ne <- euphotic.volume.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- euphotic.volume.mat[(r+0), (c-1)],
        cc <- euphotic.volume.mat[(r+0), (c+0)],
        ce <- euphotic.volume.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- euphotic.volume.mat[(r-1), (c-1)],
        ss <- euphotic.volume.mat[(r-1), (c+0)],
        se <- euphotic.volume.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      filled.canopy.euphotic <- sum(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      filled.canopy.euphotic.mat[r,c] <- filled.canopy.euphotic
    }
  }
  
  # FILLED CANOPY OLIGOPHOTIC VOLUME
  
  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(filled.canopy.oligophotic.mat) - 1)
  col.ind <- 2:(ncol(filled.canopy.oligophotic.mat) - 1)
  
  #loop through these values 
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- oligophotic.volume.mat[(r+1), (c-1)],
        nn <- oligophotic.volume.mat[(r+1), (c+0)],
        ne <- oligophotic.volume.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- oligophotic.volume.mat[(r+0), (c-1)],
        cc <- oligophotic.volume.mat[(r+0), (c+0)],
        ce <- oligophotic.volume.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- oligophotic.volume.mat[(r-1), (c-1)],
        ss <- oligophotic.volume.mat[(r-1), (c+0)],
        se <- oligophotic.volume.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      filled.canopy.oligophotic <- sum(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      filled.canopy.oligophotic.mat[r,c] <- filled.canopy.oligophotic
    }
  }
  
  # FILLED CANOPY EUPHOTIC TLA
  
  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(filled.canopy.euphotic.tla.mat) - 1)
  col.ind <- 2:(ncol(filled.canopy.euphotic.tla.mat) - 1)
  
  #loop through these values 
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- euphotic.tla.mat[(r+1), (c-1)],
        nn <- euphotic.tla.mat[(r+1), (c+0)],
        ne <- euphotic.tla.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- euphotic.tla.mat[(r+0), (c-1)],
        cc <- euphotic.tla.mat[(r+0), (c+0)],
        ce <- euphotic.tla.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- euphotic.tla.mat[(r-1), (c-1)],
        ss <- euphotic.tla.mat[(r-1), (c+0)],
        se <- euphotic.tla.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      euphotic.tla.canopy <- sum(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      filled.canopy.euphotic.tla.mat[r,c] <- euphotic.tla.canopy
    }
  }
  
  # FILLED CANOPY OLIGOPHOTIC TLA
  
  #since this is a moving window we cannot include the outside edges
  row.ind <- 2:(nrow(filled.canopy.oligophotic.tla.mat) - 1)
  col.ind <- 2:(ncol(filled.canopy.oligophotic.tla.mat) - 1)
  
  #loop through these values 
  for (r in row.ind) {
    for (c in col.ind) {
      
      moving.window <- list(
        #top row of moving window
        nw <- oligophotic.tla.mat[(r+1), (c-1)],
        nn <- oligophotic.tla.mat[(r+1), (c+0)],
        ne <- oligophotic.tla.mat[(r+1), (c+1)],
        
        #middle row of moving window
        cw <- oligophotic.tla.mat[(r+0), (c-1)],
        cc <- oligophotic.tla.mat[(r+0), (c+0)],
        ce <- oligophotic.tla.mat[(r+0), (c+1)],
        
        #bottom row of moving window
        sw <- oligophotic.tla.mat[(r-1), (c-1)],
        ss <- oligophotic.tla.mat[(r-1), (c+0)],
        se <- oligophotic.tla.mat[(r-1), (c+1)]
      )
      
      #lets calculate canopy rugosity
      oligophotic.tla.canopy <- sum(unlist(moving.window), na.rm = TRUE)
      
      #add this value to the new matrix
      filled.canopy.oligophotic.tla.mat[r,c] <- oligophotic.tla.canopy
    }
  }
  
  #this is the projection code for your site, change it as needed.
  crs.proj <- base::paste0("+init=epsg:", epsg.code)
  
  #now we can create a raster for each level of the canopy using the original x,y data from the laz data
  empty.canopy.volume.raster <- raster::raster(empty.canopy.volume.mat,
                                               xmn = laz.data$x.bin[1],
                                               xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                               ymn = laz.data$y.bin[1],
                                               ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                               crs = crs.proj)
  
  filled.canopy.euphotic.raster <- raster::raster(filled.canopy.euphotic.mat,
                                                  xmn = laz.data$x.bin[1],
                                                  xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                                  ymn = laz.data$y.bin[1],
                                                  ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                                  crs = crs.proj)
  
  filled.canopy.oligophotic.raster <- raster::raster(filled.canopy.oligophotic.mat,
                                                     xmn = laz.data$x.bin[1],
                                                     xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                                     ymn = laz.data$y.bin[1],
                                                     ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                                     crs = crs.proj)
  
  filled.canopy.oligophotic.tla.raster <- raster::raster(filled.canopy.oligophotic.tla.mat,
                                                         xmn = laz.data$x.bin[1],
                                                         xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                                         ymn = laz.data$y.bin[1],
                                                         ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                                         crs = crs.proj)
  
  filled.canopy.euphotic.tla.raster <- raster::raster(filled.canopy.euphotic.tla.mat,
                                                         xmn = laz.data$x.bin[1],
                                                         xmx = laz.data$x.bin[length(laz.data$x.bin)],
                                                         ymn = laz.data$y.bin[1],
                                                         ymx = laz.data$y.bin[length(laz.data$y.bin)],
                                                         crs = crs.proj)
  
  
  #we have to flip these rasters so that they are orientated the correct direction
  #this is standard when converting from an array to a raster
  empty.canopy.volume.raster.flip <- flip(empty.canopy.volume.raster, direction = "y")
  filled.canopy.euphotic.raster.flip <- flip(filled.canopy.euphotic.raster, direction = "y")
  filled.canopy.oligophotic.raster.flip <- flip(filled.canopy.oligophotic.raster, direction = "y")
  filled.canopy.oligophotic.tla.raster.flip <- flip(filled.canopy.oligophotic.tla.raster, direction = "y")
  filled.canopy.euphotic.tla.raster.flip <- flip(filled.canopy.euphotic.tla.raster, direction = "y")
  
  #return the final rasters
  final.data <- list("euphotic.volume.column.raster" = euphotic.volume.raster.flip, 
                     "euphotic.tla.column.raster" = euphotic.tla.raster.flip,
                     "oligophotic.volume.column.raster" = oligophotic.volume.raster.flip,
                     "oligophotic.tla.column.raster" = oligophotic.tla.raster.flip,
                     "empty.volume.column.raster" = empty.volume.raster.flip,
                     "empty.canopy.volume.raster" = empty.canopy.volume.raster.flip,
                     "filled.canopy.euphotic.raster" = filled.canopy.euphotic.raster.flip,
                     "filled.canopy.oligophotic.raster" = filled.canopy.oligophotic.raster.flip,
                     "filled.canopy.oligophotic.tla.raster" = filled.canopy.oligophotic.tla.raster.flip,
                     "filled.canopy.euphotic.tla.raster" = filled.canopy.euphotic.tla.raster.flip)
  
  return(final.data)
  
}
