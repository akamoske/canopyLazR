#' Convert canopy height model into a top of canopy rugosity raster
#'
#' This function reads in a the CHM previously calculated and determines the amount of empty space at the top
#' of the canopy compared to the other trees within a 3x3 moving window. The output is the volume of this area.
#' 
#' These forest structure attributes are based off calculations from:
#'
#' Lefsky, M.A., Cohen, W.B., Acker, S.A., Parker, G.G., Spies, T.A., and Harding, D. (1999).
#' Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of Douglas-Fir Western Hemlock Forests.
#' Remote Sensing of the Environment, 70, 339-361. https://doi.org/10.1016/S0034-4257(99)00052-8
#'
#' @param chm.raster CHM that was generated using the array.to.ground.and.canopy.rasters function.
#' @param xy.res Horizontal resolution of the CHM raster - if it is 10x10 meters then enter 10
#' @param z.res vertical resolution of the original LAD estimates - if it is 1 meter then enter 1
#' @return A raster showing the volume of empty space at the top of the canopy.
#' @export

toc.rugosity <- function(chm.raster, xy.res, z.res) {
  
  #top of canopy rugosity
  rugosity <- function(x) {
    max.ht <- max(x)
    diff.list <- list()
    q <- 1
    for (i in 1:length(x)){
      ht.diff <- max.ht - x[i]
      diff.list[[q]] <- ht.diff
      q <- q + 1
    }
    tot.diff <- sum(unlist(diff.list), na.rm = TRUE) * xy.res * xy.res * z.res
    return(tot.diff)
  }
  
  #3x3 moving window to calculate the volume of the empty space at the top of the canopy
  toc.rugosity <- focal(chm.raster, w = matrix(1, nrow = 3, ncol = 3), fun = rugosity)
  
  #return the final rasters
  return(toc.rugosity)
}
