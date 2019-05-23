#' Convert a lidar array into an array of LAD estimates
#'
#' This function reads in the lidar array created with the laz.to.array function and uses the MacArthur and Horn method to
#' estimate LAD within each voxel. The sum of each column is equal to the LAI of that column of voxels.
#'
#' @param leveld.lidar.array Leveled lidar array, which is the output of the canopy.height.levelr function
#' @param voxel.height The Z parameter that was entered into the laz.to.array function
#' @param beer.lambert.costant If you do not have a Beer Lambert Constant then set this NULL. If you have one, then enter it here.
#' @return A list of voxelized array which contain LAD estimates within each voxel
#' @export

machorn.lad <- function(leveld.lidar.array, voxel.height, beer.lambert.constant = NULL){

  #first lets just use the pulse totals in each voxel, we do not need the ground or canopy elevations at this point
  voxel.N.pulse <- leveld.lidar.array$array[3:dim(leveld.lidar.array$array)[1],,]

  #lets create an empty array to store the pulse accumulations in
  pulse.accum <- array(0, dim = dim(voxel.N.pulse))

  #since a NA return is the same as 0 in this case, lets set all NA values to zero for easier calculations
  voxel.N.pulse[is.na(voxel.N.pulse)] <- 0

  #lets write a loop that goes through all the rows of the array. In this situation the rows are the vertical bins (z.bins)
  #of the lidar array. This loops cycles through these starting at the top and working its way to the bottom.
  for(i in (dim(voxel.N.pulse)[1]):1) {

    #if we are looking at the top of the canopy (the last row in the array or the largest z.bin) then lets populate
    #our empty array with the same values that are in the voxel.N.pulse array (total pulses in each voxel)
    if(i == (dim(voxel.N.pulse)[1])) {
      pulse.accum[i,,] <- voxel.N.pulse[i,,]
    }

    #if we are looking at any other slice of the array or voxel that is not the top of the canopy (last row in the array)
    #then lets set that matrix slice (one below the previous voxel) in our empty array to the number of pulses in that canopy position
    #(from the voxel.N.pulse) plus the number of pulses in the voxel above. this produces an accumulation of pulses throughout the canopy.
    else {
      pulse.accum[i,,] <- pulse.accum[i + 1,,] + voxel.N.pulse[i,,]
    }
  }

  #now lets create a new array that has the number of ground returns for each column at every slice of the matrix (thus the ground points are available
  #at each vertical voxel). basically take the ground accumulation and stack it in an array equal to the number of bins in the z.bin or previously
  #generated array
  pulse.all <- array(rep(pulse.accum[1,,], each = dim(voxel.N.pulse)[1]),
                     dim = dim(voxel.N.pulse))

  #now we can figure out how many pulses went through each voxel (thus if there were 1000 ground hits and 100 hits at the top of the canopy, then
  #900 pulses would have went through that first voxel)
  shots.through <- pulse.all - pulse.accum

  #lets create another empty array this time to adjust for the pulses going in rather than the pulses going out
  shots.in <- array(dim = dim(voxel.N.pulse))

  #lets fill in the empty array we just created. first we need to shift all the shots out values down one voxel. this is because in the shots out
  #array the ground is a value of 0 because no pulses traveled through this column. but the accumulation of pulses above it are the number of pulses
  #that traveled into the voxel
  shots.in[1:(dim(voxel.N.pulse)[1] - 1),,] <- shots.through[2:(dim(voxel.N.pulse)[1]),,]

  #finally, lets add the number of pulses accumulated at the ground to the top. this is how many pulses entered the top of the canopy.
  shots.in[(dim(voxel.N.pulse)[1]),,] <- pulse.accum[1,,]

  ## Add the usually unadjusted LAD calculation
  # unadjLAD = ln(si-1/si)*(1/(dz)) or LAD = ln(si-1/si)*(1/(k*dz)) depending on whether k is NULL or not.
  # rLAD stands for raw LAD ... but can be unadjusted or adjusted depending on whether k is set.

  #now lets apply the LAD calculation based on the MacArthur Horn Method and Beer-Lambert Law.
  #if k = NULL: unadjusted LAD = ln(Si - 1 / Si) * (1 / dz)
  #if k = a number: LAD = ln(Si - 1 / Si) * (1 / (k * dz))
  #where, Si = number of pulses in each voxel, K = Beer-Lambert constant, dz = voxel.height

  #lets set the Beer-Lambert constant <- k for easier writing
  k <- beer.lambert.constant
  dz <- voxel.height

  #if no beer.lambert.constant is given then lets do this equation
  if(is.null(k)){

    #unadjusted lad = number of shots that went into each voxel divided by the number of shots that went through each voxel
    #then we multiple it by a theoretical beer lambert constant of 1 divded by the voxel height
    #we then need to assign all NaN and infinite values to NA
    rLAD <- log(shots.in/shots.through) * (1 / dz)
    rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
  }

  #if a beer.lambert.constant is given then lets do this equation
  else {

    #lets let the user know that they set the constant and what the constant is
    print("MacArthur-Horn constant is set! k = ")
    print(k)

    #now we are going to do the same thing as above but with the beer lambert constant put in
    rLAD <- log(shots.in/shots.through) * (1 / (k * dz))
    rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
  }

  #lets replace the zero's that we had to add into the array to make the previous code work into NA values
  #so that we can more easily calculate attributes in later steps
  for (r in 1:dim(rLAD)[2]) {
    for (c in 1:dim(rLAD)[3]) {

      #we need to add 2 to this because the value in the first slot is NA to initiate the array and we want to be
      #in the next voxel above the one with the last value, thus we need to add 2
      na.cut <- ceiling(leveld.lidar.array$array[2,r,c]) + 2

      rLAD[na.cut:dim(rLAD)[1],r,c] <- NA
    }
  }

  #lets make a list to store the data we need to complete the process later on
  out <- list();
  out$rLAD <- rLAD
  out$shots.in <- shots.in

  #memory managment
  gc()
  remove(leveld.lidar.array)
  remove(pulse.accum)
  remove(voxel.N.pulse)
  gc()

  #return the final list
  return(out)

}
