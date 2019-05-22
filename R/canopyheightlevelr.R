#' Level a voxelized array to the ground, mimicing a canopy height model
#'
#' This function reads in a list of voxelized arrays and levels them to the ground, thus each array
#' will mimic a canopy height model, making comparisions between voxels easier
#'
#' @param lidar.array Voxelized array that was created with the laz.to.array function
#' @return A list of leveled voxelized arrays
#' @export

canopy.height.levelr <- function(lidar.array){

  #lets store the pulse array as a different variable
  l.array <- lidar.array$array

  #lets create an empty array to store the new values in
  chm.pulses <- array(data = NA, dim = dim(l.array))

  #lets loop through each vertical column in the array and do some stuff to it
  for(q in 1:dim(l.array)[2]) {
    for(z in 1:dim(l.array)[3]) {

      #lets save the vertical column we are in as its own varaible
      lidar.col <- l.array[,q,z]

      #lets save the ground elevation
      ground <- lidar.col[1]

      #lets save the max canopy height elevation
      canopy <- lidar.col[2]

      #first, lets put a check in here and make sure that there is actually a ground value,
      #if there is a real ground value then lets do some stuf
      if(ground > 0 & !is.na(ground)){

        #lets figure out how tall the canopy is in this forest canopy column
        canopy.ht <- canopy - ground

        #lets save all the pulse returns as its own variable, since the ground and canopy
        #height are stored in the first two positions
        returns <- lidar.col[3:length(lidar.col)]

        #lets save the vertical column as another variable so that we can make some changes to it
        ch.col <- lidar.col

        #lets set the ground value to zero, since this is a canopy height model, ground is always zero
        ch.col[1] <- 0

        #lets set the canopy height (from ground) to the second position
        ch.col[2] <- canopy.ht

        #lets use a which statement to figure out what are the index values of voxels that actually have
        #pulse returns in them
        g.pulse <- which(ch.col[3:length(ch.col)] > 0)

        #lets that the index of the ground pulses and add 2 to it (this takes care of the
        #ground and canopy height in the first two positions)
        g.ind <- g.pulse[1] + 2

        #lets make a new vertical column of voxel that is based on the canopy height model,
        #thus we move the ground pulses to the ground (zero) position, rather than its elevation
        cht.col <- c(ch.col[1:2], ch.col[g.ind:length(ch.col)])

        #we need to add some zeros to the end of this column to make it match the same
        #about of voxels in our array, thus lets make a new index value so we can add them
        #after the values we just moved over
        new.index <- length(cht.col) + 1

        #lets add the appropriate number of zeros to the end of the vertical column so it
        #matches the correct dimensions
        cht.col[new.index:length(ch.col)] <- 0

        #lets save this vertical column to our empty array
        chm.pulses[,q,z] <- cht.col

      }

      #if there isn't a ground value then lets set all of that column to NA
      else {
        chm.pulses[,q,z] <- NA
      }
    }
  }

  #lets return our outputs
  return.data <- base::list("array" = chm.pulses,
                            "x.bin" = lidar.array$x.bin,
                            "y.bin" = lidar.array$y.bin,
                            "z.bin" = lidar.array$z.bin)

  #lets remove some variables for memory management
  gc()
  remove(lidar.array)
  gc()

  #return the final list
  return(return.data)

}
