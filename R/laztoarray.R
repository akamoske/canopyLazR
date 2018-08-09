#' Convert a .laz or .las file to an array
#' 
#' This function reads in a list of .laz or .las files from a given file path and converts
#' each one into a voxelized array based on the x,y,z resolutions you specify.
#' 
#' @param laz.files.path Path to the folder containing the .laz or .las files
#' @param voxel.resolution The spatial resolution (x,y) you want the output voxel to be - this is a single 
#' number where both sides of the cell will be the same
#' @param z.resolution the vertical resolution of the voxel
#' @return A list of voxelized arrays
#' @export
laz.to.array <- function(laz.files.path, voxel.resolution, z.resolution) {
  
  #create an empty list
  laz.list <- list()
  
  #list all the files in the path
  #laz.files.list <- list.files(laz.files.path, full.names = TRUE)
  laz.files.list <- list.files(laz.files.path, pattern="\\.laz$", full.names = TRUE) # hard-coded extension.  Is this what we want? Or an option at function call?
  
  #loop through the files so that we can use memory management techniques
  for (i in 1:length(laz.files.list)) {
    
    #save the first file in the loop as a new variable
    laz.file <- laz.files.list[i]
    
    #read in laz files
    laz.data <- rlas::readlasdata(laz.file)
    
    #convert into a x,y,z, class table for easy reading and analysis
    laz.xyz.table <- as.data.frame(c(laz.data[,"X"], laz.data[,"Y"], laz.data[,"Z"], laz.data[,"Classification"]), 
                                   col.names = c("x", "y", "z", "class"))
    
    #lets remove the points classified as noise
    laz.xyz <- laz.xyz.table[laz.xyz.table$class != "7",]
    
    #lets create some boundary information for the x,y,z columns
    x.range.raw <- range(laz.xyz[,1], na.rm=T)
    y.range.raw <- range(laz.xyz[,2], na.rm=T)
    z.range.raw <- range(laz.xyz[,3], na.rm=T)
    
    #lets set how big we want each pixel to be, in this case 2x2x2
    x.y.grain <- voxel.resolution
    z.grain <- z.resolution
    
    #this is kind of repetitive, but makes sure that there are no rounding issues, thus any number
    #with a decimal is rounded down (floor) or up (ceiling), this helps elimate edge weirdness and cell size issues
    x.range <- c(floor(x.range.raw[1] / x.y.grain) * x.y.grain, ceiling(x.range.raw[2] / x.y.grain) * x.y.grain)
    y.range <- c(floor(y.range.raw[1] / x.y.grain) * x.y.grain, ceiling(y.range.raw[2] / x.y.grain) * x.y.grain)
    z.range <- c(floor(z.range.raw[1] / z.grain) * z.grain, ceiling(z.range.raw[2] / z.grain) * z.grain)
    
    #lets create the bins that will be used as cells later on
    #this creates a 2x2 bin for the x and y axis and a 2x2x2 bin for the z axis
    x.bin <- seq(x.range[1], x.range[2], x.y.grain)
    y.bin <- seq(y.range[1], y.range[2], x.y.grain)
    z.bin <- seq(z.range[1], z.range[2], z.grain)
    
    #find the idex number for each lidar pulse so that that pulse can be placed in the appropriate bin
    #rounding to the middle point of the pixel, this assigns a value to each lidar point
    x.cuts <- round((laz.xyz[,1] - x.bin[1] + x.y.grain / 2) / x.y.grain)
    y.cuts <- round((laz.xyz[,2] - y.bin[1] + x.y.grain / 2) / x.y.grain)
    z.cuts <- round((laz.xyz[,3] - z.bin[1] + z.grain / 2) / z.grain)    
    
    #here we turn the y value index numbers into a decimal place and then add that to the x value,
    #this will give us a number of unique values equal to the number of pixels in the empty raster
    #we can then order these, and only have to do indexing one time, rather than twice (x and y),
    #thus improve performance time and computing efficiency
    #integer is x index, float is y index
    y.cuts.dec <- y.cuts / (length(y.bin))
    x.y.cuts <- x.cuts + y.cuts.dec
    
    #lets create a data frame that has two columns, one with the x index values and another 
    #with the y index values in decimal form
    x.y.levels <- as.data.frame(expand.grid(1:(length(x.bin) - 1), (1:(length(y.bin) - 1)) / length(y.bin))) 
    colnames(x.y.levels) <- c("x.level", "y.level")
    
    #lets reorder the x.y.levels data frame so that they are in numerical order
    #first order = x lev, second order = y level
    x.y.levels <- x.y.levels[order(x.y.levels[,"x.level"], x.y.levels[,"y.level"]),]
    
    #now that the variables are ordered, lets break the data frame apart and store the 
    #data as a string of values
    x.y.levels.char <- as.character(x.y.levels[,"x.level"] + x.y.levels[,"y.level"])
    
    #assign pixel values (index values) to the 250,000 pixels in the newly formed raster
    x.y.cuts.factor <- factor(x.y.cuts, levels = x.y.levels.char)
    
    #lets create an empty matrix to store all this in, fill it with NA values for now so that it has shape
    xyz.matrix <- as.data.frame(matrix(NA, nrow = length(x.y.levels.char), ncol = 4, dimnames = list(NULL, c("x","y","z", "class"))))
    
    #lets bind this empty matrix with the lidar data frame
    xyz.table <- rbind(laz.xyz, xyz.matrix)
    
    #this determines the index number the z value of all lidar pulses, 
    #so that each lidar pulse is assigned a vertical voxel to live in
    z.index <- c(z.cuts, rep(NA, length(x.y.levels.char)))
    
    #this determines the index number for the x,y cell, so that each lidar pulse is assigned a raster cell to live in
    x.y.index <- c(x.y.cuts.factor, as.factor(x.y.levels.char))
    
    #lets combine the lidar table, the x,y index values, and the z index values into a data frame
    lidar.table <- data.frame(xyz.table, x.y.index = x.y.index, z.index = z.index)
    
    #lets create a function that populates the empty array that we will create later
    #this function will (for each factor in the data frame) seperate the pre-classified ground points
    #from the non-ground points, store the lowest ground value in the first row of the array, store the 
    #highest non-ground point in the second row, and the number of lidar pulses in each z voxel being stored
    #in each subsequent row with the lowest voxel being the 3rd row and the highest voxel being the last row
    lidar.array.populator <- function(x){
      ground.pts <- x[x[,4] == 2,]
      as.numeric(c(
        quantile(ground.pts[,3], prob = 0, na.rm = TRUE),
        quantile(x[,3], prob = 1, na.rm = TRUE),
        (table(c(x[,6], 1:length(z.bin) - 1)) - 1)
      ))
    }
    
    #lets see how many rows the empty array will have, this will be equal to the number of z voxels
    #this will also test to make sure that there are no initial errors when applying the above function 
    #to the whole data set
    z.vox.test <- length(dlply(lidar.table[1:2,], "x.y.index", lidar.array.populator)[[1]])
    
    #generate the final array using the lidar table, the x.y.index values as the factor, and the lidar.array.populator
    #as the function.  set the dimensions equal to the number of z voxels, the length of y.bin and the length of x.bin.
    #we need to subtract 1 from each of these values, because there is a 0 y.bin and x.bin, due to data creation
    #and edge effect mitigation -- there is no data stored here, but makes counting and rounding easier
    lidar.array <- array(as.vector(unlist(dlply(lidar.table, "x.y.index", lidar.array.populator, .progress = "text"))), 
                         dim = c(z.vox.test, (length(y.bin) - 1), (length(x.bin) - 1)))
    
    #lets return the array and the x.bin and y.bin so that we can have spatial information for later
    return.data <- base::list("array" = lidar.array, "x.bin" = x.bin, "y.bin" = y.bin, "z.bin" = z.bin)
    
    #save the data to the list
    laz.list[[i]] <- return.data
    
    #lets remove all the stuff we don't need anymore so that R doesn't have a melt down due to memory usage
    gc()
    remove(laz.data)
    remove(laz.xyz.table)
    remove(laz.xyz)
    remove(lidar.table)
    remove(lidar.array)
    remove(return.data)
    gc()
  }
  
  #return the list
  return(laz.list)
  
}
