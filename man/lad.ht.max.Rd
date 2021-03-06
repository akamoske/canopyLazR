% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxladht.R
\name{lad.ht.max}
\alias{lad.ht.max}
\title{Convert LAD estimates into two rasters - height of maximum LAD and maximum LAD within a column}
\usage{
lad.ht.max(lad.array, laz.array, ht.cut, epsg.code)
}
\arguments{
\item{lad.array}{LAD estimate array that was generated using the machorn.lad function.}

\item{laz.array}{Voxelized LiDAR array that was generated using the laz.to.array function. This contains
spatial information for all arrays.}

\item{ht.cut}{Height that calculations will exclude. This is to remove understory LAD estimates from
further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
you want to include all calculations}

\item{epsg.code}{EPSG code so that the rasters can be projected into the appropriate projection}
}
\value{
A list containing max LAD and height of max LAD rasters.
}
\description{
This function reads in a the LAD estimates that were previously calculated,
finds the maximum LAD value within each column of voxels, and then finds the height where that value
occurs. The output is a list containing two rasters, one for each calculation.
}
\details{
These forest structure attributes are based off calculations from:

Hardiman, B., Bohrer, G., Gough, C., & Curtis, P. (2013).
Canopy structural changes following widespread mortality of canopy dominant trees.
Forests, 4, 537-552. https://doi.org/10.3390/f4030537
}
