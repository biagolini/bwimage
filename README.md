# bwimage
**Author:**  Carlos Biagolini-Jr.

**License:** GPL (>= 2)

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bwimage)](https://cran.r-project.org/package=bwimage)
[![Downloads](https://cranlogs.r-pkg.org/badges/bwimage)](https://CRAN.R-project.org/package=bwimage)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/bwimage?color=orange)](https://CRAN.R-project.org/package=bwimage)
[![Travis build status](https://travis-ci.org/biagolini/bwimage.svg?branch=master)](https://travis-ci.org/biagolini/bwimage)
[![Build status](https://ci.appveyor.com/api/projects/status/yomvstj7grbq3vv8?svg=true)](https://ci.appveyor.com/project/biagolini/bwimage)
[![Codecov test coverage](https://codecov.io/gh/biagolini/bwimage/branch/master/graph/badge.svg)](https://codecov.io/gh/biagolini/bwimage?branch=master)
<!-- badges: end -->

> A computational tool to create sequences of animal color tags

## Introdution
The facility to obtain high quality digital images creates the opportunity to measure natural variables using image analyses. Bwimage is an R package to analyze patterns in black and white images from natural structures. 
## Methods
Bwimage s analysis of images is based on the transformation from a picture (jpeg and png files are allowed) to a binary matrix (Figure 1). For each pixel, the intensity of red, green and blue is averaged and compared to a threshold. If the average intensity is less than the threshold (default is 50%) the pixel will be set as black, otherwise it will be white
## Image compaction
By reducing resolution, the accuracy of data description will also be lowered. If the user is not acquainted with scale and threshold processing and/or images were captured under different light conditions, I recommend the scale and application of threshold algorithms in a native image editor software, such as GIMP (https://www.gimp.org/), and subsequent usage of the resulting images with the bwimage package.
## Installing the release version of the `bwimage` R package

You can install `GenTag` from CRAN with:

``` r
install.packages("bwimage")
```

### Installing the development version of the `GenTag` R package
You can install the development version from GitHub with:

``` r
devtools::install_github("biagolini/bwimage")
```

## Usage
Download bwimage and load into R
``` r
install.packages("bwimage ")
library("bwimage")
```
Examples below illustrate how to use bwimage to describe patterns in black white images.

Calculate canopy openness
A photo was taken with a camera placed in the ground, perpendicular to the ground. Canopy openness is equal the proportion of white pixels in relation to all image pixels.
``` r
# Load a path to an image example
canopy<-system.file("extdata/canopy.JPG",package ="bwimage")
# Convert image into binary matrix
canopy_matrix<-threshold_color(canopy, compress_method="proportional", compress_rate=0.1)
# Calculate canopy openess
image_information(canopy_matrix)[2]/image_information(canopy_matrix)[4]
```
Describe vertical vegetation complexity from a vegetation plot of 30x100cm. An example of vegetation image was produced based in method described by Zehm et al 2003.
Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora. 
``` r
# Load a path to an image example
bush<-system.file("extdata/bush.JPG",package ="bwimage")
# Convert image into binary matrix
bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#  Quantification of vegetation denseness 
denseness_total(vegetation_matrix)

```
Quantification nest wall openness and the aggregation of nest wall holes. A photo of a bird nest was taken with a white styrofoam ball (50mm in diameter) placed inside nest chamber; than threshold processing and transparent pixels from background elements were performed in GIMP software.

``` r
# Load a path to an image example
nestwall<-system.file("extdata/bird_nestwall.png", package ="bwimage")
# Convert image into binary matrix
nestwall_imagematrix<-threshold_color(nestwall, "png","width_fixed",target_width=300)
#  Quantification of nest wall openness
image_information(nestwall_imagematrix)[2]/image_information(nestwall_imagematrix)[4]
#  Quantification of holes agregation
aggregation_index(nestwall_imagematrix)
```
Carlos Biagolini-Jr.  [@biagolini] c.biagolini@gmail.com

## Contributing
 fork <https://github.com/biagolini/bwimage/fork>
 
##  Citation information
An article of bwimage appliance to field data is under publication process. 
Check out CRAN documentation at https://cran.r-project.org/package=bwimage
