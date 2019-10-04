\name{bwimage-package}
\alias{bwimage-package}
\alias{bwimage}
\docType{package}
\title{
\packageTitle{bwimage}
}
\description{
\packageDescription{bwimage}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{bwimage}
\packageIndices{bwimage}
A computational tool to describe patterns in black and white images from natural structures.
}
\author{
\packageAuthor{bwimage}

Maintainer: \packageMaintainer{bwimage}
}
\references{
Biagolini-Jr C, Macedo RH (2019) bwimage: A package to describe image patterns in natural structures. F1000Research 8
Lambers M (2016) Mappings between sphere, disc, and square. Journal of Computer Graphics Techniques Vol 5:1-21
Nobis M, Hunziker U (2005) Automatic thresholding for hemispherical canopy-photographs based on edge detection. Agricultural and forest meteorology 128:243-250
Shirley P, Chiu K (1997) A low distortion map between disk and square. Journal of graphics tools 2:45-52
Zehm A, Nobis M, Schwabe A (2003) Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants 198:142-160


}

\concept{animal}
\concept{ecology methods}
\concept{field}
\concept{image analyses}
\concept{image processing}
\concept{vegetation patterns}

\examples{
bush<-system.file("extdata/bush.JPG", package ="bwimage")
bush_imagematrix<-threshold_color(bush, "jpeg", "proportional",compress_rate = 0.1)
aggregation_index(bush_imagematrix)
}
