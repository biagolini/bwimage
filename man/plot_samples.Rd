% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImageAnalysis.R
\name{plot_samples}
\alias{plot_samples}
\title{Plot samples from denseness_sample}
\usage{
plot_samples(imagematrix, central_lines, central_collumns, width_size,
  height_size, sample_width, sample_height, sample_shape)
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}

\item{central_lines}{Lines data (i.e. "Matrix(line)") provided by denseness_sample}

\item{central_collumns}{Collumns data (i.e. "Matrix(column)") provided by denseness_sample}

\item{width_size}{Real size of image width (in mm, cm, m, etc..).}

\item{height_size}{Real size of image height (in mm, cm, m, etc..).}

\item{sample_width}{Width of sample area.}

\item{sample_height}{Height of sample area.}

\item{sample_shape}{Inform the shape of sample unity used ("rectangle" or "ellipse"). See denseness_sample function.}
}
\value{
Plot of the analysed matrix (black and white) and sample locations (red).
}
\description{
Plot samples from denseness_sample.
}
\examples{
# Get a matrix from your image. Here  examples provided by bwimage package.

bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
a<-denseness_sample(bush, width_size=100, height_size=100, sample_width=5, sample_height=5)
plot_samples(bush, a[,4],a[,5], 100,100, 5, 5,"rectangle")
}
\seealso{
denseness_sample
}
\author{
Carlos Biagolini-Jr.
}
