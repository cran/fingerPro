\name{fingerPro-package}
\alias{fingerPro-package}
\alias{fingerPro}
\docType{package}
\title{
  Sediment Source Fingerprinting
}
\description{
Soil erosion is one of the biggest challenges for food production and reservoirs siltation around the world. Information on sediment, nutrients and pollutant transport is required for effective control strategies. Source estimates are difficult to obtain using traditional monitoring techniques, but sediment source fingerprinting, has been proved to be a valuable tool. Sediment source fingerprinting offers the potential to assess sediment provenance as a basis to develop management plans and prevent erosion.  The procedure focuses on developing methods that enable the apportionment of sediment sources to be identified from a composite sample of sediment mixture material. We developed an R-package as a tool to quantify the provenance of the sediments in a catchment. A mixing model algorithm is applied to the sediment mixture samples in order to estimate the relative contribution of each potential source. The package consists of a set of functions used to:  i) characterise and pre-process the data, select the optimum subset of tracers; ii) unmix sediment samples and quantify the apportionment of each source; iii) assess the effect of the source variability; and iv) visualize and export the results.
}
\author{
Ivan Lizaga, Borja Latorre, Leticia Gaspar, Ana Navas

Maintainer: Ivan Lizaga <ilizaga@eead.csic.es // lizaga.ivan10@gmail.com>
}
% Optionally other standard keywords, one per line,
% from the file KEYWORDS in the R documentation.
\keyword{ Sediment source fingerprinting package }
\seealso{
  https://github.com/eead-csic-eesa
}
\examples{
#Load the dataset called "catchment" 
data <- catchment
LDAPlot(data, P3D=FALSE)
#
data <- rangeTest(data)
data <- KWTest(data)
#Too many tracer properties were removed
LDAPlot(data, P3D=FALSE)
#2D and 3D LDAPlots suggest that two of the sources have to be combined

#reload the original dataset "catchment"
data <- catchment
# Combine sources PI1 and PI based on the previous LDAPlot
data$Land_Use[data$Land_Use == 'PI1'] <- 'PI'
data <- rangeTest(data)
data <- KWTest(data)
LDAPlot(data, P3D=FALSE)
#Now the optimum tracer properties selected discriminate well, so proceed with the unmix function
result <- unmix(data, samples = 100, iter =100)
#Display the results
plotResults(result, y_high = 4.5, n = 1)
writeResults(result)
}
