\name{usMap}
\alias{usMap}
\docType{data}
\title{
Shapefile of US state boundaries
}
\description{
Examples are provided using the \code{\link{usMap}} shapefile with a synthetic dataset.
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{
\url{https://www.cdc.gov/brfss/gis/gis_maps.htm}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
#read the shapefile
#display first five rows of shapefile dataset
#plot the shapefile boundaries

\dontrun{
usMap <- sf::as_Spatial(sf::st_read("shapes/usMap.shp", package="mapStats")[1]))
head(usMap@data)
plot(usMap)
}
}
