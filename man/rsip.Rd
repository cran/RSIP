\name{Remote Sensing and Image Processing}

\alias{rsip}
\alias{maskFile}
\alias{exportValueGrid}
\alias{exportValuePoligon}
\alias{exportValuePointShp}
\alias{exportValuePointsTxt}
\alias{plotAll}
\alias{trmmToTiff}

\title{Remote Sensing and Image Processing}
\usage{

maskFile(shp_poligon, nam="RSIP_", dimname=c(17,26))

exportValueGrid()

exportValuePoligon(shp_poligon)

exportValuePointShp(shp_station)

exportValuePointsTxt(txt_xy)

plotAll(dimplot = c(3,4), color = c("red", "yellow",'green3',"cyan", "blue"),
                    zlim=c(-10,2000), dimname=c(17,23))

trmmToTiff()
}
\description{Operates with raster files (.tif extension).

\code{maskfile}: This function, cuts the raster from a polygon .shp and saves files to the same extent and area of proposed work.

\code{exportValueGrid}: Export all values of each cell (raster file) into a text format.

\code{exportValuePoligon}: Export values of each cell (raster) within a polygon.

\code{exportValuePointShp}: Export values of each cell from a .shp file. The .shp file are the type point locations, such as the locations of the weather stations.

\code{exportValuePointsTxt}: Export values of each cell from a .txt file.


\code{plotAll}: Plots raster files in chronological series.

\code{trmmToTiff}: Transforms the .nc4 file of the TRMM product (ftp://disc3.nascom.nasa.gov/data/s4pa/TRMM_L3/TRMM_3B42_Daily.7/) into .tif (raster) formats with daily rainfall values.
}

\arguments{
  \item{shp_poligon}{Name basin or polygon boundary. This data is .shp extension}
  \item{shp_station}{File name stations. This data is .shp extension point type}
  \item{txt_xy}{dataframe containing three columns, longitude, latitude and station name}
  \item{nam}{optional to put the names of files saved .tif}
  \item{dimname}{Vector of two terms, initial and final to determine the length of the original character in the name of the final file.}
  \item{dimplot}{Vector of two terms. It indicates the dimension or the organization of the final files for plotting.}
  \item{color}{Vector color for the representation of the cells in the plot files.}
  \item{zlim}{Vector of two terms. Indicate the minimum and maximum values of each cell in the .tif file.}
}

\details{
The \code{shp_station} file , is in the .shp extension. It is recommended that the first column of the attribute (also known as field .shp), the station name ("name") for labeling on export performance data is assigned.


The \code{nam} parameter  can be changed to sort or distinguish ordered and/or files organized way..

The \code{dimname} is optional, must be checked before a few files to determine the final file names. It is assigned position number of the characters wrong, it can overwrite the tif files.

\subsection{Input data}{
The .tif files, must be organized in a working folder, the files also limits basin (.shp format)
}
}

\value{
The results are located within the same folder of files income. They are located in the working folder of \code{R} [getwd()].
}

%\references{
%McKee, Thomas B. and Doesken, Nolan J. and Kleist, John. 1993. The relationship of Drought Frequency and Duration to %Time Scales. \emph{Eighth Conference on Applied Climatology}

%A. Belauneh and J. Adamowski. Standard Precipitation Index Drought Forecasting Using Neural Networks, Wavelet Neural %Networks, and Support Vector Regression. \emph{Applied Computational Intelligence and Soft Computing}, http://dx.doi%.org/10.1155/2012/794061
%\url{http://}
%}

\author{
Iván Arturo Ayala Bizarro <ivan.ayala@unh.edu.pe>

Jessica Zúñiga Mendoza <zumeje@gmail.com>
}

\note{Dependencies: the \code{RSIP} function, depend on the library \code{raster}, \code{rasterVis},  \code{rgdal}, \code{ncdf4} and \code{sp}.}

%\seealso{\code{\link{plotAll}} to para dibujar los raster formatos tif.}

\examples{
## Not run:
    ## File location
    #setwd("C:/Users/abia/Desktop/DataRSIP")

    ## Mask.
    #shp_poligon <- 'AB_Limit_Mantaro'  #To Assign the name poligon (cathment)
    #maskFile(shp_poligon, nam="RSIP_", dimname=c(17,26))

    ## Export value grid all.
    #exportValueGrid()

    ## Export value from poligon.
    #exportValuePoligon(shp_poligon)

    ## Export value from point .shp extension file
    #shp_station <-'AB_Station_Mantaro' #To Assign the name station
    #exportValuePointShp(shp_station)

    ## Export value from point .txt extension file (lon, lat, name)
    #txt_xy<-read.table('Data_station.txt', header=TRUE) #Read txt data
    data(Data_station)
    summary(Data_station)
    #exportValuePointsTxt(Data_station)

    ## Plot all tif file.
    #plotAll()

    ## 3B42 TRMM .nc4 files, processing to .tif files
    #trmmToTiff()

## End(**Not run**)
}
