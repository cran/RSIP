\name{rsipDataSet}

\alias{Data_station}

\title{The data set for illustratrating the functions of the RSIP package}

\description{The data are the locations of the weather stations with names.}

\usage{
data(Data_station)
}

\format{
\code{PData_station} dataframe with:
\describe{
\item{Lon}{longitude in degrees}
\item{Lat}{latitude in degrees}
\item{Name}{Weather Stations names.}
}
}

%\references{
%McKee, Thomas B. and Doesken, Nolan J. and Kleist, John. 1993. The relationship of Drought Frequency and Duration to Time Scales. \emph{Eighth Conference on Applied Climatology}

%A. Belauneh and J. Adamowski. Standard Precipitation Index Drought Forecasting Using% Neural Networks, Wavelet Neural Networks, and Support Vector Regression. \emph{Applied Computational Intelligence and Soft Computing}, http://dx.doi.org/10.1155/2012/794061
%\url{http://}
%}

%\source{The \code{Pm_Pisco} data were obtained from the Peruvian Interpolation data of the SENAMHI's Climatological and Hidrological Observations, SENAMHI-PERU. \url{http://peruclima.pe/}.}

\author{
Iván Arturo Ayala Bizarro <ivan.ayala@unh.edu.pe>

Jessica Zúñiga Mendoza <zumeje@gmail.com>
}

%\note{}

%\seealso{}

\examples{
data(Data_station)
names(Data_station)
summary(Data_station)
}

%\keyword{}
%\keyword{}
