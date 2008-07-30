\name{Kt}
\alias{Kt}
\title{ Temperature Correction For Reaeration Value }
\description{
  Temperature Correction For Reaeration Value
}
\usage{
Kt(K, temp)
}
\arguments{
  \item{K}{ Rearation Coefficient single value or in a zoo object }
  \item{temp}{ temperature value at time t in Degrees Celcius }
}
\value{
  Single Values or zoo series
}
\references{ Thyssen, N., Erlandsen, M., Jeppesen, E., Holm T. F.,1983. Modelling the reaeration capacity of low-land streams  }
\author{ Stephen A Sefick Jr }
\examples{
#data USGS
data(DOTemp)
#velocity 0.6, depth 0.4572
d <- ODobbins(0.6, 0.4572)
Kcorr <- Kt(d , DOTemp[,1])
}
\keyword{ math }