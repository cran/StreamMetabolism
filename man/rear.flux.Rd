\name{rear.flux}
\alias{rear.flux}
\title{ Calculate Rearation Flux }
\description{
 Calculates Rearation Flux
}
\usage{
rear.flux(DO, Ktemp, temp, interval)
}
\arguments{
  \item{DO}{ Dissolved Oxygen mg/L }
  \item{Ktemp}{ Temperature Corrected K (Rearation)}
  \item{temp}{ Temperature degrees Celcius }
  \item{interval}{ Number of readings in a day 15min = 96 }
}
\details{
  zoo object as input
}
\value{time series of rearation flux (zoo object)
}
\references{ Bott, T.L. 1996. Primary Productivity and community respiration. In F. R. Hauer and G. A. Lamberti (ed.), Methods in stream ecology. Academic Press, Inc., New York, N.Y. }
\author{ Stephen A. Sefick Jr. }
\seealso{ \code{\link{ODobbins}}, \code{\link{Kt}}}
\examples{
#Use vel=0.6 and depth=0.4572
data(DOTemp)
d <- ODobbins(0.6, 0.4572)
f <- rear.flux(DOTemp[,2], Kt(d, DOTemp[,1]), DOTemp[,1], 96)
plot(f)
}
\keyword{ math }
