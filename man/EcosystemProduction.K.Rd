\name{EcosystemProduction.K}
\alias{EcosystemProduction.K}
\title{ Calculate Ecosystem Production with the Single Station Method With Temperature Correction (Thyssen) for Respiration.  In other words average night time respiration is corrected for the change in temperature for the day from the average temperature for the night. }
\description{
You get to provide the rearation rate K}
\usage{
EcosystemProduction.K(K, meandepth, temperature, DO, day, startday = 00:00:00, endday = 23:45:00, sunrise.time, sunset.time, num.readings)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{ Calculated or Measured Rearation \code{K} }
   \item{meandepth}{ mean stream channel depth in meters }
  \item{temperature}{ Time Series temperature in degrees Celcius }
  \item{DO}{ Time Series Dissolved Oxygen in mg/L }
  \item{day}{ date of the day of interest must be in quotes }
  \item{startday}{ time of the start of the "day" usually 00:00:00 must be in quotes}
  \item{endday}{ time of the end of the "day" usually 23:45:00 must be in quotes}
  \item{sunrise.time}{ time of sunrise in the form 04:22:00 must be in quotes}
  \item{sunset.time}{ time of sunset in the form 19:23:00 must be in quotes}
  \item{num.readings}{ number of readings if data is collected in 15min intervals then there are 96 readings per day }
}
\details{

**The only difference between this function and EcosystemProduction.20 is you supply K.

This function has only been tested with zoo objects read in with the read.production function.  Should work for others, but untested.  The values that are returned are only as good as the assumptions that went into collecting the data and should be viewed with scrutiny.  Also the Reaeration coefficient is the most important number used in the calculation of Production and therefore needs to be as correct as possible.
}

\value{
  \item{CR24 }{Community Repiration 24hours (sum of pre-dawn and post-dusk change in Oxygen curve corrected for Reaeration)}
  \item{ NDM }{Net Daily Metabolism (sum of total day change in Oxygen Curve corrected for Reaeration)}
  \item{ abs.CR24 }{absolute value of Community Repiration 24hours (because CR24 is always negative)}
  \item{ GPP }{Gross Primary Productivity=NDM+abs.CR24}
  \item{ P.R }{P/R ratio}
  \item{ Units }{areal units (gOxygen/squared meter/day) of everything except P.R which is unitless}
}

\references{ Odum, H. T. (1956). "Primary production in flowing waters." Limnology and Oceanography, 1: 102-117.

Thyssen, N., Erlandsen, M., Jeppesen, E., Holm T. F.,1983. Modelling the reaeration capacity of low-land streams

M.R. Grace and S.J. Imberger. 2006. "Stream Metabolism: Performing & Interpreting 
Measurements". Water Studies Centre Monash University, Murray Darling Basin Commission  and 
New South Wales Department of Environment and Climate Change.  204 pp. 
Accessed at 
http://www.sci.monash.edu.au/wsc/docs/tech-manual-v3.pdf 


}
\author{ Stephen A Sefick Jr. }
\examples{
#zoo real data
#velocity 0.6
#depth 0.4572
#sunrise 6:00AM
#sunset 8:15PM
#96 is the number of intervals in a day for fifteen minute data
data(DOTemp)

K <- ODobbins(0.6, 0.4572)
prod.K <- EcosystemProduction.K(K, 0.4572, DOTemp[,1], 
DOTemp[,2], "8/18/70", "00:00:00", 
"23:45:00", "06:00:00", "20:15:00", 96)
prod.K

#compare to EcosystemProduction.20
prod.20 <- EcosystemProduction.20(0.6, 0.4572, 
DOTemp[,1], 
DOTemp[,2], "8/18/70", "00:00:00", 
"23:45:00", "06:00:00", "20:15:00", 96)


prod.K
prod.20
}
\keyword{ math }
