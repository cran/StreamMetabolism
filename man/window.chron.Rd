\name{window.chron}
\alias{window.chron}
\title{Time Windows of Diurnal Curves}
\description{
  Takes a time window of a larger series
}
\usage{
\method{window}{chron}(x, day1, hour1, day2, hour2, ...)
}
\arguments{
  \item{x}{ data to be subsetted}
  \item{day1}{ start day must be in "" }
  \item{hour1}{ start time must be in "" }
  \item{day2}{ end date must be in "" }
  \item{hour2}{ end time must be in "" }
  \item{...}{ other arguments }
}
\value{
  subset by time
}
\references{ chron, window, window.zoo }
\author{ Stephen A Sefick Jr. }
\seealso{\code{\link{window}}}
\examples{
#with real data
data(DOTemp)
d <- window.chron(DOTemp, "8/18/70", "06:00:00", "8/18/70", "20:15:00")
plot(d)
}
\keyword{ chron }
