`EcosystemProduction` <-
function(velocity, meandepth, temperature, DO, day, startday, endday ,sunrise.time, sunset.time, num.readings)
{
#StephenSeficksProductivityCalculator
require(zoo)
require(chron)
K <- ODobbins(velocity, meandepth)
Ktemp <- Kt(K, temperature)
Rearation.Flux <- rear.flux(DO, Ktemp, temperature, num.readings)
respiration.sr <- window.chron(Rearation.Flux,day,startday,day,sunrise.time)
respiration.ss <- window.chron(Rearation.Flux, day,sunset.time,day,endday)
num.respiration.readings <- (sum(!is.na(respiration.sr))+sum(!is.na(respiration.ss)))
Average.Wholeday.Respiration <- (sum(respiration.sr, na.rm=TRUE)+sum(respiration.ss, na.rm=TRUE))/num.respiration.readings
CR24 <- Average.Wholeday.Respiration*num.readings*meandepth
NDM <- sum(Rearation.Flux, na.rm=TRUE)*meandepth
abs.CR24 <- abs(CR24)
GPP <- NDM+abs.CR24
P.R <- GPP/abs.CR24
Units <- "gO/squared meters/d"
metabolism <- cbind(CR24, NDM, abs.CR24, GPP, P.R, Units)
}

