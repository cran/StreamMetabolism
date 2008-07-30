'EcosystemProduction.20' <-
function(velocity, meandepth, temperature, DO, day, startday="00:00:00", endday="23:45:00" ,sunrise.time, sunset.time, num.readings)
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
#correct for average nightime temperature
#get the windows for temperature
temp.sr <- window.chron(temperature,day,startday,day,sunrise.time)
temp.ss <- window.chron(temperature, day,sunset.time,day,endday)
#number of readings for temperature
num.temp.readings <- (sum(!is.na(temp.sr))+sum(!is.na(temp.ss)))
Avg.night.temp <- (sum(temp.sr, na.rm=TRUE)+sum(temp.ss, na.rm=TRUE))/num.temp.readings
#readings of temperature during the day
daytime.temp <- window.chron(temperature, day, sunrise.time, day, sunset.time)
#Average.Wholeday.Respiration taken out to nighttime total (*#of readings @ night)
#plus the sum of temperature corrected ER for the day 
#Erlandsen, M. Thyssen, N., 1983. Modelling the reaeration capacity of lowland stream dominated by submerged 
#macrophytes. Pp. 861-867. In:Lauenroth, W.K., Skogerboe, G.V., Flug, M., (eds). 
#Analysis of Ecological Systems:  State of the art ecological modelling. Elsevier.
#corrected for 20 deg Celcius
CR24 <- (Average.Wholeday.Respiration*num.respiration.readings)+(sum(Average.Wholeday.Respiration*(1.072^(daytime.temp-20))))*meandepth
NDM <- sum(Rearation.Flux, na.rm=TRUE)*meandepth
abs.CR24 <- abs(CR24)
GPP <- NDM+abs.CR24
P.R <- GPP/abs.CR24
Units <- "gO/squared meters/d"
metabolism <- cbind(CR24, NDM, abs.CR24, GPP, P.R, Units)
}