'EcosystemProduction.20' <- function (velocity, meandepth, temperature, DO, day, startday = "00:00:00", endday = "23:45:00", sunrise.time, sunset.time, num.readings){
    require(zoo)
    require(chron)
    K <- ODobbins(velocity, meandepth)
    Ktemp <- Kt(K, temperature)
    Rearation.Flux <- rear.flux(DO, Ktemp, temperature, num.readings)
    respiration.sr <- window.chron(Rearation.Flux, day, startday, day, sunrise.time)
    respiration.ss <- window.chron(Rearation.Flux, day, sunset.time, day, endday)
    #number of nighttime respiration readings
    num.respiration.readings <- (sum(!is.na(respiration.sr)) + sum(!is.na(respiration.ss)))
    #this is average nighttime repiration!!!!!!!
    Average.Wholeday.Respiration <- (sum(respiration.sr, na.rm = TRUE) + sum(respiration.ss, na.rm = TRUE))/num.respiration.readings
    temp.sr <- window.chron(temperature, day, startday, day, sunrise.time)
    temp.ss <- window.chron(temperature, day, sunset.time, day, endday)
    #in the night
    num.temp.readings <- (sum(!is.na(temp.sr)) + sum(!is.na(temp.ss)))
    Avg.night.temp <- (sum(temp.sr, na.rm = TRUE) + sum(temp.ss, na.rm = TRUE))/num.temp.readings
    daytime.temp <- window.chron(temperature, day, sunrise.time, day, sunset.time)
    #remember that this is Average.Wholeday.Respiration (nighttime) multiplied by number of readings to get contribution to respiration at                    	#night.  Then correct for temperature during the day becuase you assume that it will change above the average because you can't "measure" 	#this as you can with the nighttime respiration rate becuase at night it is assumed that only respiration is occuring.
    CR24 <- (Average.Wholeday.Respiration * num.respiration.readings) + (sum(Average.Wholeday.Respiration * (1.072^(daytime.temp - Avg.night.temp)))) * meandepth
    NDM <- sum(Rearation.Flux, na.rm = TRUE) * meandepth
    abs.CR24 <- abs(CR24)
    GPP <- NDM + abs.CR24
    P.R <- GPP/abs.CR24
    Units <- "gO/squared meters/d"
    metabolism <- cbind(CR24, NDM, abs.CR24, GPP, P.R, Units)
}