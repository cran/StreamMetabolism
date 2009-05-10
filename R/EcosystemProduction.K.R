`EcosystemProduction.K` <- function (K, meandepth ,temperature, DO, day, startday = 00:00:00, endday = 23:45:00, sunrise.time, sunset.time, num.readings) 
{
    require(zoo)
    require(chron)
    Ktemp <- Kt(K, temperature)
    Rearation.Flux <- rear.flux(DO, Ktemp, temperature, num.readings)
    respiration.sr <- window.chron(Rearation.Flux, day, startday, 
        day, sunrise.time)
    respiration.ss <- window.chron(Rearation.Flux, day, sunset.time, 
        day, endday)
    num.respiration.readings <- (sum(!is.na(respiration.sr)) + 
        sum(!is.na(respiration.ss)))
    Average.Wholeday.Respiration <- (sum(respiration.sr, na.rm = TRUE) + 
        sum(respiration.ss, na.rm = TRUE))/num.respiration.readings
    temp.sr <- window.chron(temperature, day, startday, day, 
        sunrise.time)
    temp.ss <- window.chron(temperature, day, sunset.time, day, 
        endday)
    num.temp.readings <- (sum(!is.na(temp.sr)) + sum(!is.na(temp.ss)))
    Avg.night.temp <- (sum(temp.sr, na.rm = TRUE) + sum(temp.ss, 
        na.rm = TRUE))/num.temp.readings
    daytime.temp <- window.chron(temperature, day, sunrise.time, 
        day, sunset.time)
    CR24 <- (Average.Wholeday.Respiration * num.respiration.readings) + 
        (sum(Average.Wholeday.Respiration * (1.072^(daytime.temp - 
            Avg.night.temp)))) * meandepth
    NDM <- sum(Rearation.Flux, na.rm = TRUE) * meandepth
    abs.CR24 <- abs(CR24)
    GPP <- NDM + abs.CR24
    P.R <- GPP/abs.CR24
    Units <- "gO/squared meters/d"
    metabolism <- cbind(CR24, NDM, abs.CR24, GPP, P.R, Units)
}

