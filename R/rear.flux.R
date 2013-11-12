`rear.flux` <-
function(DO, Ktemp, temp, interval){
	((dC.dt(DO))+(((Ktemp)*(DO-Cs(temp))))/interval)
}

