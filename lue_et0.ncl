
procedure lue_et0(spressure,tempd,radd,et0)
local delta,lhtvap,pcgamma
begin

;Slope of vapour presure curve (kPa oC^-1) (FAO 56 Annex 2 table 2.4)
delta = (4096.0 * (0.6108 * exp((17.27*tempd) / (tempd + 237.3)))) / ((tempd + 237.3) ^2) 

;Water latent heat of vaporization (MJ kg^-1)
;TODO: This value is a weak function of temperature L = 0.00864*(28.9-0.028*t)
lhtvap = 2.45 ;MJ kg^-1

;Psychrometric constant (kPa oC^-1) (FAO 56 Eq. 8)
pcgamma =  (1.013e-3 * spressure) / (0.622 * lhtvap) 

;Calculating reference evapotranspiration using the Priestley-Taylor equation (Praveen et. al. 2011)
;Considering the soil heat flux zero for the daily timestep
;Considering the reference surface albedo of 0.23 (FAO 56)
;TODO: Longwave balance for this method, radiation here should be net, not solar
;et0 = 1.26 * ( delta / ( delta + pcgamma )) * ( radd*(1 - 0.23) / lhtvap )

;Makkink 1957 (Referenced in ETH Zurich: Empirical ET Models, also in Sediyama et. al. 1997)
et0 = ( delta / ( delta + pcgamma )) * ( 0.61 * ( radd / lhtvap) - 0.12 )


end
