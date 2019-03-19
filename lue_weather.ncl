load "params/weather.ncl"
;load "pdum.ncl"


procedure lue_weather(cldy,rady)
begin

if (dimsizes(cldy&time).eq.365) then
	dumarq = addfile(auxdatafolder + "Ro.daily.nc","r")
else if (dimsizes(cldy&time).eq.366) then
	dumarq = addfile(auxdatafolder + "Ro-leap_year.daily.nc","r")
else
	print("ERROR IN WEATHER PROCEDURE: CHECK CLD INPUT DIMENSIONS")
end if
end if

r0 = dumarq->Ro(:,{domain(1):domain(0)},{domain(2):domain(3)})
delete(dumarq)




;Angstrom equation and conversion from W m^-2 to MJ m^-2 day^-2
rady = r0*(angcoefa + angcoefb*(1-(cldy/100.0))) * 0.0864

;print(rady&longitude)
;print(rady&latitude)
;print(rady(0,27,:))
;print(rady(0,27,31:33))
;print(rady&longitude(32))
;exit

;print(cldy(:,0,0))
;print(rady(:,0,0))


end
