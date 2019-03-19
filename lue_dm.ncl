load "params/crop.ncl"


procedure lue_dm(jday,gdd,plantdead,tempy,rady,ksd,ktd,laid,ddmd,ldmd,sdmd,gdmd,fleafy,fstemy,dumy)
local par,fapar
begin
;Uses the approach of Oliveira (2012), where the dry matter daily gain is a function of a Light Use Efficiency (LUE), that converts APAR directly to dry matter, APAR, a soil water stress factor and a temperature stress factor.
;The allocation factors of dry matter to leaf and grain pools in each phenological phase are determined in the crop parameter file. TODO: Make them more dynamic, also simulate the root DM pool.

;print((/jday/))
;print((/ldmd(0,0)/))


;Assumes PAR is a fixed fraction of incoming radiation (Gabriel Abrahao: 50%)
par = 0.5 * rady(jday,:,:) ;(MJ m^-2 day^-1)


;Calculates fAPAR using Monski and Saeki's approach (Oliveira 2012 Eq. 10)
fapar = par ;Preallocating
fapar = 1 - exp(-1 * bfaclai * laid)


;Temperature stress factor
ftemp = ksd ;Preallocating it
ftemp = 1
if typeftemp.eq.0 then
;Oliveira 2012 Eq. 17, FOR PASTURE!!!)
ftemp = where(tempy(jday,:,:).lt.10. .or. tempy(jday,:,:).gt.50. ,0,ftemp)
ftemp = where(tempy(jday,:,:).ge.10. .and. tempy(jday,:,:).lt.30. , 0.05*tempy(jday,:,:) - 0.5 , ftemp)
ftemp = where(tempy(jday,:,:).ge.30. .and. tempy(jday,:,:).le.35. ,1, ftemp)
ftemp = where(tempy(jday,:,:).gt.35. .and. tempy(jday,:,:).le.50. , -0.0667*tempy(jday,:,:) + 3.333 , ftemp)
else
;Romera et. al. 2007, modified to match Fig.1
ftemp =  0.09*tempy(jday,:,:) - 0.0022*tempy(jday,:,:)^2

end if


;Warns the user if something weird happens and it exceeds bounds
if (any(ftemp.gt.1 .or. ftemp.lt.0)) then
;	print("WARNING: ftemp exceeds bounds, constraining...")
	ftemp = where(ftemp.lt.0,0,ftemp)
	ftemp = where(ftemp.gt.1,1,ftemp)
end if
;Save to an output variable
ktd = ftemp

;Dry matter gain for that day (dDM/dt gross, senescence is accounted below) (modificado de Oliveira, 2012 Eq. 16)
ddmd = par * lue * fapar * ftemp * ksd
;print((/"par","lue","fapar","ftemp","ksd"/))
;printVarSummary(fapar)
;print((/par(0,0),lue,fapar(0,0),ftemp(0,0),ksd(0,0)/))


;Sets up matrices for the allocation coefficients TODO: Make them more dynamic inside the phases
fleafmat = ddmd
fleafmat = 0
fgrainmat = fleafmat
fstemmat = fleafmat

;GDD to maturity
gddmaturity = grnfillifrac*harvgdd

;fleaf goes linearly from fleafs(0) to fleafs(1) when gddmaturity is reached, and stays constant after
;fleafmat = where(gdd.le.gddmaturity,fleafs(0) + gdd*((fleafs(1)-fleafs(0))/(gddmaturity-0.0)) ,fleafs(1))

;fleaf goes linearly from fleafs(0) to fleafs(1) when harvgdd is reached
fleafmat = where(gdd.le.gddmaturity,fleafs(0),where(gdd.le.harvgdd,fleafs(0) + (gdd-gddmaturity)*((fleafs(1)-fleafs(0))/(harvgdd-gddmaturity)) ,fleafs(1)))

;fstem goes linearly from fstems(0) to fstems(1) when harvgdd is reached
fstemmat = where(gdd.le.gddmaturity,fstems(0),where(gdd.le.harvgdd,fstems(0) + (gdd-gddmaturity)*((fstems(1)-fstems(0))/(harvgdd-gddmaturity)) ,fstems(1)))


;Stores allocation factors to daily outputs
fleafy(jday,:,:) = (/fleafmat/)
fstemy(jday,:,:) = (/fstemmat/)

;if jday.ge.25 .and. jday.lt.110 then
;print((/jday,fleafmat(0,0)/))
;end if

;Before grain fill initiation
;fleafmat  = fleaf
fgrainmat = fgrain

;Allocates DM to the pools and applies senescence
ldmd = ldmd + ddmd*fleafmat - ldmd/tauleaf
sdmd = sdmd + ddmd*fstemmat - sdmd/taustem
gdmd = gdmd + ddmd*fgrainmat

;Calculates that day's LAI based on leaf DM
laid = ldmd * specla

;if (jday.ge.140 .and. jday.le.141) then
;print("ldmd (" + jday + ") = " + ldmd(0,0))
;print("ddmd (" + jday + ") = " + ddmd(0,0))
;print("par (" + jday + ") = " + par(0,0))
;
;end if

;print((/ldmd(0,0)/))


;Zeroes all allocation and pools if the pixel is not planted
ldmd = where(plantdead,0.0,ldmd)
gdmd = where(plantdead,0.0,gdmd)
ddmd = where(plantdead,0.0,ddmd)

;print((/ddmd(0,0)/))

;dumy(jday,:,:) = ldmd ;APAGAR

end

