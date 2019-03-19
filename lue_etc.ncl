load "params/crop.ncl"


procedure lue_etc(et0,gdd,plantdead,taw,raw,depl,etcd,ksd)
local kc
begin

;Preallocates kc
kc = et0

;Initial phase
kc = where(gdd.lt.harvgdd*etgddfracs(0), kcend(0) ,kc)
;Development phase
kc = where(gdd.ge.harvgdd*etgddfracs(0) .and. gdd.lt.harvgdd*etgddfracs(1), kcend(0) + (gdd - harvgdd*etgddfracs(0)) * ( (kcend(1)-kcend(0)) / (harvgdd*etgddfracs(1) - harvgdd*etgddfracs(0)) ) ,kc)
;Mid-season phase
kc = where(gdd.ge.harvgdd*etgddfracs(1) .and. gdd.lt.harvgdd*etgddfracs(2), kcend(1) ,kc)
;Late-season phase
kc = where(gdd.ge.harvgdd*etgddfracs(2) .and. gdd.le.harvgdd, kcend(1) + (gdd - harvgdd*etgddfracs(2)) * ( (kcend(2)-kcend(1)) / (harvgdd - harvgdd*etgddfracs(2)) ) ,kc)

;Setting to a constant soil evaporation "kc" after harvest
kc = where(gdd.gt.harvgdd .or. plantdead, 1.05 ,kc)



;Potential evapotranspiration
etp = et0*kc

;Calculates Readily Available Water (mm) (FAO 56 Eq. 82)
if idynapfac.eq.1 then 
	pfac = pfac5 + 0.04*(5.0-etcd) ;Based on last day's ETC
else
	pfac = pfac5
end if

raw = pfac * taw



;Water stress coefficient (FAO 56 Eq. 84)
ksd = where(depl.gt.raw,(taw - depl) / (taw -raw),1)


;Atcual crop evapotranspiration
etcd = etp * ksd

end
