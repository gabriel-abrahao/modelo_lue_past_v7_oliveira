procedure lue_soiwbal(jday,precy,etcd,taw,depl,swcd)
begin

;Root zone depletion (FAO 56 Eq. 85)
depl = depl - precy(jday,:,:) + etcd

;Constrain it to positive values, considering negative values as runoff
depl = where(depl.lt.0,0,depl)

;Soil water content (fraction relative to saturation)
swcd = (taw - depl) / taw

end
