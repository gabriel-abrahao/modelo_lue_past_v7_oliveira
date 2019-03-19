load "conf/lue_infile.ncl"
load "params/crop.ncl"
procedure lue_cropmeta(refvar,varcrop)
local fname
begin

varcrop!0 = "harvest"
;varcrop!0 = "time"
varcrop!1 = "latitude"
varcrop!2 = "longitude"

;varcrop&time = fspan(0,0,nmaxharv)
;varcrop&time = varcrop@_FillValue
varcrop&harvest = fspan(1,nmaxharv,nmaxharv)
varcrop&latitude = refvar&latitude
varcrop&longitude = refvar&longitude

;varcrop@units = "tDM/ha"
end
