load "params/soil.ncl"
load "params/crop.ncl"

load "conf/lue_infile.ncl"
;This procedure will use sand and clay fractions and build FC and WP maps from them. However, at the moment, both are set to constant values

;Also, it calculates TAW and RAW (total and readily available water) contents (FAO 56)

procedure lue_soichar(input,taw,raw)
local fccube,wpcube
begin

if ireadawc.eq.1 then
	awcin = input
	;Calculates Total Available Water (mm) based on AWC for 1m reference depth map
	taw = awcin * rootdepth 

else

	stype = input

	;Preallocating FC an WP
	fccube = input
	wpcube = fccube
	
	;TEMPORARY: This allocates the same values of FC and WP to all layers and all pixels
	;fccube and wpcube are 3D, while fcmap and wpmap are their averages over the layers considered in nlayroot
	fccube = (/consfc/)
	wpcube = (/conswp/)
	
	nstypes = dimsizes(snums) ;Number of soil types on parameter file
	do i = 0,nstypes-1
		fccube = where(stype.eq.snums(i),fcs(i),fccube)
		wpcube = where(stype.eq.snums(i),wps(i),wpcube)
	end do
	
	
	
	;Averages them over the layers
	fcmap = dim_avg_n(fccube(0:nlayroot-1,:,:),0)
	wpmap = dim_avg_n(wpcube(0:nlayroot-1,:,:),0)
	
	;Calculates Total Available Water (mm) (FAO 56 Eq. 82)
	taw = 1000 * (fcmap - wpmap) * rootdepth 

end if

;Calculates Readily Available Water (mm) (FAO 56 Eq. 82)
raw = pfac5 * taw
;Now recalculated every day in lue_etc, here just as a starting value



end
