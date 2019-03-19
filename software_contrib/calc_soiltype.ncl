external EX01 "./funcoes.so"
;Generates a soil type map based on the USDA triangle and clay and sand maps
begin

fnamec		=	"input/clay.nc"
cvarname	=	"claypct"
fnames		=	"input/sand.nc"
svarname	=	"sandpct"

foutname	=	"soil_type.nc"


cfile = addfile(fnamec,"r")
dumclay = cfile->$cvarname$(0,:,:,:)
dumclay = where(dumclay.lt.0 .or. dumclay.gt.100, dumclay@_FillValue,dumclay)
clay = tointeger(dumclay)

sfile = addfile(fnames,"r")
dumsand = sfile->$svarname$(0,:,:,:)
dumsand = where(dumsand.lt.0 .or. dumsand.gt.100, dumsand@_FillValue,dumsand)
sand = tointeger(dumsand)

dims = dimsizes(clay)

;Preallocating
stype = dumsand
stype = stype@_FillValue
stype@type01  = "sand"
stype@type02  = "loamy sand"
stype@type03  = "sandy loam"
stype@type04  = "loam"
stype@type05  = "silt loam (combined with silt)"
stype@type06  = "sandy clay loam"
stype@type07  = "clay loam"
stype@type08  = "silty clay loam"
stype@type09  = "sandy clay"
stype@type10 = "silty clay "
stype@type11 = "clay"

printVarSummary(clay)
printVarSummary(stype)


do k = 0,dims(0)-1
print("Layer " + (k+1) + " of " + dims(0))
	do i = 0,dims(1)-1
		do j = 0,dims(2)-1
			if (.not.ismissing(clay(k,i,j)) .and. .not.ismissing(sand(k,i,j))) then
				stype(k,i,j) = EX01::textcls(sand(k,i,j),clay(k,i,j))
			end if
		end do
	end do
end do

;Metadata
;1  = sand
;2  = loamy sand
;3  = sandy loam
;4  = loam
;5  = silt loam (combined with silt)
;6  = sandy clay loam
;7  = clay loam
;8  = silty clay loam
;9  = sandy clay
;10 = silty clay 
;11 = clay




system("rm " + foutname)
arqout = addfile(foutname,"c")
arqout->stype = stype

;c = EX01::textcls(fs,fc)


end
