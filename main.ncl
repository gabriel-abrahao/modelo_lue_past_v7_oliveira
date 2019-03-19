load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "auxdata/lib_abrahao.ncl"

load "pdum.ncl"
load "conf/lue_infile.ncl"

;load "params/weather.ncl"
load "params/crop.ncl"

;load "lue_inivars.ncl"
load "lue_wrestart.ncl"
load "lue_lrestart.ncl"
load "lue_soichar.ncl"
load "lue_cropmeta.ncl"
load "lue_rdday.ncl"
load "lue_weather.ncl"
load "lue_et0.ncl"
load "lue_etc.ncl"
load "lue_soiwbal.ncl"
load "lue_dm.ncl"


load "lue_wdaily.ncl"
load "lue_wcrop.ncl"

begin

;Loads surta.nc, that contains a 4D mask of 1's that will mask everything after turned to 2D
dumarq = addfile(auxdatafolder + "surta.nc","r")
surta = dumarq->surta(0,0,{domain(1):domain(0)},{domain(2):domain(3)})
delete(dumarq)

;Gets nlat and nlon for the domain
domsizes = dimsizes(surta)
nlat = domsizes(0)
nlon = domsizes(1)

;Reading topography (lat, lon) and building surface pressure map
dumarq = addfile(auxdatafolder + "topo.nc", "r")
dumvar = dumarq->topo(0,0,{domain(1):domain(0)},{domain(2):domain(3)})
topomap = tofloat(mask(dumvar,surta,1))
copy_VarMeta(dumvar,topomap)
delete(dumarq)
delete(dumvar)
spressure = 101.3 * ( (293.0 - 0.0065 * topomap) / (293.0) ) ^ 5.26 ;kPa, FAO 56 Eq. 7
copy_VarMeta(topomap,spressure)
printVarSummary(spressure)

;Reading soil types (layer, lat, lon)
dumarq = addfile(auxdatafolder + "soil_type.nc", "r")
dumvar = dumarq->stype(:,{domain(1):domain(0)},{domain(2):domain(3)})
stype = tofloat(mask(dumvar,surta,1))
copy_VarMeta(dumvar,stype)
delete(dumvar)
delete(dumarq)


;Preallocates TAW and RAW for soilchar
taw = (/stype(0,:,:)/)
raw = (/stype(0,:,:)/)
copy_VarCoords(stype(0,:,:),taw)
copy_VarCoords(stype(0,:,:),raw)


;If reading awc directly, also preallocate it, but let lue_soichar do the reading
if ireadawc.eq.1 then
	dumarq = addfile(auxdatafolder + "awc.nc", "r")
	dumvar = dumarq->awc({domain(1):domain(0)},{domain(2):domain(3)})
	awcin = tofloat(mask(dumvar,surta,1))
	copy_VarMeta(dumvar,awcin)
	delete(dumvar)
	delete(dumarq)
end if

;Computes TAW and RAW maps from soil characteristics or from the awc map
if ireadawc.eq.1 then
	lue_soichar(awcin,taw,raw)
else
	lue_soichar(stype,taw,raw)
end if


;;;;;;;;;;;;;;;;;;;;;;;;;;; START OF THE YEARLY LOOP ;;;;;;;;;;;;;;;;;;;;;;;;
do thisyear = syear,syear + nrun - 1
	print((/"Running year: " + thisyear/))


	;Initializes a reference variable for that year, using cld file of that year
	dumarq = addfile(inputfolder + "cld.daily." + thisyear + ".nc","r")
	refvar = dumarq->cld(:,{domain(1):domain(0)},{domain(2):domain(3)})
	refvar@_FillValue = default_fillvalue(typeof(refvar))
	delete(refvar@units)
	delete(refvar@long_name)
	refvar = refvar@_FillValue
	delete(dumarq)
	
	;Gets the number of days in that year
	ndaysiny = dimsizes(refvar&time)


	;Initializes variables for daily output
	etcy = refvar ;Crop ET
	ksy = refvar  ;Soil water stress factor
	kty = refvar  ;Temperature stress factor
	swcy = refvar ;TODO: Erase this, doesn't make sense
	ddmy = refvar ;Dry matter gain
	laiy = refvar ;LAI

	ldmy = refvar ;Dry matter leaf
	sdmy = refvar ;Dry matter stem
	abvy = refvar ;Aboveground dry matter

	fleafy = refvar ;Leaf allocation factor
	fstemy = refvar ;Stem allocation factor

	gddy = refvar ;GDD accumulated

dumy = refvar ;Just a dummy variable for checking others

	;Initializes the crop yields variable
	cropyld = new((/nmaxharv,nlat,nlon/),float)
	lue_cropmeta(refvar,cropyld)
	cropyld@units = "tDM/ha"
	harvday = new((/nmaxharv,nlat,nlon/),integer)
	lue_cropmeta(refvar,harvday)
	harvday@units = "DOY"

	cropyld = cropyld@_FillValue ;Fill it with missing values, so that statistics can be more easily performed
	harvday = harvday@_FillValue ;Fill it with missing values, so that statistics can be more easily performed

	;Initializes the climate variables for rdday
	cldy = refvar ;fraction
	precy = refvar ;mm/day
	tempy = refvar ;degC


	;Reads in the years of daily data	
	lue_rdday(domain,thisyear,surta,cldy,precy,tempy)

	;Initializes rady for the angstrom procedure in weather
	rady = refvar ;W m^-2

	;Runs the weather procedure, that converts cloud cover in fraction to downward shortwave radiation in MJ m^-2 day^-1
	lue_weather(cldy,rady)



;APAGAR
system("rm myarq.nc")
myarq = addfile("myarq.nc","c")
myarq->rady=rady

	;Initialize some variables if this is the first year, or clean them if this is not
	;Variables starting in "lst" refer to the last value of the last year (or the initialization value if this is the first year)
	if (thisyear.eq.syear) then
		;Evapotranspiration
		et0d = new((/nlat,nlon/),float)
		etcd = new((/nlat,nlon/),float)
		etcd = 0.0 ;Start with 0mm
		ksd = new((/nlat,nlon/),float) ;Water stress coefficient
		ktd = new((/nlat,nlon/),float) ;Temperature coefficient

		;Soil water
		depl = new((/nlat,nlon/),float) ;Root zone depletion (mm)
		depl = 0
		swcd = new((/nlat,nlon/),float) ;Soil water content (volume fraction)
		swcd = 1.
	
		;Dry matter and plant-related variables
		laid = new((/nlat,nlon/),float) ;LAI
		laid = laiinit ;Initiliazes with the parameter
		ldmd = laid / specla  ;Leaf DM pool
		sdmd = ldmd  ;Stem DM pool
		sdmd = 0
		gdmd = ldmd  ;Grain DM pool
		gdmd = 0
		ddmd = gdmd  ;dDM/dt, also zero

		;Degree days
		gdd = new((/nlat,nlon/),float) ;the accumulated gdd for each cell
		gdd = 0
		deltagdd = new((/nlat,nlon/),float) ;the gdd respective to that day
	
		nharv = new((/nlat,nlon/),integer) ;Number of consecutive harvests
		nharv = nmaxharv ;Starts with maximum harvest number, so that planting only occurs on pdate

		plantdead = nharv.ge.nmaxharv ;Logical array referencing pixels not planted

		;Days Past Planting (DPP)
		dpp = gdd ;Preallocating with GDD, that should be zero at this point



	else
		;Zeroes the number of harvests for the new year
		nharv = 0
	end if


;;;;;;;;;;;;;;;;;;;;;;;;;;; START OF THE DAILY LOOP ;;;;;;;;;;;;;;;;;;;;;;;;
	do jday = 0,ndaysiny - 1 ;JDAY here is DOY-1
;	jday = 200
;print((/jday/))

		;Plant pixels on pdate1 of the first year
;		if (jday.eq.pdate1-1 .and. thisyear.eq.syear) then
;			laid = where(plantdead,laiinit,laid) ;Planting actually means leaf emergence
;			ldmd = laid / specla  ;Leaf DM pool
;
;			nharv = where(nharv.eq.nmaxharv,0,nharv)
;			plantdead = nharv.ge.nmaxharv ;Logical array referencing pixels not planted,calculated again in the end of every day
;		end if


		;Writes restart data on the first day of every year, and reads restartyear on the first year of simulation
		if (jday.eq.0) then
			if (iloadrestart.and.thisyear.eq.syear) then
				lue_lrestart(surta,depl)
			end if
			if (iwriterestart) then
				lue_wrestart(thisyear,surta,depl)	
			end if
		end if

		;Daily reference evapotranspiration using the method chosen in lue_et0
		lue_et0(spressure,tempy(jday,:,:),rady(jday,:,:),et0d)

		;Calculate crop evapotranspiration based on crop coefficients, accounting for water stress
		lue_etc(et0d,gdd,plantdead,taw,raw,depl,etcd,ksd)	

		;Calculate soil water balance	
		lue_soiwbal(jday,precy,etcd,taw,depl,swcd)

;system("rm meudum.nc")
;arqmeu = addfile("meudum.nc","c")
;poi=laid
;copy_VarCoords(refvar(0,:,:),poi)
;printVarSummary(poi&latitude)
;printVarSummary(taw&latitude)
;arqmeu->awcin=awcin
;arqmeu->plantdead=poi
;arqmeu->taw=taw
;arqmeu->surta=surta
;delete(arqmeu)
;delete(poi)
;
;if (jday.eq.1) then
;exit
;end if	
	
		;Dry matter assimilation and allocation
		lue_dm(jday,gdd,plantdead,tempy,rady,ksd,ktd,laid,ddmd,ldmd,sdmd,gdmd,fleafy,fstemy, dumy)
;print((/ldmd(0,0)/))
		


		;Accumulate GDD TODO: Zero it in the pixels where harvest ocurred, and do this only starting on the planting date
		deltagdd = where(tempy(jday,:,:).le.maxtresh , tempy(jday,:,:) , maxtresh) - baset ;Constrain for values .le.maxt
		deltagdd = where(deltagdd.gt.0,deltagdd,0) ;Constrain for positive values
		gdd = gdd + deltagdd

		;Accumulate days past planting
		dpp = dpp + 1

		;Hold pixels that are not planted with 0 gdd and dpp until pdate TODO: Also zero NPP
		gdd = where(nharv.ge.nmaxharv,0,gdd)
		dpp = where(nharv.ge.nmaxharv,0,dpp)
		plantdead = nharv.ge.nmaxharv ;Logical array referencing pixels not planted


		if iharv then
			;Harvests harvestable pixels, and plants the ones that have been harvested less than nmaxharv times
			;This should be the ONLY time the script has to iterate through the pixels
	;print(nharv)
			convfac = 1e-2 ;gDM m^-2 to t ha^-1 TODO:This factor should include things like grain moisture
	
			;This solution is much faster, TODO: Verify this and delete he last one that iterates throught all the pixels

			if (harvmode.eq.0) then ;Harvest based on GDD
	
				if (any(gdd.ge.harvgdd)) then
					do i = 0,nmaxharv-1
						;cropyld(i,:,:) = where(gdd .ge. harvgdd , (/ldmd + sdmd/) * convfac , cropyld(i,:,:)) ;Harvest both leaf and stem
						cropyld(i,:,:) = where((gdd .ge. harvgdd) .and. (nharv.eq.i) , (/ldmd + sdmd/) * convfac , cropyld(i,:,:)) ;Harvest both leaf and stem
						harvday(i,:,:) = where((gdd .ge. harvgdd) .and. (nharv.eq.i) , toint(jday)+1 , harvday(i,:,:))
					end do
		
				end if
		
				nharv = where(gdd.ge.harvgdd, nharv+1 , nharv) ;Adds a harvest
				;TODO: here the DM pools should be added to an output
				laid = where(gdd.ge.harvgdd,laiinit,laid)
				ldmd = where(gdd.ge.harvgdd,laiinit/specla,ldmd)
				sdmd = where(gdd.ge.harvgdd,0,sdmd)
				gdmd = where(gdd.ge.harvgdd,0,gdmd)
		
				gdd = where(gdd.ge.harvgdd, 0, gdd) ;Zeroes GDD to start a new cycle
				dpp = where(gdd.ge.harvgdd, 0, dpp) ;Zeroes NPP to start a new cycle
			else if (harvmode.eq.1) then
				if (any(dpp.ge.harvdpp)) then
					do i = 0,nmaxharv-1
						cropyld(i,:,:) = where((dpp .ge. harvdpp) .and. (nharv.eq.i) , (/ldmd + sdmd/) * convfac , cropyld(i,:,:)) ;Harvest both leaf and stem
						harvday(i,:,:) = where((dpp .ge. harvdpp) .and. (nharv.eq.i) , toint(jday)+1 , harvday(i,:,:))
					end do
		
				end if
		
				nharv = where(dpp.ge.harvdpp, nharv+1 , nharv) ;Adds a harvest
				;TODO: here the DM pools should be added to an output
				laid = where(dpp.ge.harvdpp,laiinit,laid)
				ldmd = where(dpp.ge.harvdpp,laiinit/specla,ldmd)
				sdmd = where(dpp.ge.harvdpp,0,sdmd)
				gdmd = where(dpp.ge.harvdpp,0,gdmd)
		
				gdd = where(dpp.ge.harvdpp, 0, gdd) ;Zeroes GDD to start a new cycle
				dpp = where(dpp.ge.harvdpp, 0, dpp) ;Zeroes NPP to start a new cycle



			end if 
			end if	
		else
		end if ;IHARV

		;Plant pixels on pdate1 of the first year
		if (jday.eq.pdate1-1 .and. thisyear.eq.syear) then
			laid = where(plantdead,laiinit,laid) ;Planting actually means leaf emergence
			ldmd = laid / specla  ;Leaf DM pool

			nharv = where(nharv.eq.nmaxharv,0,nharv)
			plantdead = nharv.ge.nmaxharv ;Logical array referencing pixels not planted,calculated again in the end of every day
		end if



;print((/jday/))
;print((/gdd(0,0)/))
		;Writes to output variables
		etcy(jday,:,:) = (/etcd/)
		ksy(jday,:,:) = (/ksd/)
		kty(jday,:,:) = (/ktd/)
		swcy(jday,:,:) = (/swcd/)
		ddmy(jday,:,:) = (/ddmd/)
		laiy(jday,:,:) = (/laid/)
		ldmy(jday,:,:) = (/ldmd/)
		sdmy(jday,:,:) = (/sdmd/)

		gddy(jday,:,:) = (/gdd/)
	end do

;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF THE DAILY LOOP ;;;;;;;;;;;;;;;;;;;;;;;;


	;Writes daily output to files
	lue_wdaily(thisyear,etcy,ksy,kty,swcy,ddmy,laiy,ldmy,sdmy,abvy,fleafy,fstemy,gddy)

	;Writes crop output
	lue_wcrop(thisyear,cropyld,harvday)

pdum(dumy)	
	;Cleans up the daily vars
	delete(refvar)
	delete(cldy)
	delete(tempy)
	delete(precy)
	delete(rady)
	delete(etcy)
	delete(ksy)
	delete(kty)
	delete(swcy)
	delete(ddmy)
	delete(laiy)
	delete(ldmy)
	delete(sdmy)
	delete(abvy)
	delete(fleafy)
	delete(fstemy)
	delete(gddy)

delete(dumy)

end do
;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF THE YEARLY LOOP ;;;;;;;;;;;;;;;;;;;;;;;;

end

