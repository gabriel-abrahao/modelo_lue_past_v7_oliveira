begin

rootdepth = 0.9 ;Depth of the root zone (m) (Gabriel Abrahao: 0.4 is an average value for soybeans and should be revised) TODO:Dynamic root growth
;rootdepth = 1.0 ;Depth of the root zone (m) (Gabriel Abrahao: 1.0 is an average value for extensive pasture (FAO56 Table 22))

idynapfac = 0 ; 0 Fixed pfac, 1: Corrected with pfac = pfac5 + 0.04(5-etc) (FAO 56) (Gabriel Abrahao: 0 appears to yield better values)
pfac5 = 0.6 ;Soil water depletion factor for no stress at etc=5mm. TODO: Could be corrected with pfac = pfac5 + 0.04(5-etc) (FAO 56)

baset = 18.0 ;Base temperature for GDD calculation (oC)
maxtresh  = 30.0 ;Maximum temperature that will accumulate GDD (oC)

;pdate1 = 26  ;Planting date for the first harvest. The next ones will be immediately after harvest

pdate1 = 26   

nmaxharv = 36  ;Maximum number of harvests allowed before waiting for pdate1
harvgdd = 385.0 ;GDD to harvest


;harvdpp = 20.0 ;Number of days to harvest

harvdpp = 365    


iharv = True ;True to zero pools after harvest
harvmode = 1 ;0: harvest when GDD reaches harvgdd, 1: harvest when DPP reaches harvdpp

;Leaf residence time (days)
tauleaf = 40.0    ;Marandu: ~40 days in Casagrande et. al. 2010

;Stem residence time (days)
taustem = 22.0    ;Marandu:

;The phenology for carbon allocation is divided in 2 phases, before and after maturity
grnfillifrac = 0.00  ;Fraction of harvgdd to trigger maturity phase

;Carbon allocation factors
fleafs  =(/ 0.9 , 0.15 /)
fstems  =(/ 0.0 , 0.6 /) 


fgrain = 0.0


;

;Initial LAI
laiinit = 0.01

;LAI light extinction coefficient
bfaclai  =  0.5

;Specific leaf area (m^2leaf gDM^-1)
specla  = 11.0e-3
;specla  = 10.0e-3

;Light Use Efficiency (LUE) ( gDM MJ^-1 )
lue = 6.6

;Type of ftemp calculation. 0: Oliveira et. al. 2012 1: Romera et. al. 2009
typeftemp = 1

;The phenology for ET is divided in the 4 phases of evapotranspiration described in FAO 56 and FAO 24:
; Initial	(0)	:	from germination to 10% ground cover
; Development	(1)	: 	from 10% GC to 70-80% GC (Full effective ground cover)
; Mid-season 	(2)	:	from full GC to maturity, depending on the crop
; Late-season	(3)	:	from end of maturity to harvest
;
;The crop coefficients used are constant for each phase based on Table 12 of FAO 56
;For phase 0, kc is kcini
;For phase 1, where kc should be rising from kcini to kcmid, it is the average of the two
;For phase 2, kc is kcmid
;For phase 3, where it should be falling from kcmid to kcend, it is the average of the two


;			     1        2        3
etgddfracs	=	(/ 0.15  ,  0.35  ,  0.99  /)	;Fraction of harvgdd to trigger phase WARNING do not set to 1

;kcend		=	(/ 0.50  ,  1.15  ,  0.50  /) (Gabriel Abrahao: Soybeans as in FAO 56)
;kcend		=	(/ 0.50  ,  1.00  ,  0.75  /)
kcend		=	(/ 0.40  ,  1.05  ,  0.85  /) ;(Gabriel Abrahao: Grazing pasture: rotated as in FAO 56)

end
