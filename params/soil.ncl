begin

nlayroot	=	3 ;Number of soil layers to average in calculating soil characteristics

ireadawc	=	1 ;0 - Read texture data and apply the soil triangle procedure, 1 - Read AWC (Available Water Content, mm) directly from awc.nc

;The USDA triangle, data from (?)
snums		=	(/ 01.0 , 02.0 , 03.0 , 04.0 , 05.0 , 06.0 , 07.0 , 08.0 , 09.0 , 10.0 , 11.0 /)
fcs		=	(/ 0.091, 0.125, 0.207, 0.270, 0.330, 0.255, 0.318, 0.366, 0.339, 0.387, 0.396/)
wps		=	(/ 0.033, 0.055, 0.095, 0.117, 0.133, 0.148, 0.197, 0.208, 0.239, 0.250, 0.272/)

; 01 = Sand
; 02 = Loamy Sand
; 03 = Sandy Loam
; 04 = Loam
; 05 = Silty Loam
; 06 = Sandy Clay Loam
; 07 = Clay Loam
; 08 = Silty Clay Loam
; 09 = Sandy Clay
; 10 = Silty Clay
; 11 = Clay




;The values here are temporary, constant values for all the domain. rdsoi should be improved with at least an USDA soil triangle procedure

;InLand's Clay, yields about 60mm of total available water with root depth = 0.5m
consfc		=	0.396	;Field capacity (volume fraction)
conswp		=	0.272	;Wilting point (volume fraction)

;Attempt at organic soil
;consfc		=	0.6	;Field capacity (volume fraction)
;conswp		=	0.2	;Wilting point (volume fraction)

end
