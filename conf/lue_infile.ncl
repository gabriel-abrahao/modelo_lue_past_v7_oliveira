begin
syear		=	2014
nrun		=	3

iwriterestart	=	False
iloadrestart	=	False
restartyear	=	2016

epsilon		=	0.000000000001

;snorth ssouth swest  seast
;domain		=	tofloat((/13.1,    -55.0,   -81.0,   -35.25/)) ;SAM
;domain          =       tofloat((/ 5.0,       -35.0,    -75.0,      -35.0/)) ;BR
domain          =       tofloat((/ 6.0,       -34.0,    -74.0,      -34.0/)) ;BR2
;domain		=	tofloat((/-20.25 , -20.75 , -43.75 ,  -43.25/)) ;Vicosa 
;domain		=	tofloat((/-20.25 , -20.75 , -43.75 ,  -44.25/)) ;2 point Vicosa 
;domain		=	tofloat((/-20.25 , -21.75 , -43.75 ,  -44.25/)) ;4 point Vicosa 

inputfolder	=	"input/"
auxdatafolder	=	"auxdata/"
outputfolder	=	"output/"
restartfolder	=	"restart/"


end
