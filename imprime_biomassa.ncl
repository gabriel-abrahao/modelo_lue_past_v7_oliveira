
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "~/lib_abrahao.ncl"

begin
pdfout = "biomassa"

year = 2014

infile = "output/bio-daily-" + year + ".nc"
varname = "abv"

xobs = (/7.0,14.0,31.0,38.0,46.0,55.0,59.0,66.0,81.0/) + 26
yobs = (/0.02,0.05,0.19,0.35,0.50,0.72,0.75,0.77,0.83/)*1000

laiobs = (/0.4,0.9,2.2,3.2,3.6,4.2,4.4,4.4,4.5/)

lat = -20.5
lon = -43.56059

xmin = 1
xmax = 365

nplots = 6

arq = addfile(infile,"r")
abv = arq->$varname$(:,{lat},{lon})
leaf = arq->ldm(:,{lat},{lon})
stem = arq->sdm(:,{lat},{lon})
arqlai = addfile("output/lai-daily-" + year + ".nc","r")
laivar = arqlai->lai(:,{lat},{lon})


var = (/abv,leaf,stem/)

time = abv&time

pstr = "x,biosim,laisim,bioobs,laiobs"
system("echo " + pstr + ">saida.csv")

do x = xmin,xmax
pstr = "" + x + "," + abv(x) + "," + laivar(x) 

if any(xobs.eq.x) then
	pstr = pstr + "," + yobs(ind(where(x.eq.xobs,True,False))) + "," + laiobs(ind(where(x.eq.xobs,True,False)))
else
	;pstr = pstr + "," + "-9999.0" + "," + "-9999.0"
	pstr = pstr + ",,"
end if
print("" + pstr)
system("echo " + pstr + ">>saida.csv")

end do


wkstype = "pdf"
;wkstype@wkPaperHeightF = 29.7/2.54
;wkstype@wkPaperWidthF = 23.0/2.54
;wkstype@wkPaperHeightF = 59.7/2.54
wks = gsn_open_wks(wkstype,pdfout)

res = True
res@trXMinF     =       xmin
res@trXMaxF     =       xmax

res@trYMinF     =       0.0
res@trYMaxF     =       max((/max(yobs),max(var(:,xmin:xmax))/)) +50

res@vpWidthF	=	2.5
res@vpHeightF	=	1.0

res@xyLineColors=	(/"blue","green","brown"/)
res@xyDashPattern = 0

res@tiYAxisString = "Biomass (gDM/m2)"
;res@tiXAxisString = "DOY"
res@tiXAxisString = ""

;res@pmLegendDisplayMode = "Always"
;res@pmLegendWidthF = 0.1
;res@pmLegendHeightF = 0.2
;res@lgPerimOn = False
;res@pmLegendSide = "Top"
;res@pmLegendOrthogonalPosF = 0.5
;res@pmLegendParallelPosF = 0.5


res@xyExplicitLegendLabels = (/"AbvGrnd","Leaf","Stem"/)

res@gsnFrame	=	False
res@gsnDraw	=	False


plot = new(nplots,graphic)

plot(0) = gsn_csm_xy(wks,time,var,res)

res2 = res
res2@xyMarkLineMode	=	"Markers"
res2@xyMarkerColor	=	"red"


plot2 = gsn_csm_xy(wks,xobs,yobs,res2)

overlay(plot(0),plot2)

legend = create "Legend" legendClass wks
    "vpXF" : 0.37
    "vpYF" : 0.99
    "vpWidthF" : 0.05
    "vpHeightF" : 0.03

    "lgPerimOn" : False
    "lgAutoManage" : False
    "lgOrientation" : "Vertical"
    "lgItemType" : "Lines"
    "lgItemCount" : 3
    "lgLabelStrings" : (/"AbvGrnd","Leaf","Stem"/)
    "lgLabelsOn" : True
    "lgLabelFontHeightF" : 0.005
    "lgLineColors" : (/"blue","green","brown"/)
    "lgMonoDashIndex" : 0
    "lgMonoLineLabelFontColor" : True ; Only one label color - black
end create



;LAI plot


sla = 1e3*laivar/where(abv.ne.0,abv,abv@_FillValue)

reslai = res
delete(reslai@xyLineColors)
reslai@trXMinF     =       xmin
reslai@trXMaxF     =       xmax
reslai@gsnDraw	=	False
reslai@gsnFrame	=	False
reslai@trYMaxF     =       0.0
;reslai@trYMaxF     =       max(laivar(xmin:xmax)) +0.5
reslai@trYMaxF     =     6.0 
reslai@xyLineColors =	(/"green"/)
reslai@tiYAxisString = "LAI"
ressla = reslai
ressla@trYMinF     =       min(sla(xmin:xmax)) -1.0
ressla@trYMaxF     =       max(sla(xmin:xmax)) +1.0
ressla@xyLineColors =	(/"red"/)
ressla@tiYAxisString = "SpecLA(m2 kgDM) "

plot(1) = gsn_csm_xy2(wks,time,laivar,sla,reslai,ressla)


delete(res2)
res2 = reslai
res2@xyMarkLineMode	=	"Markers"
res2@xyMarkerColor	=	"red"

laiplot2 = gsn_csm_xy(wks,xobs,laiobs,res2)

overlay(plot(1),laiplot2)

;Allocation factors
fleaf = arq->fleaf(:,{lat},{lon})
fstem = arq->fstem(:,{lat},{lon})
alovar = (/fleaf,fstem/)

resalo = reslai
delete(resalo@xyLineColors)
resalo@trYMinF     =      0 
resalo@trYMaxF     =      1
resalo@xyLineColors =	(/"green","brown"/)

resalo@tiYAxisString = "Allocation Factors (G=Leaf,B=Stem)"



;Stress factors plot
arqstr = addfile("output/str-daily-" + year + ".nc","r")
ksvar = arqstr->ks(:,{lat},{lon})
ktvar = arqstr->kt(:,{lat},{lon})
strvar = (/ksvar,ktvar/)

resstr = reslai
delete(resstr@xyLineColors)
resstr@trYMinF     =      0 
resstr@trYMaxF     =      1
resstr@xyLineColors =	(/"blue","red"/)

resstr@tiYAxisString = "Stress Factors (B=Water,R=Temp)"

;plot(2) = gsn_csm_xy(wks,time,strvar,resstr)
plot(2) = gsn_csm_xy2(wks,time,strvar,alovar,resstr,resalo)

;Climate plots
arqcld = addfile("input/cld.daily." + year + ".nc","r")
cld = arqcld->cld(:,{lat},{lon})
arqtemp = addfile("input/temp.daily." + year + ".nc","r")
temp = arqtemp->temp(:,{lat},{lon})
arqprec = addfile("input/prec.daily." + year + ".nc","r")
prec = arqprec->prec(:,{lat},{lon})

delete(reslai@xyLineColors)
rescld = reslai
rescld@trYMinF     =      -10.0
rescld@trYMaxF     =      110.0
rescld@xyLineColor =   "cyan"
;rescld@tmYLLabelsOn= False
;rescld@tmYRLabelsOn= False
;rescld@tmYROn	= False
;rescld@tmYLOn	= False

resprec = reslai
resprec@trYMinF     =      0.0
resprec@trYMaxF     =      max(prec)
resprec@xyLineColor =   "blue"
resprec@tiYAxisString = "Prec(mm/day)"

restemp = reslai
restemp@trYMinF     =      min(temp)
restemp@trYMaxF     =      max(temp)
restemp@xyLineColor =   "red"
restemp@tiYAxisString = "Temp(degC)"

;plot(3) = gsn_csm_xy3(wks,time,temp,prec,cld,restemp,resprec,rescld)

plot(3) = gsn_csm_xy2(wks,time,temp,prec,restemp,resprec)

plot3 = gsn_csm_xy(wks,time,cld,rescld)

;overlay(plot(3),plot3)

;GDD plot
arqgdd = addfile("output/gdd-daily-" + year + ".nc","r")
gddvar = arqgdd->gdd(:,{lat},{lon})

resgdd = reslai
delete(resgdd@trYMinF)
delete(resgdd@trYMaxF)
delete(resgdd@xyLineColors)
resgdd@xyLineColor = "red"
resgdd@tiYAxisString = "GDD (degC-days)"


plot(4) = gsn_csm_xy(wks,time,gddvar,resgdd)







;Generic comparison plot

comp1 = yobs
comp2 = abv(toint(xobs))


;comp1 = temp(toint(xobs))
;comp2 = abv(toint(xobs)) - yobs

rescomp = True
rescomp@gsnDraw = False
rescomp@gsnFrame = False
delete(rescomp@xyLineColors)
rescomp@vpHeightF = 1.0
rescomp@vpWidthF  = 1.0
;rescomp@gsnMaximize = True

rescomp@trYMinF     =      0.0
rescomp@trYMaxF     =      1000.0
rescomp@trXMinF     =      0.0
rescomp@trXMaxF     =      1000.0 


rescomp@xyMarkLineMode	=	"Markers"


plot(5) = gsn_xy(wks,comp1,comp2,rescomp)


;regstats = simple_reg(comp1,comp2,95.0)
;;print(regstats)
;
;
;resline = rescomp
;resline@xyMarkLineMode	=	"Lines"
;resline@xyLineColor	=	"red"
;
;resline@tiMainString	=	"slp: " + regstats + " int: " + regstats@intercept + " (R2 = " + regstats@rsquare + ")"
;
;
;xline = (/-10000.0,10000.0/)
;yline = (/-10000.0,10000.0/)
;plotline = gsn_xy(wks,xline,yline,resline)
;
;overlay(plot(5),plotline)



resP = True
resP@gsnMaximize = True
;resP@vpHeightF = 1.0
;resP@vpWidthF = 1.0
resP@gsnFrame = False
resP@gsnPaperMargin = 0.1

gsn_panel(wks,plot,(/nplots,1/),resP)


draw(legend)
frame(wks)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Second page
;plotsec = new(4,graphic)
;
;
;
;
;resPsec = resP
;gsn_panel(wks,plotsec,(/4,1/),resPsec)

end

