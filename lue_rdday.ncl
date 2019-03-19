procedure lue_rdday(domain,thisyear,surta,cldy,precy,tempy)
local arqy
begin

;Cloud
arqy = addfile(inputfolder + "cld.daily." + thisyear + ".nc", "r")
cldy = (/arqy->cld(:,{domain(1):domain(0)},{domain(2):domain(3)})/)
cldy = mask(cldy,surta,1)
delete(arqy)

;Precipitation
arqy = addfile(inputfolder + "prec.daily." + thisyear + ".nc", "r")
precy = (/arqy->prec(:,{domain(1):domain(0)},{domain(2):domain(3)})/)
precy = mask(precy,surta,1)
delete(arqy)

;Temperature
arqy = addfile(inputfolder + "temp.daily." + thisyear + ".nc", "r")
tempy = (/arqy->temp(:,{domain(1):domain(0)},{domain(2):domain(3)})/)
tempy = mask(tempy,surta,1)
delete(arqy)
end
