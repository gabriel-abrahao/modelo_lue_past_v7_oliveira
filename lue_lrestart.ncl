load "conf/lue_infile.ncl"
procedure lue_lrestart(surta,depl)
local arqy
begin

arqy = addfile(restartfolder + "restart-" + restartyear + ".nc", "r")
;Root zone depletion
depl = (/arqy->depl({domain(1):domain(0)},{domain(2):domain(3)})/)
depl = mask(depl,surta,1)
delete(arqy)

end
