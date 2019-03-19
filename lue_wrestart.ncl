load "conf/lue_infile.ncl"
procedure lue_wrestart(thisyear,surta,depl)
local fname
begin

fname = restartfolder + "restart-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
copy_VarCoords(surta,depl)
dumarq->depl = depl
delete(dumarq)

end
