load "conf/lue_infile.ncl"
procedure lue_wcrop(thisyear,cropyld,harvday)
local fname
begin

fname = outputfolder + "crop-yearly-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->cropyld = cropyld
dumarq->harvday = harvday
delete(dumarq)


end
