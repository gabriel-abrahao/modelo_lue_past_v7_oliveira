load "conf/lue_infile.ncl"
procedure lue_wdaily(thisyear,etcy,ksy,kty,swcy,ddmy,laiy,ldmy,sdmy,abvy,fleafy,fstemy,gddy)
local fname
begin

fname = outputfolder + "etc-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->etc = etcy
delete(dumarq)

fname = outputfolder + "str-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->ks = ksy
dumarq->kt = kty
delete(dumarq)

fname = outputfolder + "swc-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->swc = swcy
delete(dumarq)

fname = outputfolder + "lai-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->lai = laiy
delete(dumarq)

fname = outputfolder + "bio-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->ldm = ldmy
dumarq->sdm = sdmy
abvy = ldmy + sdmy
dumarq->abv = abvy
dumarq->ddm = ddmy
dumarq->fleaf = fleafy
dumarq->fstem = fstemy
delete(dumarq)

fname = outputfolder + "gdd-daily-" + thisyear + ".nc"
system("[ -f  '" + fname + "' ] && rm " + fname)
dumarq = addfile(fname,"c")
dumarq->gdd = gddy
delete(dumarq)
end
