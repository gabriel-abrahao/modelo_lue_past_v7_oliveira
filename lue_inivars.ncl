load "conf/lue_infile.ncl"

begin
;This is run before everything in the script, and preallocates the spatial vars using cld of the first year of the run as a reference

dumarq = addfile(inputfolder + "cld.daily." + syear + ".nc","r")
refvar = dumarq->cld(:,{domain(1):domain(0)},{domain(2):domain(3)})
delete(dumarq)


end
