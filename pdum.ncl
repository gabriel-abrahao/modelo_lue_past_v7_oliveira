procedure pdum(var)
local dumarq
begin
system("rm dummy.nc")
dumarq = addfile("dummy.nc","c")
dumarq->dumvar = var
delete(dumarq)

end
