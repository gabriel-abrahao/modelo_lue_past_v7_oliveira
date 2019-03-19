
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

arqref = addfile("ref_surta.nc","r")
varref = arqref->surta

arq1 = addfile("../auxdata/awc.nc","r")
var1 = arq1->awc

;arq1 = addfile("../auxdata/soita.sand.nc","r")
;var1 = arq1->sandpct

;arq2 = addfile("../auxdata/soita.clay.nc","r")
;var2 = arq2->claypct

surta = varref
surta = 1


;do lay = 0,dimsizes(var1&layer)-1

surta = where(.not.ismissing(var1(:,:)) .and. (var1(:,:).le.9999 .and. var1(:,:).gt.0) .and. surta(0,0,:,:).eq.1,1,0)

;surta = where(.not.ismissing(var1(0,lay,:,:)) .and. (var1(0,lay,:,:).le.100 .and. var1(0,lay,:,:).ge.0) .and. surta(0,0,:,:).eq.1,1,0)
;surta = where(.not.ismissing(var2(0,lay,:,:)) .and. (var2(0,lay,:,:).le.100 .and. var2(0,lay,:,:).ge.0) .and. surta(0,0,:,:).eq.1,1,0)


;end do

system("rm surta.nc")
arqout = addfile("surta.nc","c")
arqout->surta=surta

end
