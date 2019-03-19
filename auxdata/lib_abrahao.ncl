
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"




; Gabriel Abrahao - gabriel.abrahao@ufv.br
; Gets all the unique values that occur in var
undef("get_unique_values")
function get_unique_values(var)
local var1d,dummy
begin
var1d = ndtooned(var)
levels = new(1,typeof(var1d))
do i=0,dimsizes(var1d)-1
        if (ismissing(levels(0)) .eq. True) then
                if (ismissing(var1d(i)) .ne. True) then
                        levels(0) = var1d(i)
                end if  
        else
		if (.not. ismissing(var1d(i))) then
		        if (any(levels.eq.var1d(i)) .or. ismissing(var1d(i)).eq.True) then
     		        else
				dummy = new(dimsizes(levels) + 1, typeof(levels))
				dummy(0:dimsizes(levels)-1) = levels
               			dummy(dimsizes(dummy)-1) = var1d(i)
				delete(levels)
				levels = dummy
				delete(dummy)
        	        end if
		end if
        end if
end do
return(levels)
end
; Copy the coordinate variables from one variable to another, except for the leftmost dimensions of var_to that are not in var_from 
; Currently only works when the rank of var_to is higher than var_from 
undef ("copy_VarCoords_exl")
procedure copy_VarCoords_exl(var_from,var_to)
local dimt, dimf, rfrom, rto, i, dName
begin
  dimf  = dimsizes(var_from)
  rfrom = dimsizes(dimf)      ; rank of var_from
  if (rfrom.le.1) then
      return
  end if
  dName = getvardims(var_from)               ; Oct 18, 2005
  if (.not.all(ismissing(dName))) then
        if (rfrom.gt.1) then
            dimt  = dimsizes(var_to)
            rto   = dimsizes(dimt)      ; rank of var_to
            do i = 0,rfrom-1            ; 
                ito = i + (rto-rfrom)   ; the difference in rank
               if (.not.ismissing(dName(i)) .and. dimf(i).eq.dimt(ito)  ) then
                    var_to!ito = var_from!i
                    if(iscoord(var_from,var_from!i))
                       var_to&$var_to!ito$ = var_from&$var_from!i$
                    end if
                end if
            end  do
        end if
  end if
end ;PROCEDURE COPY_VARCOORDS_EXL

undef("calc_stat_mode")
function calc_stat_mode(var)
local var1d,unvals,noc
begin
var1d = ndtooned(var)
unvals = get_unique_values(var1d)
noc = unvals ;Number of ocurrences of that value
noc = 0
do i = 0,dimsizes(unvals)-1
        noc(i) = sum(where(var1d.eq.unvals(i),1,0))
end do

stat_mode = unvals(ind(noc .eq. max(noc))) ;The statistical mode of the array given
return(stat_mode)
end ;FUNCTION CALC_STAT_MODE




undef("simple_reg")
function simple_reg(x,y,conflev)
local sx,sy,sxx,syy,sxy,n,b1,b0,stdres,stdb1,stdb0,rcoef,rsquare,mae,rmse,cib0up,cib0lo,cib1up,cib1lo,cib0,cib1,output
begin

;x = (/10.,20.,30.,40.,50.,60./)
;y = (/-10.5,-20.8,-31.,-39.,-52.,-58./)
;conflev = 95


;Number of observations
n = tofloat(num(.not.ismissing(y)))
if (n.eq.0) then
	n@_FillValue = default_fillvalue("float")
	n = n@_FillValue
end if

;Filter missing values postions on both variables
x = where(ismissing(y),x@_FillValue,x)
y = where(ismissing(x),y@_FillValue,y)

;Compute sums
sx = sum(x)
sxx = sum(x^2)
sy = sum(y)
syy = sum(y^2)
sxy = sum(x*y)



;Coefficients
b1 = (n*sxy - sx*sy)/(n*sxx - sx^2)	;Slope
b0 = (1.0/n)*sy - b1*(1.0/n)*sx		;Intercept



;Standard errors, first calculated as variances
stdres	= (1./(n*(n-2.)))*(n*syy-sy^2-b1^2*(n*sxx-sx^2))	;Residuals
stdb1	= (n*stdres)/(n*sxx-sx^2)				;Slope
stdb0	= stdb1*(1./n)*sxx					;Intercept
stdres	= sqrt(stdres)
stdb1	= sqrt(stdb1)
stdb0	= sqrt(stdb0)

;Product-moment correlation coefficient
rcoef = (n*sxy - sx*sy)/(sqrt((n*sxx-sx^2)*(n*syy-sy^2)))

;Coefficient of determination
rsquare = rcoef^2

;Residuals
resid = y - (b0 + b1*x)

;Root mean square error
rmse = sqrt(avg((resid^2)))

;Mean absolute error
mae = avg(abs(resid^2))


;Confidence intervals
tup = cdft_t(1.0-((1.-(conflev/100.))/2.0),n-1);Upper t value
tlo = cdft_t(0.0+((1.-(conflev/100.))/2.0),n-1);Lower t value

cib0up = b0 + tup*stdb0
cib0lo = b0 + tlo*stdb0
cib1up = b1 + tup*stdb1
cib1lo = b1 + tlo*stdb1


;;;;;;;;OUTPUT;;;;;;;;;
;The output is the slope, and everything else comes as attributes
output = b1
output@intercept = b0
output@stdslope = stdb1
output@stdintercept = stdb0
output@rcoef = rcoef
output@rsquare = rsquare
output@conflev = conflev
output@cib0 = (/cib0up,cib0lo/)
output@cib1 = (/cib1up,cib1lo/)
output@rmse = rmse
output@mae = mae
output@number_of_observations = n
output@yintercept = b0

;print(output)
;print((/b0,b1/))
;print((/stdb1,stdb0/))
;print((/rcoef,rsquare/))
;print(resid)
;print((/rmse,mae/))

return(output)
end ;SIMPLE_REG






















undef ("reg_multlin_stats")
function reg_multlin_stats(Y[*]:numeric, XP:numeric, opt)
;
; Nomenclature:
; Y     - dependent variable (size NY)
;         missing values (_FillValue) are not allowed.
; XP    - one [*] or more [*][*] independent variables.
;         Missing values (_FillValue) are not allowed.
;         The size of the leftmost dimension of XP must be the
;         same size as Y[*]. The rightmost dimension for [*][*]
;         contains the independent variables.
; opt   - options [type logical: ie, True or False]
;         opt = False              .... no options               
;         opt = True
;         opr@print_data = True    .... print input Y and XP in table form
;         opr@print_anova= True    .... print ANOVA information
;
; See: 
; Wilks, D.S. (2006): Statistical Methods in the Atmospheric Sciences
;                     Academic Press (2nd Edition)
; Davis, J.C. (2002): Statistics and Data Analysis in Geology
;                     Wiley (3rd Edition)   ; pgs 462-470 
;
; Very readable discussions:
; http://www.ltrr.arizona.edu/~dmeko/notes_11.pdf
; http://reliawiki.org/index.php/Multiple_Linear_Regression_Analysis
; http://homepages.rpi.edu/~tealj2/stat03.pdf
; ---
;
local dimXP, N, NP, NY, M, T, Y, X, b, B, rankXP         \ ; local is
    , Yavg, Yvar, Ystd, Xavg, Xvar, Xstd, Yest, n, m     \ ; not necessary
    , Yres, SST, SSE, SSR, MST, MSE, MSR, SE, Frat       \
    , r2, ra, r, fuv, XXt, XXTi, varcovx, df, stderr, tval, pval \
    , opt_wm, fmt_XXt, fmt_XXti, fmt_covx, opt_pd, fmt_data
begin

       dimXP   = dimsizes(XP)
       NX      = dimXP(0)              ; number of rows ('observations')    
       N       = dimsizes(Y)

       rankXP  = dimsizes(dimXP)

; error checks on input

       if (N.NE.NX) then              
           print("reg_multlin_stats: number of rows of Y and XP  must match")
           print("                   NY="+N +"  NXP="+NX           )
           exit
       end if

       if (rankXP.gt.2) then
           print("reg_multlin_stats: rank of XP > 2: rankXP="+rankXP)
           exit
       end if

       if (isatt(Y,"_FillValue")  .and. any(ismissing(Y))) then
           print("reg_multlin_stats: Y has missing values. Not allowed!")
           exit
       end if

       if (isatt(XP,"_FillValue") .and. any(ismissing(XP))) then
           print("reg_multlin_stats: XP has missing values. Not allowed!")
           exit
       end if
           
; input OK: onward

       if (rankXP.eq.2) then
           NP  = dimXP(1)              ; number predictor variables   
       else
           NP  = 1                     ; one predictor
       end if

       if (opt .and. isatt(opt,"print_data") .and. opt@print_data) then
           print(" ")
           opt_pd = True
           opt_pd@title  = "----- reg_multlin_stats: Y, XP -----"
           data       = new( (/N,NP+1/), typeof(Y))
           data(:,0)  = (/ Y /)

           if (NP.eq.1) then
               data(:,1)  = (/ XP /)
           else
               data(:,1:) = (/ XP /)
           end if

           fmt_data   = "f11.2"
           if (isatt(opt,"fmt_data")) then
               fmt_data  = opt@fmt_data        ; eg: "f8.3", "e13.5",....
           end if
           write_matrix (data, (NP+1)+fmt_data , opt_pd)
           print(" ")
           delete(data)        ; not needed
       end if
       
; Create the necessary arrays. X is a "design matrix"  
;       http://en.wikipedia.org/wiki/Design_matrix
;
; Most multiple regression models include a constant term.
;   This ensures that the model will be "unbiased"
;   --i.e., the mean of the residuals will be exactly zero.

       M       = NP+1                     ; extra column for design matrix
                                          ; required ordering for reg_multlin 
       X       = new ( (/M,N/) , typeof(XP), getFillValue(XP))
       X(0,:)  = 1.0

       if (NP.eq.1) then
           X(1,:)  = XP                   ; one predictor
       else
           do m=0,NP-1  
              X(m+1,:)  = XP(:,m)         ; m refers to predictor variables
           end do
       end if

; Calculate the partial regression coefficients: b->beta_hat
;      b = (X'X)^-1 X'y  (theoretically)
; http://www.ncl.ucar.edu/Document/Functions/Built-in/reg_multlin.shtml
                                         
       b       = reg_multlin(Y,X,False)   ; partial regression coef 
                                          ; unstandardized...depend on units
       delete(b@constant)                 ; extraneous attribute

; Assorted calculations

       Yavg    = avg(Y)
       Yvar    = variance(Y)         ; Unbiased estimate the variance 
       Ystd    = sqrt(Yvar)

       Xavg    = new(NP, typeof(b), "No_FillValue")  
       Xvar    = new(NP, typeof(b), "No_FillValue")
       Xstd    = new(NP, typeof(b), "No_FillValue")

       bstd    = new(M,  typeof(b), "No_FillValue") ; standardized regression coef
                                     ; describe what the partial regression coef. 
                                     ; would equal if all variables had the same 
                                     ; standard deviation.
       do n=0,NP-1                 
          Xavg(n)   = avg(X(n+1,:))
          Xvar(n)   = variance(X(n+1,:))
          Xstd(n)   = sqrt(Xvar(n))
          bstd(n+1) = b(n+1)*Xstd(n)/Ystd       ; don't do b(0)
       end do

; Calculate model estimates: Yest = b(0) + b(1)*   + b(2)* + .....

       Yest = new(N,typeof(Y),getFillValue(Y))
       do n=0,N-1
          Yest(n) = b(0) + sum(b(1:)*X(1:,n))   ; array syntax 
       end do

; ANOVA table info. (Eg, Wilks pg 185 & pg 197)
;       Sums of squares , degrees of freedom, mean squares, F statistic
;       Other statistics are also calculated.
;
; http://reliawiki.org/index.php/ANOVA_Calculations_in_Multiple_Linear_Regression
;        Mean squares are obtained by dividing the sum of squares 
;        with their associated degrees of freedom. 

       dof  = N-NP-1 

       Yres = Yest-Y               ; residuals (array operation)

                                   ; SS ==> Sum of Squares 
       SST  = sum((Y-Yavg)^2)      ; total SS      [dof=N-1]
       SSR  = sum((Yest-Yavg)^2)   ; regression SS [dof=NP] ; (aka: model explained SS=>SSM)
       SSE  = sum(Yres^2)          ; residual SS   [dof=N-NP-1]
 
       MST  = SST/(N-1)            ; Mean SST      [dof=N-1]
       MSR  = SSR/NP               ; Mean SSR      [dof=NP]
       MSE  = SSE/dof              ; Mean SSE      [dof=(N-NP-1)] 

       RSE  = sqrt(MSE)            ; residual standard error
                                   
       Frat = MSR/MSE              ; R dof=(M,N-NP-1) 
                                   ; F-Test is an overall (global) test of the model’s fit.
                                   ; If at least one variable has a significant coefficient,
                                   ; then the model fit should be significant.
                                   ; The F-statistic might be interpreted as the variation 
                                   ; explained by the regression relative to the variation
                                   ; not explained by the regression.
                                   
       r2   = SSR/SST              ; r2 = coefficient of determination. It is
                                   ;      the square of the Pearson correlation 
                                   ;      coefficient between the observed 
                                   ;      and modeled (predicted) data values
                                   ;      of the dependent variable. It is
                                   ;      another measure of 'goodness of fit.'
                                   ; biased high, particularly for small N

       r2a  = r2-(1-r2)*NP/tofloat(dof)   ; adjusted r2... better for small N
       r    = sqrt(r2)             ; multiple (overall) correlation
       fuv  = 1.0-r2               ; fraction of variance *unexplained* 

; Determine (a) standard error of coefficients; (b) t-values; (c)  p-values

       XXt     = X#transpose(X)    ; (M,M); opposite of text books 
       XXti    = inverse_matrix(XXt)
       varcovx = MSE*XXti          ; variance-covariance matrix
                                   ; of estimated regression coefficients

       if (opt .and. isatt(opt,"debug") .and. opt@debug) then ; used for debugging only
           print(" ")
           print("-----  DEBUG ---------")
           print(" ")
           opt_wm = True
           opt_wm@title  = "-----   XXt  ---------"
           fmt_XXt = "f11.2"                     
           if (isatt(opt,"fmt_XXt")) then
               fmt_XXt  = opt@fmt_XXt            ; "f8.2", "f12.4", "e13.5", ...
           end if
           write_matrix (XXt, M+fmt_XXt , opt_wm)

           opt_wm@title  = "-----   XXti ---------"
           fmt_XXti = "f11.2"
           if (isatt(opt,"fmt_XXti")) then
               fmt_XXti = opt@fmt_XXti
           end if
           write_matrix (XXti, M+fmt_XXti , opt_wm)

           opt_wm@title  = "----- varcovx --------"
           fmt_covx = "f11.2"
           if (isatt(opt,"fmt_covx")) then
               fmt_covx = opt@fmt_covx
           end if
           write_matrix (varcovx, M+fmt_covx , opt_wm)

           print(" ")
       end if
           
       stderr = new( M, typeof(Y))
       tval   = new( M, typeof(Y))
       pval   = new( M, typeof(Y))

       df  = N-NP
       
       do m=0,M-1
          stderr(m) = sqrt(varcovx(m,m))  
          tval(m)   = b(m)/stderr(m)
          pval(m)   = betainc( df/(df+tval(m)^2), df/2.0, 0.5)
       end do

       if (opt .and. isatt(opt,"print_anova") .and. opt@print_anova) then
           print(" ")
           print("------- ANOVA information-------- ")
           print(" ")
           print("SST="+SST+"  SSR="+SSR+"  SSE="+SSE)
           print("MST="+MST+"  MSR="+MSR+"  MSE="+MSE+" RSE="+RSE)
           print("F-statistic="+Frat +" dof=("+NP+","+(N-NP-1)+")")
           print("          -------                 ")
         ;;print("r2="+r2+"  r="+r+"  ra="+ra+"  fuv="+fuv)  
         ;;print("          -------                 ")
         ;;print("stderr, tval, pval: "+stderr+"    "+tval+"    "+pval)
           print(" ")
       end if

; associate assorted information as attributes of 'b'

       b@long_name = "multiple regression coefficients"
       b@model     = "Yest = b(0) + b(1)*X1 + b(2)*X2 + ...+b(M)*XM"

       b@N     = N              ; # of 'observations'
       b@NP    = NP             ; # of predictors
       b@M     = M              ; design matrix size
       b@bstd  = bstd           ; standardized coefficients

       b@SST   = SST            ; [1]
       b@SSE   = SSE
       b@SSR   = SSR

       b@MST   = MST
       b@MSE   = MSE
       b@MSE_dof = dof 
       b@MSR   = MSR

       b@RSE   = RSE            ; [1]
       b@RSE_dof= N-M-1         ; [1]
       
       b@F     = Frat           ; [1]
       b@F_dof = (/NP,dof/)     ; [2]

       df1      = b@F_dof(0)
       df2      = b@F_dof(1)
       b@F_pval = ftest(MSR, df1+1, MSE, df2+1, 0)*0.5   ; [1]

       b@r2     = r2            ; [1]
       b@r      = r             ; [1]
       b@r2a    = r2a           ; [1]

       b@fuv    = fuv

       b@Yest   = Yest          ; [NY]
       b@Yavg   = Yavg          ; [1]
       b@Ystd   = Ystd          ; [1]

       b@Xavg   = Xavg          ; [1]
       b@Xstd   = Xstd          ; [1]

       b@stderr = stderr        ; [M]
       b@tval   = tval          ; [M]
       b@pval   = pval          ; [M]

       return(b)
end
;============================================




