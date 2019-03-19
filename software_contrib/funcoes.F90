
!-------------------------------------------------------------------------
integer function textcls(msand,mclay)
! adapted by cjk 01/11/01
!-------------------------------------------------------------------------
! |
! |                         T R I A N G L E
! | Main program that calls WHAT_TEXTURE, a function that classifies soil
! | in the USDA textural triangle using sand and clay %
! +-----------------------------------------------------------------------
! | Created by: aris gerakis, apr. 98 with help from brian baer
! | Modified by: aris gerakis, july 99: now all borderline cases are valid
! | Modified by: aris gerakis, 30 nov 99: moved polygon initialization to
! |              main program
! +-----------------------------------------------------------------------
! | COMMENTS
! | o Supply a data file with two columns, in free format:  1st column sand,
! |   2nd column clay %, no header.  The output is a file with the classes.
! +-----------------------------------------------------------------------
! | You may use, distribute and modify this code provided you maintain
! ! this header and give appropriate credit.
! +-----------------------------------------------------------------------
!
! input variables
      integer  msand,&
               mclay

! local variables
      logical inpoly

      real*8  silty_loam(1:7,1:2),      &
              sandy(1:7,1:2),           &
              silty_clay_loam(1:7,1:2), &
              loam(1:7,1:2),            &
              clay_loam(1:7,1:2),       &
              sandy_loam(1:7,1:2),      &
              silty_clay(1:7,1:2),      &
              sandy_clay_loam(1:7,1:2), &
              loamy_sand(1:7,1:2),      &
              clayey(1:7,1:2),          &
              sandy_clay(1:7,1:2)

! ---------------------------------------------------------------------
! initalize polygon coordinates:
! each textural class reads in the sand coordinates (1,7) first, and
! then the corresponding clay coordinates (1,7)

!     data silty_loam/0, 0, 23, 50, 20, 8, 0, 12, 27, 27, 0, 0, 12, 0/
!
! because we do not have a separate silt category, have to redefine the
! polygon boundaries for the silt loam  
      data sandy           /85, 90, 100, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0/
      data loamy_sand      /70, 85, 90, 85, 0, 0, 0, 0, 15, 10, 0, 0, 0, 0/
      data sandy_loam      /50, 43, 52, 52, 80, 85, 70, 0, 7, 7, 20, 20, 15, 0/
      data loam            /43, 23, 45, 52, 52, 0, 0, 7, 27, 27, 20, 7, 0, 0/
      data silty_loam      /0, 0, 23, 50, 0, 0, 0, 0, 27, 27, 0, 0, 0, 0/ 
      data sandy_clay_loam /52, 45, 45, 65, 80, 0, 0, 20, 27, 35, 35, 20, 0, 0/
      data clay_loam       /20, 20, 45, 45, 0, 0, 0, 27, 40, 40, 27, 0, 0, 0/
      data silty_clay_loam /0, 0, 20, 20, 0, 0, 0, 27, 40, 40, 27, 0, 0, 0/
      data sandy_clay      /45, 45, 65, 0, 0, 0, 0, 35, 55, 35, 0, 0, 0, 0/
      data silty_clay      /0, 0, 20, 0, 0, 0, 0, 40, 60, 40, 0, 0, 0, 0/
      data clayey          /20, 0, 0, 45, 45, 0, 0, 40, 60, 100, 55, 40, 0, 0/

! polygon coordinates  
!
!     sand
!
!     >  85, 90, 100, 0, 0, 0, 0,       ! sand
!     >  70, 85, 90, 85, 0, 0, 0,       ! loamy sand
!     >  50, 43, 52, 52, 80, 85, 70,    ! sandy loam
!     >  43, 23, 45, 52, 52, 0, 0,      ! loam
!     >   0, 0, 23, 50, 0, 0, 0,        ! silt loam (combined with silt)
!     >  52, 45, 45, 65, 80, 0, 0,      ! sandy clay loam
!     >  20, 20, 45, 45, 0, 0, 0,       ! clay loam
!     >   0, 0, 20, 20, 0, 0, 0,        ! silty clay loam
!     >  45, 45, 65, 0, 0, 0, 0,        ! sandy clay
!     >   0, 0, 20, 0, 0, 0, 0,         ! silty clay 
!     >  20, 0, 0, 45, 45, 0, 0         ! clay
!
!      clay
!
!     > 0, 10, 0, 0, 0, 0, 0,           ! sand
!     > 0, 15, 10, 0, 0, 0, 0,          ! loamy sand
!     > 0, 7, 7, 20, 20, 15, 0,         ! sandy loam 
!     > 7, 27, 27, 20, 7, 0, 0,         ! loam
!     > 0, 27, 27, 0, 0, 0, 0,          ! silt loam (combined with silt)
!     > 20, 27, 35, 35, 20, 0, 0,       ! sandy clay loam
!     > 27, 40, 40, 27, 0, 0, 0,        ! clay loam
!     > 27, 40, 40, 27, 0, 0, 0,        ! silty clay loam
!     > 35, 55, 35, 0, 0, 0, 0,         ! sandy clay
!     > 40, 60, 40, 0, 0, 0, 0,         ! silty clay
!     > 40, 60, 100, 55, 40, 0, 0       ! clay
!
! +-----------------------------------------------------------------------
! | figure out what texture grid cell and layer are part of  
! | classify a soil in the triangle based on sand and clay %
! +-----------------------------------------------------------------------
! | Created by: aris gerakis, apr. 98
! | Modified by: aris gerakis, june 99.  Now check all polygons instead of
! | stopping when a right solution is found.  This to cover all borderline 
! | cases.
! +-----------------------------------------------------------------------

!
! find polygon(s) where the point is.  
      textcls = 0 
      if (msand .gt. 0.0 .and. mclay .gt. 0.0) then
         if (inpoly(sandy, 3, msand, mclay)) then
            textcls = 1      ! sand
         endif
         if (inpoly(loamy_sand, 4, msand, mclay)) then
            textcls = 2      ! loamy sand
         endif
         if (inpoly(sandy_loam, 7, msand, mclay)) then
            textcls = 3      ! sandy loam
         endif
         if (inpoly(loam, 5, msand, mclay)) then
            textcls = 4      ! loam
         endif
         if (inpoly(silty_loam, 4, msand, mclay)) then
            textcls = 5      ! silt loam
         endif
         if (inpoly(sandy_clay_loam, 5, msand, mclay)) then
            textcls = 6      ! sandy clay loam
         endif
         if (inpoly(clay_loam, 4, msand, mclay)) then
            textcls = 7      ! clay loam
         endif
         if (inpoly(silty_clay_loam, 4, msand, mclay)) then
            textcls = 8      ! silty clay loam
         endif
         if (inpoly(sandy_clay, 3, msand, mclay)) then
            textcls = 9      ! sandy clay
         endif
         if (inpoly(silty_clay, 3, msand, mclay)) then
            textcls = 10     ! silty clay
         endif
         if (inpoly(clayey, 5, msand, mclay)) then
            textcls = 11     ! clay
         endif
      endif

      if (textcls .eq. 0) then
         textcls = 5         ! silt loam
!1000    format (/, 1x, 'Texture not found for ', f5.1, ' sand and ', f5.1, ' clay')
      endif

return

end function textcls

     logical function inpoly (poly, npoints, xt, yt)
!
! adapted by cjk 01/11/01
! ---------------------------------------------------------------------------
!
!                            INPOLY
! Function to tell if a point is inside a polygon or not.
! --------------------------------------------------------------------------
! Copyright (c) 1995-1996 Galacticomm, Inc.  Freeware source code.
!
! Please feel free to use this source code for any purpose, commercial
! or otherwise, as long as you don't restrict anyone else's use of
! this source code.  Please give credit where credit is due.
!
! Point-in-polygon algorithm, created especially for World-Wide Web
! servers to process image maps with mouse-clickable regions.
!
! Home for this file:  http://www.gcomm.com/develop/inpoly.c
!
!                 6/19/95 - Bob Stein & Craig Yap
!                       stein@gcomm.com
!                      craig@cse.fau.edu
! --------------------------------------------------------------------------
! Modified by:
! Aris Gerakis, apr. 1998: 1.  translated to Fortran
!                          2.  made it work with real coordinates
!                          3.  now resolves the case where point falls
!                              on polygon border.
! Aris Gerakis, nov. 1998: Fixed error caused by hardware arithmetic
! Aris Gerakis, july 1999: Now all borderline cases are valid
! --------------------------------------------------------------------------
! Glossary:
! function inpoly: true=inside, false=outside (is target point inside
!                    a 2D polygon?)
!   poly(*,2):  polygon points, [0]=x, [1]=y
!   npoints: number of points in polygon
!   xt: x (horizontal) of target point
!   yt: y (vertical) of target point
! --------------------------------------------------------------------------
!
! input variables
!
     integer ::  npoints, &
                 xt,      &
                 yt
!
     real*8 ::   poly(7, 2)
!
! local variables
!
     real*8 ::  xnew,   &
                ynew,   &
                xold,   &
                yold,   &
                x1,     &
                y1,     &
                x2,     &
                y2
!
     integer ::  i
!
     logical :: inside, on_border


     inside = .false.
     on_border = .false.
!
     if (npoints .lt. 3)  then
        inpoly = .false.
        return
     end if
!
     xold = poly(npoints,1)
     yold = poly(npoints,2)

     do 300  i = 1 , npoints
        xnew = poly(i,1)
        ynew = poly(i,2)

        if (xnew .gt. xold)  then
           x1 = xold
           x2 = xnew
           y1 = yold
           y2 = ynew
        else
           x1 = xnew
           x2 = xold
           y1 = ynew
           y2 = yold
        end if

! the outer IF is the 'straddle' test and the 'vertical border' test.
! the inner IF is the 'non-vertical border' test and the 'north' test.

! the first statement checks whether a north pointing vector crosses
! (stradles) the straight segment.  There are two possibilities, depe-
! nding on whether xnew < xold or xnew > xold.  The '<' is because edge
! must be "open" at left, which is necessary to keep correct count when
! vector 'licks' a vertix of a polygon.

        if ((xnew .lt. xt .and. xt .le. xold) .or. &
            (.not. xnew .lt. xt .and. .not. xt .le. xold)) then
!
! the test point lies on a non-vertical border:
!
           if ((yt-y1)*(x2-x1) .eq. (y2-y1)*(xt-x1)) then
              on_border = .true.
!
! check if segment is north of test point.  If yes, reverse the
! value of INSIDE.  The +0.001 was necessary to avoid errors due
! arithmetic (e.g., when clay = 98.87 and sand = 1.13):
!
           elseif ((yt-y1)*(x2-x1) .lt. (y2-y1)*(xt-x1) + 0.001) then
              inside = .not.inside ! cross a segment
           endif
!
! this is the rare case when test point falls on vertical border or
! left edge of non-vertical border. The left x-coordinate must be
! common.  The slope requirement must be met, but also point must be
! between the lower and upper y-coordinate of border segment.  There
! are two possibilities,  depending on whether ynew < yold or ynew >
! yold:
!
        elseif ((xnew .eq. xt .or. xold .eq. xt) .and. (yt-y1)*(x2-x1) &
                 .eq. (y2-y1)*(xt-x1) .and. ((ynew .le. yt .and. yt    &
                 .le. yold) .or. (.not. ynew .lt. yt .and. .not. yt    &
                 .lt. yold))) then
                    on_border = .true.
        endif
!
        xold = xnew
        yold = ynew
!
 300    continue  
!
! If test point is not on a border, the function result is the last state
! of INSIDE variable.  Otherwise, INSIDE doesn't matter.  The point is
! inside the polygon if it falls on any of its borders:
!
     if (.not. on_border) then
        inpoly = inside
     else
        inpoly = .true.
     endif
!




      end function inpoly




