!
! File is Program.f90
!
! This file controls the debug calls to the FORTRAN model
! ---------------------------------------------------------------------
!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 5.0

!       This is the Fortran code for the WaterSim_DCDC FORTRAN dll.

!       Copyright (C) 2014 , The Arizona Board of Regents
!              on behalf of Arizona State University

!       All rights reserved.

!       Developed by the Decision Center for a Desert City
!       Lead Model Development - David A. Sampson <david.a.sampson@asu.edu>

!       This program is free software: you can redistribute it and/or modify
!       it under the terms of the GNU General Public License version 3 as published by
!       the Free Software Foundation.

!       This program is distributed in the hope that it will be useful,
!       but WITHOUT ANY WARRANTY; without even the implied warranty of
!       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!       GNU General Public License for more details.

!       You should have received a copy of the GNU General Public License
!       along with this program.  If not, please see <http:!www.gnu.org/licenses/>.
!
!====================================================================================

! OUTPUTS: No Outputs
!
! created on 08.12.10
!
! david arthur sampson
!
! last write was: 01.15.13,07.21.14
! ---------------------------------
!

! =================================================================================================
    ! -------------------------
    program Program
          use lm_Initialize
           use lm_Kernel
           ! -- Types ------
           integer :: i=1
           ! ===============
            !
            call startupK()
            call readFilesCheckK()
            call InitK()
            !
            do i = 2000,2085,1
             call RunOneYearKernel()
            end do
           !
        !
        write(6,*)"Normal Process Termination"
        !
    end program Program
    ! -------------------------- 

    subroutine parmInitialization()
      use lm_Kernel
!        integer :: i
        !
        if(gpl_release)then
        else
         gvl_LULCstatus=.true.
          gvi_AgPumpCurveIndex=9
          gv_indexyearCO=1939
          !gv_indexyearCO=1975
          ! gv_indexyearCO=1906

!            do i = 1,35,1
!              gvf_parm_WWtoRWWTP(i)=0.5
!            end do
            !lvl_catchAPI=.true.
        endif
        !
     return
    end subroutine parmInitialization
! 
! =================================================================================================
! E.O.F. Program.f90