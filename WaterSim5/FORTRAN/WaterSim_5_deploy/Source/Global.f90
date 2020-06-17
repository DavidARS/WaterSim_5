!
!  File is Global.f90
!
!  Fhis file contains the global modules and defined types for WaterSim
!
! ---------------------------------------------------------------------------------------
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

! OUTPUTS: none
!

! Module:       Module gm_GlobalData
! Module:       Module gm_VersionControl
! Module:       Module gm_ModelControl
! Module:       Module gm_Exception

! Subroutines:  none
! Functions:    none

! created on 09.30.09
!
! david arthur sampson
! last write was: 01.15.13,07.21.14,07.22.14
! ------------------------------------------
!

! =========================================================
!
Module gm_GlobalData
    ! for opening text files
   character(len=30) :: gvc_Path='App_Data\WaterSim_5_0\'
   logical :: lexist
   integer :: LU,string
   character(len=25) :: module
   character(len=50) :: Infile
   character(200) :: errorString
   character(50) :: streamString
   ! Exception handeling
   integer :: gv_exceptionCode=0
   ! Fortran Mask
   logical :: gvl_mask(35)
   integer :: Ag_array_last=0
   ! used for defining array bounds
   integer,parameter :: gpi_laggedStartYr=1980
   integer, parameter :: gpi_lBY=2000
   integer, parameter :: gpi_uBY=2086
   ! Used for array declarations
   integer, parameter :: gvi_maxProV=35
    ! Used to conduct "do" loops, etc.
   integer, parameter :: gvi_Providers=35
   !
   ! Used for indexing (and assigning) arrays
   integer, parameter :: gpi_minPVY=2000
   integer, parameter :: gpi_maxPVY=2086
   ! Used in the Include file
   integer, parameter :: gpi_maxCOrec=1250
   integer, parameter :: gpi_maxSVTrec=700

    ! records SVT data
    ! For do loops
   integer, parameter :: gpi_dailySVTrec=24837 !24471
    ! For array defintion
   integer, parameter :: gpi_dailySVT=24837 !24471 !9150 was 366*25 years

    ! Order of when the demand or supply sources are called
    ! ------------------------------------------------------

   integer, parameter :: gpi_vadose=2
   integer, parameter :: gpi_directInject=3

   integer, parameter :: gpi_rainWaterHarvest=4
   integer, parameter :: gpi_stormWaterCapture=5
   integer, parameter :: gpi_grayWater=6

   integer, parameter :: gpi_reclaimed=7
   integer, parameter :: gpi_roReclaimed=8

   integer, parameter :: gpi_defPump=9
   integer, parameter :: gpi_classA=10
   integer, parameter :: gpi_ClassBC=11
   integer, parameter :: gpi_cap=12
   integer, parameter :: gpi_ncs=13
   integer, parameter :: gpi_newSupplies=14
   integer, parameter :: gpi_banking=15

   integer, parameter :: gpi_groundWater=16
   integer, parameter :: gpi_unmet=17
   integer, parameter :: gpi_unusedNonPotable=18
   !
   logical :: gvl_InOutDemandArray
   !
    
    ! GAP (stack) counter balance
    logical :: gpl_holdGD_one
    !
     ! Constants
   ! ----------------------------------------------------------
   real,parameter     :: gpf_daysMonth=30.416666667
   real,parameter     :: gpf_monthsPerYear=12
   real,parameter     :: gpf_ft3sToAcftDay=1.9834710458
   real,parameter     :: gpf_gallonsToliters=3.785411784
   real, parameter    :: gpf_sqftInOnesqm=10.763910417
   real,parameter     :: gpf_sqftInAcre=43560
   real,parameter     :: gpf_sqftTom2=0.09290304
   real(8), parameter :: gpd_acftTomaf=1e-6
   real(8), parameter :: gpd_galperacft=325851.43326
   real(8), parameter :: gpd_ft3sToacftmonth=60.33057764466317
   real(8), parameter :: gpd_m3Toacft=0.00081071318212
   real(8), parameter :: gpd_acftToft3=43560.000627
   !
   real,parameter     :: gpf_mmTometers=0.001
   real(8),parameter     :: gpf_cubicMetersToCubicmm=100000000
   real(8),parameter  :: gpf_cubicMetersToAF= 0.00081071318212
   !
   ! - Time-step of central model (1=annual, 12=monthly)
   integer, parameter :: gpi_timeStep=1
   integer, parameter :: gpi_timeStepWB=1
   !
   integer,parameter  :: LULC=13

   ! Default Time lag (in years) for vadose to aquifer
   integer, parameter :: gpi_Tlag=10
  ! -----------------------------------------------------------
   logical, parameter :: gpl_DemandDriven=.true.
   logical, parameter :: gpl_ClimateDriven=.false.
   logical, parameter :: gpl_SVT_SESDriven=.false. ! use simple exponential smoothing to reduce Climate factor on SVT 
   !

   ! These are Important
   ! -----------------------------------------------
   ! Start and end default 
   integer, parameter :: gpi_start=2000
   !integer, parameter :: gpi_end=2086
   integer, parameter :: gpi_end=2085
   !
   integer, parameter :: gpi_startCF=12
   integer, parameter :: gpi_lclu=2010
   !  Don Gross CO RIver comparisons - ADWR
   ! Also loads and unloads the Logical Units 100 to 105
   ! Release or Debug

    ! ---------------------------------------
    logical :: gvl_writeToLog=.false.   
    !logical :: gvl_writeLog=.false.
    ! ---------------------------------------
    ! End of 2014 - it is now 01.16.15 for use 2015
    ! as the start of projection data (in theory 
    ! we could have empirical data through 2014)
    integer, parameter :: gpi_projectionStartYear=2015  
    ! New Testing 2014 Summer
    ! -------------------------------------------
    logical :: gvl_writeError=.false.

    !   Provider .txt file (Unit 4) writes out
    ! All allocations- source-on and off project for
    ! each provider
    logical, parameter :: gpl_writeProviderDotTxt=.false.
    ! ---------------------------------------------------
   ! Use this for testing units
   logical, parameter :: gpl_comparisons=.false.
   logical,parameter ::  gpl_verify=.false.
   logical, parameter :: gpl_validate=.false.
   !
   logical, parameter :: gpl_testing2016=.false.    
   logical, parameter :: gpl_103=.false.
   !
   logical, parameter :: gpl_1=.false.
   logical, parameter :: gpl_writeLog=.false.
   logical, parameter :: gpl_release=.true.

 !
End Module gm_GlobalData
! =========================================================

! Version Control of the dll
! -----------------------------------------------
Module gm_VersionControl
  ! These are version control for dll labeling
  integer :: num
  integer, parameter :: gdi_Model=5
  integer, parameter :: gdi_Vers=9
  integer, parameter :: gdi_SubVers=7
  !
  integer, parameter :: gdi_Month=01
  integer, parameter :: gdi_Day  =26
  integer, parameter :: gdi_Year =17
  integer, parameter :: gdi_Hour =12
  integer, parameter :: gdi_Min  =28
End Module gm_VersionControl
! ===============================================

! ----------------------------------------------------------
!   Controls over run-time events. Keeps
! tract of time events
! ==========================================================
Module gm_ModelControl
 use gm_GlobalData
    type MyPolicy
      integer :: harvesting,stormwater,ROreclaimedWater,reclaimed
      integer :: augmented,waterBanking
    end type
    ! Simulation Controls
    !
    type runTime
     ! simyear is 0 to N (year number in simulation stream)
     integer :: year=0,month=0,simyear=0,policyYear=0,lcluYear=0
     integer :: jumpYear
     integer startyear,endyear
     integer simulations         
               ! Number of years to simulate (i.e. 25)
     integer :: days,gvi_holdrun,gvi_holdrun2
     real :: propYears
     character(len=2), dimension(35) :: providers
     character(len=2), dimension(35) :: align

     logical :: atStartOfSimulation,atStartOfProviderLoop
     logical :: atEndOfProviderLoop
      type(MyPolicy) :: Policy

    end type
!     contains
!       function StartPolicy(this,num)
!         type(runTime) this
!          integer :: StartPolicy
!          integer :: num
!          StartPolicy = this%mypolicy(num)
!        end function
    
End Module gm_ModelControl
! ==========================================================

! ---------------------------------------------
!    Exception to tract run-time errors 
! (use in subroutines or functions)
! =============================================
Module gm_Exception
  type Exception
   integer :: Prov,gvi_hold
   integer :: TStep,hold
   !
   character(8) :: MySub
   character(10) :: MyFunction
   character(20) :: Mycall
   character(12) :: boundary_1
   character(10) :: align
  end type Exception
End Module gm_Exception
! =============================================

!
! ===================================================================
! E.O.F. Global.f90