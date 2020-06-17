!
!      WaterSimDCDC Regional Water Demand and Supply Model Version 6.0

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
!
!



! ===================================================================================
Module lm_LULC
 use gm_ModelControl
  use gm_GlobalData
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_5\WaterSimDCDC.txt"
    !
  contains
    !
    ! ---------------------
     subroutine initLULC()
       !if(6 <=gdi_Model)then
        call openFiles_lu()
        call readFiles_lu()
       !endif
      return
     end subroutine initLULC
    ! ---------------------

      ! ------------------------
      subroutine openFiles_lu()
          !
          ! -------------- Types -------------
          character(len=200) :: lvc_DPath=' '
          ! ==================================
          !
                !
                if(gpl_release)then
                    lvc_DPath=trim(gvc_DPath)
                else
                    lvc_DPath=gvc_Path
                endif
              !
              module="lm_LULC"
              !
               Infile='App_Data\Data\landUseLandCover_2010.txt'; LU=81
               call openFiles(module,lvc_DPath,Infile,LU)
              !
              Infile='App_Data\Data\lULC_Scenario.txt'; LU=82
               call openFiles(module,lvc_DPath,Infile,LU)
              !
          !
        return
1000    continue
        !
         gvo_errorCode=1
           if(gvl_writeLog)then
                string=61
                LU=0
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
             endif
            gvl_errorFlag=.false.
        !
      end subroutine openFiles_lu
     ! ---------------------------

        ! ------------------------
        subroutine readFiles_lu()
            !
            ! -------- Types -------
            integer :: i,j,ios
            ! ======================
            ios=0
            !----------------
            ! Read from Disk
            ! Provider-level data;
            !--------------------------------------------------------------------------------------
            ! acft a-1
            ! 
            ! ---------------------
            read(81,*,err=810,iostat=ios)((gvf_landCover_2010(i,j),j=1,gvi_maxProV),i=1,LULC)      
            ! Missing "Other Provider"
            !
810          continue
            close(81)
            if(ios >0)then
             LU=81
             gvo_errorCode=80
             goto 1000
            endif
            ! ---------------------

            read(82,*,err=820,iostat=ios)((gvf_landCover_2060(i,j),j=1,gvi_maxProV),i=1,LULC)      
            ! Missing "Other Provider"
            !
820          continue
            close(82)
            if(ios >0)then
             LU=82
             gvo_errorCode=80
             goto 1000
            endif
            ! ----------------------
         return
1000     continue
           if(gvl_writeLog)then
              string=63
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
          gvl_errorFlag=.false.
          !
        end subroutine readFiles_lu
        ! --------------------------
!
End Module lm_LULC

! ---------------------------------
    subroutine initializeLULC()
        use lm_LULC
        !
            call initLULC()
        !
      return
    end subroutine initializeLULC
    ! -----------------------------------

Module lms_LULC
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
        !
        ! -----
        include "C:\WaterSim\FORTRAN\Include\WaterSim_5\WaterSimDCDC.txt"
        !
        real,parameter :: mpf_timeDifference=50
        real,parameter :: mpf_x1=2010
        real,parameter :: mpf_x2=2009
        !
  contains

        ! -----------------------------------------------------------------------------------------

        ! Surface water subroutines from rainfall
        ! ------------------------------------------ 
        subroutine ProviderRCN(T,Provider,lvf_xbarRCN)
            !
            ! ---------- Types -------------
            integer :: Provider,i

            real :: lvf_RCN(13) ! m2
            real ::lvf_PropLandCover(13)
            real :: lvf_add,lvf_totalRCN
            real :: lvf_xbarRCN(gvi_maxProV) ! individual provider-specific possible, though not yet used
           ! real :: lvf_LCLU_proportions(gvi_maxProV,13)
            ! -------------------------------
            !
!            type (waterbalance)lvWB
            type(runTime)T
            ! ==============================================
                !
!                ! ------------------------------------
                 ! New RCN numbers for the Scenario Project
                 ! 03.06.16 DAS
                 lvf_RCN(1)=74 ! Ag
                 lvf_RCN(2)=89 ! building - use commercial business, soil group A
                 lvf_RCN(3)=10 ! canal - assuming some runoff
                 lvf_RCN(4)=61 ! Turf
                 lvf_RCN(5)=70 ! Greenway : Turf plus water plus impervious
                 lvf_RCN(6)=98 ! Impervious
                 lvf_RCN(7)=96 ! Mtn Vegetation
                 lvf_RCN(8)=75 ! residential - using 1/4 acre B soil group
                 lvf_RCN(9)=82 ! soil/desert - using "dirt Right-of-way" B hydrologic soil group
                 lvf_RCN(10)=68 ! tree - poor condition grass cover
                 lvf_RCN(11)=50 ! unclassified
                 lvf_RCN(12)=76 ! wash - using the "A hydrologic soil group for gravel
                 lvf_RCN(13)=0 ! water
                !http://onlinemanuals.txdot.gov/txdotmanuals/hyd/nrcs_runoff_curve_number_methods.htm
!                lvf_RCN(1)=61 ! Grass 39,61,74,80  correspond to A,B,C,D soil groups (see pub)
!                lvf_RCN(2)=98 ! Paved all (A,B,C,D) =98
!                lvf_RCN(3)=85 ! Gravel 76,85,89,91
!                lvf_RCN(4)=82 ! "Dirt" 72,82,87,89 
!                lvf_RCN(5)=77 ! Desert Landscaping 63,77,85,88
!                lvf_RCN(6)=96 ! Artificial Desert 96
!                lvf_RCN(7)=92 ! Commercial 89,92,94,95
!                lvf_RCN(8)=88 ! Industrial 81,88,91,93
!                lvf_RCN(9)=70 ! Residential -mesic-(using 1/2 acre lot) 54,70,80,85
!                lvf_RCN(10)=85 ! Residedntial -xerix- (using 1/8 acre lot size) 77,85,90,92
!                lvf_RCN(11)=83 ! "cultivated" 74,83,88,90
                !
!                if( T%year < gpi_lclu)then
!                  call sDefaultRCN(T,i,lvf_xbarRCN,lvf_PropLandCover)
!                else
!                  call sProportionalLandCover(T%year,Provider,T%lcluYear,lvf_PropLandCover)
!                  call sLandUseLandCoverArea(T%lcluYear,Provider,lvf_PropLandCover)
!                endif
                !               
                lvf_totalRCN=0.
                lvf_add=0.
                !
                do i = 1,LULC,1
                !
                lvf_PropLandCover(i)=gvf_waterUseRatioLCLU(i,Provider)
                 !
                 gvf_LULCProp(i,provider)=lvf_PropLandCover(i)
                 !lvf_LCLU_proportions(provider,i)=lvf_PropLandCover(i)
                 gvf_propLandCover(i,provider)=lvf_PropLandCover(i)
                 !
                 lvf_add=lvf_RCN(i)*lvf_PropLandCover(i)
                 lvf_totalRCN=lvf_totalRCN+lvf_add
                   lvf_xbarRCN(Provider)=lvf_totalRCN
                end do
                !
            !
         return
        end subroutine ProviderRCN
        ! ------------------------
 
        ! -----------------------------
        subroutine sLCLUArea(T,provider)
            ! ---------- Types -------------
            integer :: Provider
            real ::lvf_PropLandCover(13)
            ! -------------------------------
            !
            type(runTime)T
            ! ==============================================
                !
                call sProportionalLandCover(T%year,Provider,T%lcluYear,lvf_PropLandCover)
                call sLandUseLandCoverArea(T%lcluYear,Provider,lvf_PropLandCover)
            !
          return
        end subroutine sLCLUArea
        ! -----------------------------

        ! ------------------------------
        subroutine sProportionalLandCover(year,provider,policyYear, lvf_PropLandCover)
            ! ----------------------- types ------------------
            integer :: i,k,provider,year,policyYear
            real :: lvf_PctLandCover(13),lvf_PropLandCover(13)

            ! ================================================
                !
                if(gvl_LULCstatus)then
                  !
                  call sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)
                  !
                else
                ! New values for 2015 - Default when data not available
                ! ----------------------------
                ! Set the 2010 estimate, the 2011 and beyond will be calculated by the change
                ! in Agricultural acreas as determined by the UI using the API field "WEBAGTR1"
                ! 09.04.16 das
                if(year < 2011)then
                   call sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)          
                    do k = 1,13,1
                     gvf_LULCProp(k,provider)=lvf_PctLandCover(k)* 0.01
                    end do
                else
                    do k = 1,13,1
                     lvf_PctLandCover(k)= gvf_LULCProp(k,provider)
                    end do

                endif
                ! =======================================
                endif
                ! New classifications from Elizabeth - March, 2016
                ! Scenarios
                !1-AG
                !2-building
                !3-canal
                !4-cultivated grass
                !5-greenway
                !6-impevious
                !7-mountain vege
                !8-residential
                !9-soil
                !10-tree
                !11-unclassified
                !12-wash
                !13-water
                !
                if(gvl_LULCstatus)then
                  do i = 1,13,1
                   lvf_PropLandCover(i)= lvf_PctLandCover(i) * 0.01
                  end do
                else
                  do i = 1,13,1
                   lvf_PropLandCover(i)= gvf_LULCProp(i,provider)
                  end do        
                endif
                !
          return
        end subroutine sProportionalLandCover
        ! ------------------------------------

        ! ------------------------------------
        subroutine sLandUseLandCoverArea(PolicyYear,provider,lvf_PropLandCover)
            ! ------------- Types -------------
            integer :: i,PolicyYear,provider
            real :: lvf_PropLandCover(13)
            real :: vArea(gvi_maxProV)
            ! =================================
                !
                ! Provider service area (m2)
                call ProviderArea2012(vArea)
                !
                PolicyYear=max(1,PolicyYear)
                    do i = 1,LULC,1
                     if(PolicyYear <=55)then
                       gvf_areaLCLU(i,provider,PolicyYear)=0
                      gvf_areaLCLU(i,provider,PolicyYear)=lvf_PropLandCover(i)*vArea(provider)
                        !
                        gvf_propLandCover(i,provider)=lvf_PropLandCover(i)
                        !
                     endif
                      gvf_waterUseRatioLCLU(i,provider)=0
                    end do
                !

                gvf_waterUseRatioLCLU(1,provider)= lvf_PropLandCover(1)
                gvf_waterUseRatioLCLU(2,provider)= lvf_PropLandCover(2)
                gvf_waterUseRatioLCLU(4,provider)= lvf_PropLandCover(4)
                gvf_waterUseRatioLCLU(5,provider)= lvf_PropLandCover(5)
                gvf_waterUseRatioLCLU(8,provider)= lvf_PropLandCover(8)
                gvf_waterUseRatioLCLU(9,provider)= lvf_PropLandCover(9)
                gvf_waterUseRatioLCLU(10,provider)=lvf_PropLandCover(10)
            !
            return
        end subroutine sLandUseLandCoverArea
        ! ------------------------------------    
 
        ! --------------------------------------------------
        subroutine sSetWaterUseByLCLU(T,provider)
            ! ------------ types ------------
            integer :: i,provider,PolicyYear
            real :: temp,lvf_rawWeight
            ! ===============================

            type(runTime)T
            ! ===============================
                !
                temp=0
                PolicyYear=0
                PolicyYear=T%lcluYear ! max(1,PolicyYear)
                !
                if(mpf_x1 <= T%year)then
                    !
                    if(T%year <=gvi_baseYear )then
                         temp=gvf_TotalDemand_acft(provider) + go_AgToLCLUdemand(provider)
                        ! Ag=go_AgToLCLUdemand(provider)
                        !
                        do i = 1,LULC,1
                          gvf_waterUseLCLUsquareMeter(i,provider)=0
                          !
                          call sGetWeighting(i,provider,lvf_rawWeight)
                               if(PolicyYear <=55)then
                                if(0 < gvf_areaLCLU(i,provider,PolicyYear))then
                                 gvf_waterUseLCLUsquareMeter(i,provider)= ((temp*lvf_rawWeight)* gvf_waterUseRatioLCLU(i,provider)) &
                                  / gvf_areaLCLU(i,provider,PolicyYear)
                                endif
                               endif
                        end do
                    endif
                endif
          return
        end subroutine sSetWaterUseByLCLU
        ! ------------------------------------

        ! ------------------------------------
        subroutine sGetWaterUseByLCLU(T,provider)
            ! ------------ types ------------
            integer :: i,provider
            integer :: amount
            real :: vArea(gvi_maxProV)
            real :: temp
            real :: startLULC=2010
            real :: fLCLUefficiencies
            ! ===============================

            type(runTime)T
            ! ==============================================
                !
                if(startLULC <= T%year)then
                    call ProviderArea2012(vArea)
                    !
                    do i = 1,13,1
                      gvf_waterUseLCLU(i,provider)=0
                      temp=0
                      !
                      temp= gvf_propLandCover(i,provider)*((gvf_waterUseLCLUsquareMeter(i,provider)*vArea(provider)))
                      !
                      call sGetRawEfficiencies(i,amount)
                      gvf_waterUseLCLU(i,provider) = temp*fLCLUefficiencies(T%lcluYear,amount)
                      !
                    end do
                    !
                endif
          return
        end subroutine sGetWaterUseByLCLU
        ! -------------------------------
        ! ==================================
        subroutine sGetRawEfficiencies(lvi_lclu,lvf_rawEfficiency)
            ! --------- types ----------
            integer :: lvi_lclu
            integer :: lvf_rawEfficiency
            ! ==========================
                !
                select case(lvi_lclu)
                    case(1)
                     lvf_rawEfficiency=80
                    case(8)
                     lvf_rawEfficiency=65
                    case default
                     lvf_rawEfficiency=90
                end select
                !
            !
          return
        end subroutine sGetRawEfficiencies
        ! =================================

       ! =================================
        subroutine sGetWeighting(lvi_lclu,provider,lvf_rawWeight)
            ! --------- types ----------
            integer :: lvi_lclu
            integer:: provider

            real :: lvf_raw_2
            real :: lvf_raw_4
            real :: lvf_raw_5
            real :: lvf_raw_8
            real :: lvf_raw_10
            real :: lvf_rawWeight,lvf_raw
            real :: lvf_reduce
            ! ==========================
                !
                lvf_reduce=0.965
                lvf_rawWeight=0
                !
                lvf_raw_2=0;lvf_raw_4=0;lvf_raw_5=0;lvf_raw_8=0;lvf_raw_10=0
                !
                lvf_raw_2= (gvf_parm_WStoCom_prop(provider)+gvf_parm_WStoInd_prop(provider))* lvf_reduce
                lvf_raw_4= 0.027
                lvf_raw_5= 0.02
                lvf_raw_8= gvf_parm_WStoRes_prop(provider)*lvf_reduce
                lvf_raw_10=  max(0,1 - (lvf_raw_2+lvf_raw_4+lvf_raw_5+lvf_raw_8))
                !
                select case(lvi_lclu)
                    case(1)
                     lvf_raw=1
                    case(2)
                     lvf_raw=lvf_raw_2
                    case(3)
                    case(4)
                     lvf_raw=lvf_raw_4
                    case(5)
                     lvf_raw=lvf_raw_5
                    case(6)
                    case(7)
                    case(8)
                     lvf_raw=lvf_raw_8
                    case(9)
                    case(10)
                     lvf_raw=lvf_raw_10
                    case(11)
                    case(12)
                    case(13)
                    case default
                     lvf_raw=0
                end select
                !
                lvf_rawWeight=lvf_raw

            !
          return
        end subroutine sGetWeighting
        ! =================================

            ! ===========================================================
            subroutine sMultiplyMatricies(year,provider,policyYear,lvf_PctLandCover)
                ! ---------------- types -------------------
                integer :: i,provider,year,policyYear
                real :: lvf_PctLandCover(LULC)
                real :: lvf_Y1
                logical :: lvl_flag
                ! ==========================================
                    !
                    lvl_flag=.false.
                    ! T%year <= 2010
                   if(year <= mpf_x1)then
                       do i = 1,LULC,1
                        lvf_PctLandCover(i)=gvf_landCover_2010(i,provider)
                       enddo                     
                        ! Must be called once to determime slope and intercept terms for
                        ! linear interpolation between 2010 and 2060
                        if(mpf_x2 < year)then
                         call setLinearLCLU(provider)
                        endif
                   else                    
                    !
                    call getLCLUpct(year,policyYear,provider,lvf_PctLandCover)
                    !
                      lvf_Y1=gvf_landCover_2010(1,provider)
                     if(99.9 < lvf_Y1)lvl_flag=.true.
                     if(2060 < year)lvl_flag=.true.
                    !
                     if(lvl_flag)then
                       call sDefaultLandCover(lvf_PctLandCover)
                     endif
                   endif
                !
              return
            end subroutine sMultiplyMatricies
            ! =====================================

            ! =====================================
            subroutine getLCLUpct(year,policyYear,provider,lvf_PctLandCover)
                ! ------------------ types -----------------
                integer :: lvi_lclu,provider,year,policyYear
                real :: lvf_PctLandCover(LULC)
                logical :: lvl_static
                ! ==========================================
                    !
                    ! ! Scenarios: 1-AG,2-building,3-canal,4-cultivated grass,5-greenway,6-impevious
                    ! 7-mountain vege,8-residential,9-soil,10-tree,11-unclassified,12-wash,13-water
                    !
                    ! Trajectories in the change in LCLU over time (1010-2060) for the 13 cover types
                    ! 05.19.16
                    ! =============================================================================
                      lvl_static=.false.
                    do lvi_lclu=1,13,1
                        !
                        select case(lvi_lclu)
                            case(1)
                             call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                            case(2)
                             call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)

                            case(4)
 ! call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                             !call hyperbolLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
   call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                            case(5)
!call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                             call hyperbolLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)



!call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                            case(8)
                             call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)



                            case(10)
   call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)


                            case default
  call logisticLCLU(policyYear,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                             !call getLinearLCLU(year,lvi_lclu,provider,lvl_static,lvf_PctLandCover)
                        end select
                        !                 
                    end do
                !
              return
            end subroutine getLCLUpct
            ! =====================================

            ! =====================================
            subroutine setLinearLCLU(provider)
                ! --------- Types ---------
                integer :: i,provider
                real :: lvf_Y1,lvf_y2,lvf_m, lvf_b !,lvf_est
                ! =========================
                    !
                      lvf_y2=0
                      lvf_Y1=0
                      lvf_m=0
                      lvf_b=0
                    !
                     do i = 1,LULC,1
                        lvf_y2=gvf_landCover_2060(i,provider)
                        lvf_Y1=gvf_landCover_2010(i,provider)
                        lvf_m=(lvf_y2-lvf_y1)/mpf_timeDifference
                        !
                        lvf_b=lvf_Y1-(lvf_m*mpf_x1)
                        !
                        gvf_slope(i,provider)=lvf_m
                        gvf_intercept(i,provider)=lvf_b
                       ! lvf_est = max(0, lvf_m * year + lvf_b)
                      end do
                    !
              return
            end subroutine setLinearLCLU
            ! =====================================

            ! ===================================
            subroutine getLinearLCLU(year,j,provider,lvl_static,lvf_PctLandCover)
                ! -------- Types -------
                integer :: i,j,provider,year
                real :: lvf_PctLandCover(LULC)
                logical :: lvl_static
                ! ======================
                    !
                    if(lvl_static)then
                     do i = 1,13,1
                       lvf_PctLandCover(i)=gvf_slope(i,provider)* year + gvf_intercept(i,provider)   
                     enddo
                    else
                       lvf_PctLandCover(j)=gvf_slope(j,provider)* year + gvf_intercept(j,provider)   
                    endif
                    !
                !
              return
            end subroutine getLinearLCLU
            ! ===================================

            ! =====================================
            subroutine logisticLCLU(Time,j,provider,lvl_static,lvf_PctLandCover)
                ! ------------- Types ---------------
                integer :: i,j,provider,Time
                real :: lvf_y2,lvf_Y1,lvf_proportion
                real :: lvf_PctLandCover(LULC)
                real :: fLogisticLCLUstatic
                logical :: lvl_static
                ! ===================================
                    !  Time = T%policyYear (zero to x)
                    !
                    if(lvl_static)then
                        lvf_proportion =0
                       lvf_proportion =  fLogisticLCLUstatic(Time)
                      do i = 1,LULC,1
                        lvf_y2=gvf_landCover_2060(i,provider)
                        lvf_Y1=gvf_landCover_2010(i,provider)
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(i)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(i)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(i)=0
                        endif
                      end do
                    else
                          lvf_proportion =0
                         lvf_proportion =  fLogisticLCLUstatic(Time)
                         lvf_y2=gvf_landCover_2060(j,provider)
                         lvf_Y1=gvf_landCover_2010(j,provider)
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(j)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(j)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(j)=0
                        endif

                    endif
                !
              return
            end subroutine logisticLCLU
            ! =====================================

            ! =====================================
            subroutine hyperbolLCLU(Time,j,provider,lvl_static,lvf_PctLandCover)
                ! ------------- Types ---------------
                integer :: i,j,provider,Time
                real :: lvf_proportion
                real :: lvf_Y1,lvf_y2
                real :: lvf_PctLandCover(LULC)

             !   real,parameter :: lpf_alpha=5
              !  real,parameter :: lpf_beta=0.9
                real :: fHyperbolLCLUstatic
                logical :: lvl_static
                ! ===================================
                    !  Time = T%policyYear (zero to x)
                    !
                    if(lvl_static)then
                        lvf_proportion =0
                       lvf_proportion =  fHyperbolLCLUstatic(Time)
                      do i = 1,LULC,1
                        lvf_y2=gvf_landCover_2060(i,provider)
                        lvf_Y1=gvf_landCover_2010(i,provider)
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(i)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(i)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(i)=0
                        endif
                      end do
                    else
                          lvf_proportion =0
                         lvf_proportion =  fHyperbolLCLUstatic(Time)
                         lvf_y2=gvf_landCover_2060(j,provider)
                         lvf_Y1=gvf_landCover_2010(j,provider)
                        if(lvf_y1 < lvf_y2)then
                         lvf_PctLandCover(j)= lvf_Y1 + lvf_proportion* (lvf_y2-lvf_Y1)
                        else
                         lvf_PctLandCover(j)= lvf_Y1 - lvf_proportion* (lvf_Y1-lvf_y2)
                        endif
                        ! FIX THIS WHEN WE CAN!
                        ! 05.17.16 das
                        if(provider == 23)then
                         lvf_PctLandCover(j)=0
                        endif

                    endif
                !
              return
            end subroutine hyperbolLCLU
            ! =====================================

            ! ===================================
            subroutine sDefaultLandCover(lvf_PctLandCover)
                real :: lvf_PctLandCover(13)
                !
                lvf_PctLandCover(1)=1  ! 
                lvf_PctLandCover(2)=14 ! 
                lvf_PctLandCover(3)=1  ! 
                lvf_PctLandCover(4)=1  ! 
                lvf_PctLandCover(5)=7  ! 
                lvf_PctLandCover(6)=12 ! 
                lvf_PctLandCover(7)=6  ! 
                lvf_PctLandCover(8)=6  ! 
                lvf_PctLandCover(9)=45 ! 
                lvf_PctLandCover(10)=4 !
                lvf_PctLandCover(11)=4 !
                lvf_PctLandCover(12)=4 !
                lvf_PctLandCover(13)=0 !
                !
              return
            end subroutine sDefaultLandCover
            ! ===================================

End Module lms_LULC

       subroutine sDefaultRCN(T,i,lvf_xbarRCN,lvf_PropLandCover)
         use lms_LULC
            integer :: i,j
            real :: lvf_totalRCN,lvf_add,lvf_RCN(13)
            real :: lvf_xbarRCN(gvi_maxProV)
            real :: lvf_PropLandCover(gvi_maxProV)
            type(runTime)T
            ! ==============================================

                 lvf_RCN(1)=74 ! Ag
                 lvf_RCN(2)=89 ! building - use commercial business, soil group A
                 lvf_RCN(3)=10 ! canal - assuming some runoff
                 lvf_RCN(4)=61 ! Turf
                 lvf_RCN(5)=70 ! Greenway : Turf plus water plus impervious
                 lvf_RCN(6)=98 ! Impervious
                 lvf_RCN(7)=96 ! Mtn Vegetation
                 lvf_RCN(8)=75 ! residential - using 1/4 acre B soil group
                 lvf_RCN(9)=82 ! soil/desert - using "dirt Right-of-way" B hydrologic soil group
                 lvf_RCN(10)=68 ! tree - poor condition grass cover
                 lvf_RCN(11)=50 ! unclassified
                 lvf_RCN(12)=76 ! wash - using the "A hydrologic soil group for gravel
                 lvf_RCN(13)=0 ! water

                lvf_totalRCN=0.
                lvf_add=0.
                !
                do j = 1,LULC,1
                  lvf_PropLandCover(j)=gvf_landCover_2010(j,i)*0.01
                  lvf_add=lvf_RCN(j)*lvf_PropLandCover(j)
                  lvf_totalRCN=lvf_totalRCN+lvf_add
                  lvf_xbarRCN(i)=lvf_totalRCN
                enddo      
                !
          return
        end subroutine sDefaultRCN

    Subroutine sProviderRCN(T,i,lvf_xbarRCN)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        real :: lvf_xbarRCN(gvi_maxProV)
        ! ===============================
        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call ProviderRCN(T,i,lvf_xbarRCN)
            !
        ! 
      return
    end subroutine sProviderRCN
    ! ---------------------------------------

    ! ---------------------------------------
    Subroutine sSetLCLUdemand(T,i)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        ! ===============================
        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sSetWaterUseByLCLU(T,i)
            !
        ! 
      return
    end subroutine sSetLCLUdemand
    ! ---------------------------------------
    ! ---------------------------------------
    Subroutine sGetLCLUdemand(T,i)
     use lms_LULC
        ! ------------ types ------------
        integer :: i
        ! ===============================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sGetWaterUseByLCLU(T,i)
            !
        ! 
      return
    end subroutine sGetLCLUdemand
    ! ---------------------------------------

    ! ---------------------------------------
    subroutine sRunLCLUarea(T,i)
     use lms_LULC
        ! ------ Types -------
        integer :: i
        ! ====================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sLCLUArea(T,i)
            !
      return
     end subroutine sRunLCLUarea
    ! ---------------------------------------

    ! ---------------------------------------
    subroutine sRunLCLU(T,i)
     use lms_LULC
        ! ------ Types -------
        integer :: i
        ! ====================

        ! -- Type Construct ---
        type(runTime)T
        ! ======================
            !
            call sLCLUArea(T,i)
            call  sSetLCLUdemand(T,i)
            call  sGetLCLUdemand(T,i)
            !
      return
    end subroutine sRunLCLU
    ! ---------------------------------------
! E.O.F.