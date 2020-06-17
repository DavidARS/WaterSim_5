!
! File is Agriculture.f90
!
!   This Module models the Regional Agriculture pool and fluxes.  And, it controls
! some of the provider water balance for municipal water providers
! -----------------------------------------------------------------------------------
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
!

! Module:       Module lm_AgWaterBudgets
! Subroutines:  subroutine initAg()
!                 calls(also present):
!                   call openFiles_ag()
!                   call readFiles_ag()
!
! No Module:    subroutine initializeAgriculture()
!               subroutine sAgSurfaceWater(lvf_add)

! Module:       lms_AgWaterBudgets
! Subroutines:  subroutine  AgricultureDemand(T,i,effluent,lvf_GWtoAg,VadoseFromAg,lvf_AgEvap)
!
! No Module:    subroutine sAgDemand(T,prov,effluent,GW,Vad,Evap)
!               subroutine sAgInitialize_Water()
!

! Global OUTPUTS:  
!
! Local OUTPUTS:             
!   
! Local INPUTS:
!
! created on 05.09.12
!
! david arthur sampson

! last write was: 01.16.13,10.17.13,06.09.14,07.20.14
! ---------------------------------------------------
!

! ======================================================================================================
!
Module lm_AgWaterBudgets
 use gm_ModelControl
    use gm_GlobalData
   
    implicit none
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_5\WaterSimDCDC.txt"
    !
  contains
    !
        ! -------------------
        subroutine initAg()
          call openFiles_ag()
          call readFiles_ag()
         return
        end subroutine initAg
        ! -------------------

        ! -----------------------
        subroutine openFiles_ag()
            !
            ! --------------- Types ------------
             character(len=200) :: lvc_DPath=' '
            ! ==================================
                !
                if(gpl_release)then
                    lvc_DPath=trim(gvc_DPath)
                else
                    lvc_DPath=gvc_Path 
                endif
                !
                module="lm_AgWaterBudgets"
                !
                ! 03.13.14 DAS
                Infile='App_Data\Data\agPumpingSurface_2085.txt'; LU=90
                call openFiles(module,lvc_DPath,Infile,LU)

!                Infile='App_Data\Data\EffluentToAgBaseline.txt'; LU=91
!                call openFiles(module,lvc_DPath,Infile,LU)

                ! moved here on 03.04.14
                Infile='App_Data\Data\providerAcres_2012.txt'; LU=52
                call openFiles(module,lvc_DPath,Infile,LU)
                !
            !
         return
        end subroutine openFiles_ag
        ! ---------------------------

      ! -----------------------
      subroutine readFiles_ag()
        !
        ! ---------------------- Types -------------------------
        integer :: i,j,ios,LU
        real :: lvf_year,lvf_pumpingAg,lvf_surfaceAg,lvf_ag_CAP
        ! ======================================================
        !
            !
            LU=90
          do i = gpi_minPVY,gpi_maxPVY,1
            read(LU,*,err=10,iostat=ios)lvf_year,lvf_pumpingAg,lvf_surfaceAg,lvf_ag_CAP
             gv_AgWaterPumpingSRV(i,1)=0.
            gv_AgWaterPumpingSRV(i,1)=lvf_pumpingAg
             gv_AgWaterPumpingSRV(i,2)=0.
            gv_AgWaterPumpingSRV(i,2)=lvf_surfaceAg
             gv_AgWaterPumpingSRV(i,3)=0
            gv_AgWaterPumpingSRV(i,3)=lvf_ag_CAP
             gv_AgWaterPumpingSRV(i,4)=0
             gv_AgWaterPumpingSRV(i,5)=0
            gv_AgWaterPumpingSRV(i,6)=0
            lvf_year=lvf_year*1 ! Only used to surpress verbose warnings
          end do
10          continue
            close(LU)
            if(0 < ios)then
             goto 1000
            endif
            !
!             do i = gpi_minPVY,gpi_maxPVY,1
!              gvf_EffluentToAg_baseline(i)=0
!             end do
!            LU=91
!            read(LU,*,err=20,iostat=ios)(gvf_EffluentToAg_baseline(i),i=gpi_lBY,gpi_uBY)
!20          continue
!            close(LU)
!            if(0 < ios)then
!             goto 1000
!            endif
            !
            ! Missing "Other Provider"
            do i = 1,gvi_Providers,1
              lid_providerAcres(i,1)=0
              lid_providerAcres(i,2)=0
            end do
            LU=52
            read(LU,*,err=30,iostat=ios)((lid_providerAcres(i,j),j=1,2),i=1,gvi_maxProV)
30          continue
            close(LU)
            !
             !
            if(0 < ios)then
             goto 1000
            endif
            !
         return
1000     continue
            if(gvl_writeLog)then
              string=13
                call sStrings(string,errorString)
                call eWrite(errorString,LU)
            endif
           gvl_errorFlag=.false.
           !
        return
      end subroutine readFiles_ag
      ! -------------------------
!
End Module lm_AgWaterBudgets
!
    ! --------------------------------
    subroutine initializeAgriculture()
      use lm_AgWaterBudgets
        !
        call initAg()
        call sAgInitialize_Water()
        call sAgPumpingSES()
        call sAgSurfaceSES
        !
      return
    end subroutine initializeAgriculture
    !-----------------------------------

! ======================================================================================================
!
Module lms_AgWaterBudgets
 use gm_ModelControl
  use gm_GlobalData
    use gm_TypeControl
    !
    ! -----
    include "C:\WaterSim\FORTRAN\Include\WaterSim_5\WaterSimDCDC.txt"
    !
        real :: lmf_addAgWaterFromSurface(gvi_maxProV)
    !
 contains
    !
        ! --------------------------------------------------------------------
        subroutine AgricultureDemand(T,i,effluent,lvf_GWtoAg,VadoseFromAg,lvf_AgEvap)
            ! ---------------- Types ---------------
            integer:: i
            real :: effluent
            !
            real :: lvf_temp
            real :: GW,SW,EFF
            real :: lvf_relArea(gvi_maxProV)
            real :: lvf_GWtoAg,VadoseFromAg
            real :: lvf_AgEvap
            !real :: GWtransfers,CAPtransfers
            real :: CAPusedByAg
            real,parameter :: lpf_AgEvap=0.793
            real,parameter :: lpf_AgToVadose=0.20 
            ! =====================================
            ! - Type Constructs -
            type(runTime)T
            ! ===================
                !
                GW=0; SW=0; EFF=0; CAPusedByAg=0
                lvf_relArea(i)=0
                !
                if(T%year < gpi_lclu)then
                 ! 
                else
                    !
                    lvf_relArea(i)=gvf_relativeAgArea(i)
                    !
                  call sMaskCAP(gvl_maskCAP)
                  !
                !  GWtransfers=go_AgWaterGW_AF(i)
                !  CAPtransfers=gvf_AgCAPused_AF(i)

                    ! =================================================================
                    !  Supplies
                    !
                    ! ------- Set the Raw Data for Ag Pumping here ------
                    !
                    ! This will need changing 
                    ! 11.09.16 to perhaps gv_AgWaterPumpingSRV(T%year,4)
                     gvf_pumpingAg=0
                    gvf_pumpingAg=gv_AgWaterPumpingSRV(T%year,6)
                    ! 
                    ! ---------------------------------------------------
                    ! Send to Interface pumping
                    ! ==========================
                    go_pumpingAgTotal_AF(i)=lvf_relArea(i)*gvf_pumpingAg
                   ! go_WaterFromAgPumping_acft_a(i)= go_pumpingAgTotal_AF(i)
                    ! ---------------------------------------------------
                    !
                    !gvf_AgEfficiency(i)=1
                    !
                  !
                  GW=lvf_relArea(i)*gvf_pumpingAg * gvf_AgEfficiency(i) 
                  gvf_AgAFGWTotal(T%year,i)=GW

                  !
                   CAPusedByAg=go_AgWaterCAPavailable_AF(i) * gvf_AgEfficiency(i)
                  SW= CAPusedByAg + gvi_WaterToAgriculture_acft_a(i)
                  !
                  EFF=effluent
                  !
                  !
                  ! Send data to subroutine AgAcerageAndCredits(T,lvf_relArea)
                  ! to alter acres of agricultural lands in a provider boundary
                  ! 09.06.16
                  gvf_AgAFGWTotal(T%year+1,i)= gvf_AgAFGWTotal(T%year,i)

                  ! ===================================================================
                  ! Demands
                  !
                    lvf_temp=GW+SW+EFF
                  lvf_GWtoAg=GW
                  !
                  VadoseFromAg=lvf_temp*lpf_AgToVadose
                  !
                   lvf_AgEvap=0
                  lvf_AgEvap=lpf_AgEvap*(lvf_temp)
                  !
                  go_AgToLCLUdemand(i)=nint(lvf_temp)
                  !
                  ! =====================================================================
                  !
                 endif
                !
100 format(I4,1x,I2,1x,5(F9.2,1x))
30  format(I4,1x,I2,1x,I4,1x,6(F9.2,1x))
                !
          return
        end subroutine AgricultureDemand
        ! --------------------------------------------------------------------

        ! --------------------------------------------------------------------
        subroutine AgricultureProduction(T,effluent)

            ! ----------------- Types ---------------
            real :: GW,SW,effluent
            real :: lvf_temp
            real :: lvf_agTotalWaterUse
            real :: lvf_agProduction
            real :: lvf_AgEfficiency=1
            real :: GWtransfers !,CAPtransfers
            real :: lvf_totalCredits
            real :: lvf_temp_1,lvf_temp_2,lvf_temp_3
            ! =======================================
            !
            ! - Type Constructs -
            type(runTime)T
            ! ===================
                !
                GWtransfers=0
                !
                  if(T%year <= gvi_baseYear-1)then
                    
                    gvf_AgGroundWater_2015 =gv_AgWaterPumpingSRV(T%year,1)*lvf_AgEfficiency 
                    gvf_AgSurfaceWater_2015=gv_AgWaterPumpingSRV(T%year,5)*lvf_AgEfficiency 
                    gvf_AgEffluent_2015=effluent*lvf_AgEfficiency
                     lvf_temp=0
                    lvf_temp=gvi_Agpool_acft_a
                    gvf_ratioMaricopaAg=gv_AgWaterPumpingSRV(T%year,5)/lvf_temp
                  else
                  endif
                  !
                   if(gvi_baseYear-1 <= T%year)then
                     !
                      lvf_temp_1=0
                      lvf_temp_2=0
                      lvf_temp_3=0
                     !
                     GWtransfers=sum(go_AgWaterGW_AF)
                    ! CAPtransfers=sum(gvf_AgCAPused_AF)
                    !
                    gvf_EffluentToAg_diff(T%year)=(effluent-gvf_AgEffluent_2015)
                    !
30 format(I4,1x,3(F10.1,1x))
                    !
                    !
                    ! I had been subtracting the GW transfers from GW, but in fact, they have already been subtracted
                    ! by virtue of the curve itself, no?
                    ! 10.21.16
                    ! =======================================
                     lvf_totalCredits=gvf_AgEffluent_2015+gvf_AgSurfaceWater_2015+gvf_AgGroundWater_2015
                    !
                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=max(0, min(lvf_totalCredits-GWtransfers,gv_AgWaterPumpingSRV(T%year,6)- gvf_EffluentToAg_diff(T%year)))
                     SW= max(0, min(gvf_ratioMaricopaAg*gvi_Agpool_acft_a,gv_AgWaterPumpingSRV(T%year,5)))
                     !
                     ! 11.16.16 DAS moved here.....
                    ! go_WaterFromAgPumping_acft_a(i)=GW
                     !
                      gvf_AgWaterTotals(1)=GW
                      gvf_AgWaterTotals(2)=SW
                      gvf_AgWaterTotals(3)=effluent
                   !
                       lvf_temp=0
                      lvf_temp_1=gvf_AgWaterTotals(1)+gvf_AgWaterTotals(2)+ gvf_AgWaterTotals(3)
                      lvf_temp_2=gvf_AgGroundWater_2015+gvf_AgSurfaceWater_2015+gvf_AgEffluent_2015
                      !
                      if(0 < lvf_temp_2)lvf_temp_3=lvf_temp_1/lvf_temp_2
                         !
                        if( 0 < go_AgEfficiency)lvf_AgEfficiency=go_AgEfficiency*0.01
                        !
                         lvf_agTotalWaterUse=0
                        lvf_agTotalWaterUse=GW+SW+effluent
                         lvf_agProduction=0
                        lvf_agProduction=lvf_agTotalWaterUse*lvf_AgEfficiency

                     go_AgWaterUsedByMuni_PCT_a=nint(lvf_temp_3*100)

                     lvf_temp=0
                    lvf_temp=go_AgWaterUsedByMuni_PCT_a
                   else
                    !
                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=gv_AgWaterPumpingSRV(T%year,6)
                     SW= max(0, min(gvf_ratioMaricopaAg*gvi_Agpool_acft_a,gv_AgWaterPumpingSRV(T%year,5)))
                     !
                      gvf_AgWaterTotals(1)=GW
                      gvf_AgWaterTotals(2)=SW
                      gvf_AgWaterTotals(3)=effluent

                      lvf_temp_1=0
                      lvf_temp_2=0
                      lvf_temp_3=0
                    !
                    go_AgWaterUsedByMuni_PCT_a=100
                    lvf_temp=go_AgWaterUsedByMuni_PCT_a
                    !
                   endif
                    !
10   format(I4,1x,I4)
100  format(I4,1x,2(F9.0,1x))
1000 format(I4,1x,5(F9.0,1x))

          return
        end subroutine  
        ! --------------------------------------------------------------------

       ! --------------------------------------------------------------------
        subroutine AgriculturalProduction(T,i,effluent,GW,lvf_AgToVadose,lvf_AgEvap)

            ! ----------------- Types ---------------
            integer :: i
            real :: GW,SW,effluent
            real :: lvf_temp
            real :: lvf_AgEfficiency=1
            real :: GWtransfers !,CAPtransfers
            real :: lvf_totalCredits
            real :: lvf_AgToVadose,lvf_AgEvap
            real :: lvf_EffluentToAg_diff
            real,parameter :: lpf_AgEvap=0.793
            real,parameter :: lpf_AgToVadose=0.20 
            ! =======================================
            !
            ! - Type Constructs -
            type(runTime)T
            ! ===================
                !
                lvf_temp=0
                gvf_AgGroundWater_2015 =0
                gvf_AgSurfaceWater_2015=0
                gvf_AgEffluent_2015=0
                !
                  if(T%year <= gvi_baseYear-1)then
                    
                    gvf_AgGroundWater_2015 =gv_AgWaterPumpingSRV(T%year,1)*lvf_AgEfficiency *gvf_AgCreditWeight(i)
                    gvf_AgSurfaceWater_2015=gv_AgWaterPumpingSRV(T%year,5)*lvf_AgEfficiency *gvf_AgCreditWeight(i)
                    gvf_AgEffluent_2015=effluent*lvf_AgEfficiency*gvf_AgCreditWeight(i)
                 !    lvf_temp=0
                 !   lvf_temp=gvi_Agpool_acft_a *gvf_AgCreditWeight(i)
                 !   gvf_ratioMaricopaAg=gv_AgWaterPumpingSRV(T%year,5)*gvf_AgCreditWeight(i)/lvf_temp
                  else
                  endif
                  !
                   if(gvi_baseYear-1 <= T%year)then
                     !
                      GWtransfers=0
                     GWtransfers=go_AgWaterGW_AF(i)
                    ! CAPtransfers=sum(gvf_AgCAPused_AF)
                    !
                     lvf_EffluentToAg_diff=0
                    lvf_EffluentToAg_diff=(effluent-gvf_AgEffluent_2015)
                    !
30 format(I4,1x,3(F10.1,1x))
                    !
                    !
                    ! I had been subtracting the GW transfers from GW, but in fact, they have already been subtracted
                    ! by virtue of the curve itself, no?
                    ! 10.21.16
                    ! =======================================
                     lvf_totalCredits=gvf_AgEffluent_2015+gvf_AgSurfaceWater_2015+gvf_AgGroundWater_2015
                    !
                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=max(0, min(lvf_totalCredits-GWtransfers,gvf_AgCreditWeight(i)*gv_AgWaterPumpingSRV(T%year,6)- lvf_EffluentToAg_diff))
                     SW= max(0, min(gvf_AgCreditWeight(i)*gvf_ratioMaricopaAg*gvi_Agpool_acft_a,gvf_AgCreditWeight(i)*gv_AgWaterPumpingSRV(T%year,5)))
                     !
                   else
                    !
                    ! gv_AgWaterPumpingSRV(T%year,6) is the user defined pumping by Ag (or, credit transfer difference)
                     GW=gv_AgWaterPumpingSRV(T%year,6)*gvf_AgCreditWeight(i)
                     SW= max(0, min(gvf_AgCreditWeight(i)*gvf_ratioMaricopaAg*gvi_Agpool_acft_a,gvf_AgCreditWeight(i)*gv_AgWaterPumpingSRV(T%year,5)))
                    !
                   endif
                    !
                  ! Demands
                  !
                    lvf_temp=GW+SW+effluent
                  !
                  lvf_AgToVadose=lvf_temp*lpf_AgToVadose
                  !
                   lvf_AgEvap=0
                  lvf_AgEvap=lpf_AgEvap*(lvf_temp)
                  !
10   format(I4,1x,I4)
100  format(I4,1x,2(F9.0,1x))
1000 format(I4,1x,5(F9.0,1x))

          return
        end subroutine AgriculturalProduction
        ! --------------------------------------------------------------------

! 
End Module lms_AgWaterBudgets
!
    ! ------------------------------
    subroutine sAgDemand(T,prov,effluent,GW,Vad,Evap)
      use lms_AgWaterBudgets
        ! ----- Types -------
        integer:: prov
        real :: effluent,GW,Vad
        real :: Evap
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgricultureDemand(T,prov,effluent,GW,Vad,Evap)
            !
      return
    end subroutine sAgDemand
    ! ------------------------------

    ! ------------------------------
    subroutine sAgProductionRunning(T,i,effluent,AgGW,Tovadose,lvf_toEvapoFromAg)
      use lms_AgWaterBudgets
        ! ----- Types -------
        integer :: i
        real :: effluent
        real :: AgGW,Tovadose
        real :: lvf_toEvapoFromAg
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgriculturalProduction(T,i,effluent,AgGW,Tovadose,lvf_toEvapoFromAg)
            !
      return
    end subroutine sAgProductionRunning
    ! ------------------------------

   ! ------------------------------
    subroutine sAgProductionStatic(T,effluent)
      use lms_AgWaterBudgets
        ! ----- Types -------
        real :: effluent
        ! ===================
        ! - Type Constructs -
        type(runTime)T
        ! ===================
            !
            call AgricultureProduction(T,effluent)
            !
      return
    end subroutine sAgProductionStatic
    ! ------------------------------




    ! ------------------------------
    subroutine sAgInitialize_Water()
        use lm_AgWaterBudgets
            ! ------------------------- Types -------------------------
            integer :: i,j,k

            real :: lvf_RelativeAreaGW(gvi_maxProV)! m2
            real :: lvf_AreaAgSurface(gvi_maxProV)
            ! ===============================================================
            !
            ! Start of first provider with Ag acreage
            ! -------------------------------------------------------------------------------------------------------------------
            ! 

            ! Groundwater Ag
            ! ----------------------------
            do i = 1,gvi_Providers,1
                if(i == 1)then
                 do k = 1,gvi_Providers,1
                  lvf_RelativeAreaGW(k)=0
                 end do
                 !
                 ! Extract area of a provider that has Agriculture
                 ! -------------------------------------------------
                  gvf_totalAreaAg=0
                 call sMaskAgriculture(gvl_mask)
                 do j = 1,gvi_Providers,1
                  gvf_AreaAg(j)=0
                !
                  if(gvl_mask(j))then
                   call ProviderAreaAg_2012(j,gvf_AreaAg)
                   gvf_totalAreaAg=gvf_totalAreaAg+gvf_AreaAg(j)
                  endif
                 end do
                !
                ! Total, regional Ag pumping from ADWR
                ! Read in from a text file above
                ! --------------
                 gvf_pumpingAg=0
                gvf_pumpingAg=gv_AgWaterPumpingSRV(2000,1)
                !
                gvf_AG_Banking(2000)=0
                !
              endif
            !
            ! ------------------------------------------------------------------------------
            !
            !  Relative area is at the Provider area. Has NOTHING to do with how
            ! much agriculture acres they have. I will improve this as the model
            ! develops. m2 at the point. 
            if( 0 < gvf_totalAreaAg)lvf_RelativeAreaGW(i)=gvf_AreaAg(i)/gvf_totalAreaAg
            !
               ! 05.08.12
                ! Do I limit the request for water from Ag in the interface, or do I set it here and report 
                ! back the result? 01.09.14
                !
                 gvi_defaultAgPumping_acft_a(i)=0
                gvi_defaultAgPumping_acft_a(i)=nint(lvf_RelativeAreaGW(i)*gvf_pumpingAg)
                !
            ! ==============================================================================================================
            !
            ! Surface Ag
            !
            ! ----------------------------
            if(i == 1)then
                 !
                 ! Extract area of a provider that has Agriculture
                ! -------------------------------------------------
                 gvf_totalAreaAgSURF=0
                 call sMaskAgriculture(gvl_mask)
                !
                do j = 1,gvi_Providers,1
                  lvf_AreaAgSurface(j)=0
                !
                  if(gvl_mask(j))then
                   call ProviderAreaAg_2012(j,lvf_AreaAgSurface)
                    gvf_totalAreaAgSURF=gvf_totalAreaAgSURF+lvf_AreaAgSurface(j)

                  endif
                end do
                !
                do k = 1,gvi_Providers,1
                  gvf_relativeAreaAg(k)=lvf_AreaAgSurface(k)/gvf_totalAreaAgSURF
                end do
            endif

            !
         end do
      return
    end subroutine sAgInitialize_Water
    ! --------------------------------

    ! 01.28.15 DAS
    ! estimates the alpha parameter of 
    ! the SES algorithm to model new
    ! Ag pumping based in UI inputs. Used
    ! in subroutine sAgGroundWater(T,lvf_creditTransfer)
    ! found in GlobalSubroutines.f90 (~ line 396)
    ! --------------------------------------------------
    subroutine sAgPumpingSES()
        use lm_AgWaterBudgets
        !
        ! ------------ Types --------------
        integer:: i,j

        real:: IN
        real:: fSES_alpha
        real:: lvf_Alter
        real:: pumping(7),targetArray(30)
        real:: lvf_target,Nalpha, alphaArray(30)    
        real,parameter:: lpf_modAlpha=1.004
        real,parameter:: lpf_minAlpha=0.2
        real,parameter:: lpf_maxAlpha=0.99

        logical :: lvl_error
        ! ================================        
           !
            IN=2010
            lvl_error=.false.
            pumping(1) = gv_AgWaterPumpingSRV(2014,1)
            pumping(2) = gv_AgWaterPumpingSRV(2013,1)
            pumping(3) = gv_AgWaterPumpingSRV(2012,1)
            pumping(4) = gv_AgWaterPumpingSRV(2011,1)
            pumping(5) = gv_AgWaterPumpingSRV(2010,1)
            pumping(6) = gv_AgWaterPumpingSRV(2009,1)
            pumping(7) = gv_AgWaterPumpingSRV(2008,1)
            !
            ! 12088 AF each step; array(3) is odd, but needed.
            !
            targetArray(30)=0
            targetArray(29)=7
            targetArray(28)=12095
            targetArray(27)=24183
            targetArray(26)=36271
            targetArray(25)=48359
            targetArray(24)=60447
            targetArray(23)=72535
            targetArray(22)=84623
            targetArray(21)=96711
            targetArray(20)=108799
            !
            targetArray(19)=120887 ! ~50 % reduction
            targetArray(18)=132975
            targetArray(17)=145063 ! 40% reduction from ADWR pumping estimate
            targetArray(16)=157151
            targetArray(15)=169240
            targetArray(14)=181328
            targetArray(13)=193417
            targetArray(12)=205505
            targetArray(11)=217594
            targetArray(10)=229682

            targetArray(9)=241771 ! this is the approximate ADWR pumping estimate

            targetArray(8)=253860
            targetArray(7)=265948
            targetArray(6)=278037
            targetArray(5)=290125
            targetArray(4)=302214
            targetArray(3)=314302
            targetArray(2)=326391
            targetArray(1)=338479 ! 5% reduction from flat response
            !                       0% reduction is a flat response
             Nalpha=0.99
            !
            do i = 1,30,1
             lvf_Alter=i*4.0 !3.9
             lvf_target= targetArray(i)
             Nalpha= max(lpf_minAlpha,min(fSES_alpha(IN,lvf_Alter,lvf_target,pumping,lvl_error),lpf_maxAlpha))
             alphaArray(i)=Nalpha
            end do
            do j = 1,30,1
             gvf_Ag_alphaPump(j)=alphaArray(j)
             if(28 < j )gvf_Ag_alphaPump(j)=0.4399
             if(29 < j )gvf_Ag_alphaPump(j)=0.35
             ! ModAlpha brings pumping in line with UI controls
             ! i.e., 50% transfer is 50% of threshold by 2085
             gvf_Ag_alphaPump(j)=gvf_Ag_alphaPump(j)*lpf_modAlpha
             !
            end do
      return
    end subroutine sAgPumpingSES
    ! --------------------------

    ! --------------------------
   subroutine sAgSurfaceSES()
        use lm_AgWaterBudgets
        !
        ! ------------ Types -------------
        integer:: i,j

        real:: Nalpha, alphaArray(20)      
        real:: lvf_AlterPumping
        real:: alpha_t
        real, parameter :: a=90.5916
        real, parameter :: b= 0.9969
        real, parameter :: c=-0.0422
        real,parameter:: lpf_minAlpha=0.2
        real,parameter:: lpf_maxAlpha=0.99
        ! ================================        
           !
            do i = 1,20,1
              lvf_AlterPumping=i*20
                alpha_t=90
                if(0 < abs(lvf_AlterPumping))then
                alpha_t=(a*b**abs(lvf_AlterPumping) * abs(lvf_AlterPumping)**c )
                endif
                Nalpha=max(lpf_minAlpha,min(alpha_t*1/100,lpf_maxAlpha))
             alphaArray(i)=Nalpha
            end do
            do j = 1,20,1
             gvf_Ag_alphaSurfOther(j)=alphaArray(j)
            end do
      return
    end subroutine sAgSurfaceSES
!
! =====================================================================================================================
!E.O.F. - Agriculture.f90
