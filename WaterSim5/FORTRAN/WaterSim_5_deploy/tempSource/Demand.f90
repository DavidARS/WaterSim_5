!
! File is Demand.f90
!
! This file controls added demand for the FORTRAN model
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
! No Modules:   subroutine pRainWaterHarvest(T,gvi_order,lvl_flag)  
!               subroutine pWaterBanking_k(T,gvi_order,interrupt)
!               subroutine pWaterForDirectInject_s(T,gvi_order,lvl_flag)
!               subroutine pWaterReclaimed_s(T,gvi_order)
!               subroutine pROWaterReclaimed_s(T,gvi_order)
!               subroutine pWaterForVadose_s(T,gvi_order,lvl_flag)
!               subroutine sCWaccounting(gvi_order,lvf_in_1,lvf_in_2,code)
!

! created on 01.16.13
!
! david arthur sampson

! last write was: 04.01.13,07.18.14
! ----------------------------------
!

! ======================================================================================================

    ! --------------------------
    subroutine pStartUp_k(T)
      use lms_ParameterControl
       use gm_ModelControl
        !  
        ! - Type Constructs -
        type(runTime)T
        ! ===================
        !
        !
      return
    end subroutine pStartUp_k
    ! -------------------------

    ! -------------------------------------------
    subroutine sDemandWrap(i,gvi_order)
     use lms_CitiWaterBudgets
       use gm_ModelControl

        ! -------- types --------
        integer :: i
        integer(1) :: gvi_order
        integer :: code=16
        ! =======================
            ! 
            ! What is this?
          !
          gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) 
          gvf_WaterDemand_acft(i,gvi_order,2)=  gvf_WaterDemand_acft(i,gvi_order,2)
          gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)
          !
          gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) 
          gvf_WaterDemand_acft(i,gvi_order,8)=  gvf_WaterDemand_acft(i,gvi_order,8)
          gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9)
          gvf_WaterDemand_acft(i,gvi_order,10)=  gvf_WaterDemand_acft(i,gvi_order,10)
          ! -------------------------------------------------------------------------
          ! Move the demand to the next level- i.e. none added or removed on this step
          gvf_WaterDemand_acft(i,gvi_order+1,1)=  gvf_WaterDemand_acft(i,gvi_order,1) 
          gvf_WaterDemand_acft(i,gvi_order+1,2)=  gvf_WaterDemand_acft(i,gvi_order,2)
          gvf_WaterDemand_acft(i,gvi_order+1,3)=  gvf_WaterDemand_acft(i,gvi_order,3) 
          !
          gvf_WaterDemand_acft(i,gvi_order,6)=code
          !
          gvf_WaterDemand_acft(i,gvi_order+1,7)=  gvf_WaterDemand_acft(i,gvi_order,7) 
          gvf_WaterDemand_acft(i,gvi_order+1,8)=  gvf_WaterDemand_acft(i,gvi_order,8)
          gvf_WaterDemand_acft(i,gvi_order+1,9)=  gvf_WaterDemand_acft(i,gvi_order,9) 
          gvf_WaterDemand_acft(i,gvi_order+1,10)= gvf_WaterDemand_acft(i,gvi_order,10) 
        !
      return
    end subroutine sDemandWrap
    ! -------------------------

    ! ------------------------------------------------
    subroutine sOutdoorAccounting(i,gvi_order,lvf_add)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        ! -------- Types --------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_propDemand
        real :: lvf_add
        real :: lvf_outdoorDemand
        ! =======================
            !
             lvf_propDemand=0
             lvf_outdoorDemand=0
            lvf_outdoorDemand=gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8)
            if(0 < lvf_outdoorDemand)then
               lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,10)/ &
                 (gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8))
                !
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10) &
                    + lvf_propDemand*lvf_add
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8) &
                    + ((1-lvf_propDemand)*lvf_add)
                !
            else
              if(0 < gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1))then
               lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3)/ gvf_WaterDemand_acft(i,gvi_order,1)
                !
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10) &
                    + lvf_propDemand*lvf_add
                 gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8) &
                    + ((1-lvf_propDemand)*lvf_add)
                !
              endif
            endif
            !
      return
    end subroutine sOutdoorAccounting
    ! -------------------------------------------
    subroutine pGrayWater(T,gvi_order,lvl_flag)
     use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_usedGrayWater(gvi_maxProV)
         real :: code=gpi_grayWater

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ===========================
          do i = 1,gvi_Providers,1
             lvf_usedGrayWater(i)=0
             !
           if(lvl_flag(i))then
           else
               !
                 call sDemandWrap(i,gvi_order)
               !
           endif
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
          end do
            !
             gvl_InOutDemandArray=.true.
            call sCWaccountingIndoorOut(gvi_order,code,lvf_usedGrayWater,gvl_InOutDemandArray)
          !
          ! ===================================================================================
            !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=17
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
      return
    end subroutine pGrayWater
    ! -------------------------------------------

    ! ---------------------------------------------------
     subroutine pStormWaterHarvest(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_harvesting(gvi_maxProV)
         real :: code=gpi_stormWaterCapture

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ================================
           do i = 1,gvi_Providers,1
             lvf_harvesting(i)=0
             !
            if(lvl_flag(i))then

            else
                !
                 call sDemandWrap(i,gvi_order)
                !
            endif
            !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
           end do
           !         
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_harvesting,gvl_InOutDemandArray)
            !
          !
          ! =================================================================================

            gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=7
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pStormWaterHarvest
    ! --------------------------------

    ! --------------------------------------------
     subroutine pRainWaterHarvest(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! -------------------- Types ---------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_harvesting(gvi_maxProV)
         real :: code=gpi_rainWaterHarvest

         logical :: lvl_flag(gvi_maxProV)
        ! ============================================================

        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! ================================
           do i = 1,gvi_Providers,1
             lvf_harvesting(i)=0
             !
            if(lvl_flag(i))then

            else
                !
                !
                 call sDemandWrap(i,gvi_order)
                !
            endif
            !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
           end do
            !
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_harvesting,gvl_InOutDemandArray)
            !
          !
          ! =================================================================================

            gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=7
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pRainWaterHarvest
    ! ------------------------
    
    ! -----------------------------------------------
    subroutine pWaterBanking_k(T,gvi_order,interrupt)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! --------------------------------- Types --------------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_Banking(gvi_maxProV)
         real :: code=gpi_banking

         logical :: interrupt
        ! ========================================================================
        !
    
        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          ! Note: only annual CityModel time step covered here.  Will need to change        
            do i = 1,gvi_Providers,1
              lvf_Banking(i)=0
              gvf_GW_Banking(T%year,i,1)=0
             !
              if(interrupt)then
                gvf_WBankingBalance(T%year,i,1)= gvf_GW_Banking_Hold(i)
              endif
            ! 10.14.13
            ! 
             if(0 < gvf_WBankingBalance(T%year,i,1))then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                    if(gvf_WaterDemand_acft(i,gvi_order,1) < gvf_WBankingBalance(T%year,i,1))then
                      lvf_Banking(i)=gvf_WaterDemand_acft(i,gvi_order,1)
                      gvf_WBankingBalance(T%year,i,1)= max(0,gvf_WBankingBalance(T%year,i,1)-lvf_Banking(i))
                    else
                      lvf_Banking(i)=max(0,gvf_WBankingBalance(T%year,i,1))
                      gvf_WBankingBalance(T%year,i,1)=0
                    endif     
                    ! Used in subroutine sSurfaceWater
                    gvf_GW_Banking(T%year,i,1)=lvf_Banking(i)                             
                endif
              !
              else
                lvf_Banking(i)=0
              endif
                !
                ! 10.16.14 DAS added to account for banked water in CAP used (if SRP is added, must adjust)
                ! NOTE: multi-year banked (possible), so could exceed the annual CAP designation.....!!!!
                 gvf_usedBankedWater_CAP(i)=0
                gvf_usedBankedWater_CAP(i)=lvf_Banking(i)
                !
            end do
!            !
!              call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
               gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_Banking,gvl_InOutDemandArray)

          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=17
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
        return
    end subroutine pWaterBanking_k
    ! ----------------------------

    ! ------------------------------------------------------
    subroutine pWaterForDirectInject_s(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------- Types ---------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_DirectInject,lvf_add,lvf_proportion
         real :: lvf_addIN_on,lvf_addIN_off
         real :: lvf_indoor_on,lvf_indoor_off

         real :: code=gpi_directInject

         logical :: lvl_flag(gvi_maxProV)
        ! ===============================================

        ! - TYpe Construct -
        type(runTime)T
        ! ==================
          !          
          ! ===========================
          !  This subroutine MUST be called first, as coded- gvf_GW_Banking(T%year,i,vTstep)
          ! -----------------------
          call sMaskCAP(gvl_maskCAP)
          !              
          do i = 1,gvi_Providers,1
            lvf_DirectInject=0
            lvf_proportion=0
            !
            if(gvl_maskCAP(i))then

              if(lvl_flag(i))then

              else
                  if(gvi_baseYear <= T%year)then
                   lvf_DirectInject=gvf_parm_WStoDIamount(i)
                  endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                  lvf_proportion= gvf_WaterDemand_acft(i,gvi_order,2) * (1./gvf_WaterDemand_acft(i,gvi_order,1))
                endif
                  lvf_add=0
                  lvf_add= lvf_DirectInject * lvf_proportion
                  !
                    lvf_indoor_on=0
                    lvf_indoor_off=0
                    lvf_addIN_on=0
                    lvf_addIN_off=0
                   lvf_addIN_off=gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8)
                   lvf_addIN_on=gvf_WaterDemand_acft(i,gvi_order,9)+ gvf_WaterDemand_acft(i,gvi_order,10)
                   if(0 < lvf_addIN_off)lvf_indoor_off= gvf_WaterDemand_acft(i,gvi_order,7)/lvf_addIN_off
                   if(0 < lvf_addIN_on)lvf_indoor_on= gvf_WaterDemand_acft(i,gvi_order,9)/lvf_addIN_on
                   !
                gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_DirectInject            
                if(0 <  gvf_WaterDemand_acft(i,1,3))then
                  gvf_WaterDemand_acft(i,gvi_order,2)= anint(gvf_WaterDemand_acft(i,gvi_order,2)+ lvf_add)
                  gvf_WaterDemand_acft(i,gvi_order,3)= gvf_WaterDemand_acft(i,gvi_order,3)+ anint(lvf_DirectInject -lvf_add)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9) + lvf_indoor_on     * (lvf_DirectInject* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,10)= gvf_WaterDemand_acft(i,gvi_order,10) + (1-lvf_indoor_on)* (lvf_DirectInject* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,7)= gvf_WaterDemand_acft(i,gvi_order,7)+  lvf_indoor_off     * (lvf_DirectInject* lvf_proportion )
                    gvf_WaterDemand_acft(i,gvi_order,8)= gvf_WaterDemand_acft(i,gvi_order,8)+  (1-lvf_indoor_off) * (lvf_DirectInject* lvf_proportion )
                    !
                else
                  gvf_WaterDemand_acft(i,gvi_order,2)= gvf_WaterDemand_acft(i,gvi_order,2)+ lvf_DirectInject
                  gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)+ 0
                    !
                    gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) +  lvf_indoor_off * lvf_DirectInject
                    gvf_WaterDemand_acft(i,gvi_order,8)=   gvf_WaterDemand_acft(i,gvi_order,8) + lvf_DirectInject - (lvf_indoor_off * lvf_DirectInject)
                    !
                endif
              endif
            endif
              !         
                  !
                    call sDemandWrap(i,gvi_order)
                  !
            gvf_WaterDemand_acft(i,gvi_order,6)=code
            !
          end do
          !
         gvi_order=gvi_order+1
         ! 
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=9
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !     
      return
    end subroutine pWaterForDirectInject_s
    ! -------------------------------------

    ! ---------------------------------------
    subroutine pWaterReclaimed_s(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------- Types -----------------------------
        integer :: i
        integer(1) :: gvi_order

        real :: lvf_reclaimed(gvi_maxProV)
        real :: lvf_totalRecDemand
        real :: code=gpi_reclaimed
        real :: lvf_demand
        ! =======================================================
        !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
        do i = 1,gvi_Providers,1      
           !
           lvf_reclaimed(i)=0
           lvf_demand=0
            lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
            lvf_totalRecDemand=0
           !
          if(0 < lvf_demand)then
            if(gpi_timeStepWB /= 1)then
            else
              !
                  ! We have default settings for reclaimed water - No baseYear determination of use
                  ! From the previous time-step
                  ! 04.18.16
                  ! -----------------------------------------------------------------------------------------
                if(2000 < T%year)then
                  lvf_totalRecDemand=gvf_WaterDemand_acft(i,gvi_order,8)+gvf_WaterDemand_acft(i,gvi_order,10)
                    if(0 < gvf_reclaimedOutput(T%year-1,i,1))then
                        if(gvf_reclaimedInputMax(T%year,i) < gvf_reclaimedOutput(T%year-1,i,1))then
                         gvf_reclaimedInput(T%year,i,1)=gvf_reclaimedInputMax(T%year,i)
                        else
                         gvf_reclaimedInput(T%year,i,1)= gvf_reclaimedOutput(T%year-1,i,1)
                        endif  
                      !
                      ! Check to see if there is outdoor demand for use
                      ! of reclaimed water. If no demand, goes to unused pool
                      ! 04.18.16
                      ! -----------------------------------------------------           
                        lvf_reclaimed(i)= gvf_reclaimedInput(T%year,i,1)
                      !
                      if( 0 < lvf_totalRecDemand)then

                        if(lvf_reclaimed(i) <= lvf_totalRecDemand)then
                        else
                           lvf_reclaimed(i)=lvf_totalRecDemand
                            gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                            + (lvf_reclaimed(i) - lvf_totalRecDemand)
                          !
                          call sOutdoorAccounting(i,gvi_order,(lvf_reclaimed(i) - lvf_totalRecDemand))
                          !
                        endif
                      else
                        lvf_reclaimed(i)=0
                        gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)= gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1) &
                            + gvf_reclaimedInput(T%year,i,1)
                          !
                          call sOutdoorAccounting(i,gvi_order,gvf_reclaimedInput(T%year,i,1))
                          !
                      endif
                      !
                    else
                    endif

                  !-------------------------------------------------------------------
                else
                    ! Initialize
                   if(0 < gvf_reclaimedOutput(T%year,i,1))then
                        if(gvf_reclaimedInputMax(T%year,i) < gvf_reclaimedOutput(T%year,i,1))then
                         gvf_reclaimedInput(T%year,i,1)=gvf_reclaimedInputMax(T%year,i)
                        else
                         gvf_reclaimedInput(T%year,i,1)= gvf_reclaimedOutput(T%year,i,1)
                        endif                
                     lvf_reclaimed(i)=gvf_reclaimedInput(T%year,i,1)
                    else
                    endif
                endif
              !
            endif
           !
          else
          endif
        end do
        !
!               call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.true.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_reclaimed,gvl_InOutDemandArray)
 
        !
        gvi_order=gvi_order+1
        !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=10
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
        !
      return
    end subroutine pWaterReclaimed_s
    ! -------------------------------

    ! ---------------------------------------------
    subroutine pROWaterReclaimed_s(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------------- Types -------------------------
        integer :: i,j
        integer(1) :: gvi_order

        real :: lvf_ROreclaimed(gvi_maxProV)

        real :: code=gpi_roReclaimed
        real :: lvf_demand
        ! =========================================================
        !
        ! - Type Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          do i = 1,gvi_Providers,1
           lvf_ROreclaimed(i)=0
           lvf_demand=0
           !lvf_propDemand=0
           lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
           !
           if(0 < lvf_demand)then
            if(gpi_timeStepWB /= 1)then
              do j = 1,12,1
!                lvf_ROreclaimed(i)=lvf_ROreclaimed(i)+gvf_ROreclaimedInput(T%year-1,i,j)
              end do
            else
                if(2000 < T%year)then
                  if(gvi_baseYear <= T%year)then
                    if(0 < gvf_ROreclaimedOutput(T%year-1,i,1))then
                        lvf_ROreclaimed(i)=gvf_ROreclaimedInput(T%year,i,1)
                    endif
                  endif
                else
                    if(0 < gvf_ROreclaimedOutput(T%year,i,1))then
                        lvf_ROreclaimed(i)=gvf_ROreclaimedInput(T%year,i,1)
                    endif
                endif          
            endif
            !
           else
           endif
          end do
          !
!               call sCWaccounting(gvi_order,lvf_off,lvf_on,code)
                gvl_InOutDemandArray=.false.
              call sCWaccountingIndoorOut(gvi_order,code,lvf_ROreclaimed,gvl_InOutDemandArray)

          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=11
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
     return
    end subroutine pROWaterReclaimed_s
    ! ---------------------------

    ! --------------------------------------------------
     subroutine pWaterForVadose_s(T,gvi_order,lvl_flag)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! ------------------------- Types ----------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_Vadose(gvi_maxProV),lvf_add,lvf_proportion
         real :: lvf_addIN_on,lvf_addIN_off
         real :: lvf_indoor_on,lvf_indoor_off
         real :: code=gpi_vadose

         logical :: lvl_flag(gvi_maxProV)
        ! ======================================================

        ! - Type COnstructs -
        type(runTime)T
        ! ===================
          !
          call sMaskCAP(gvl_maskCAP)
          !
          ! ===========================
          if(gvl_start)then
            do i = 1,gvi_Providers,1
              lvl_flag(i)=.false.
              gvl_parm_shortage(i)=.false.
            end do
          endif
          do i = 1,gvi_Providers,1
            !
            lvf_proportion=0
            !
            if(gvl_maskCAP(i))then
              if(lvl_flag(i))then
                lvf_Vadose(i)=0
                !
              else
                !
                lvf_Vadose(i)=0
                
                ! Add to the demand so that I can remove later
                ! ---------------------------------
                  if(gvi_baseYear <= T%year)then
                    lvf_Vadose(i)= gvf_parm_SWtoVadoseAmt(i) 
                  endif
                  !
                  if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
                    lvf_proportion= gvf_WaterDemand_acft(i,gvi_order,2)/gvf_WaterDemand_acft(i,gvi_order,1) 
                  endif

                  lvf_add=0
                  lvf_add=lvf_Vadose(i)*lvf_proportion
                  !
                    lvf_indoor_on=0
                    lvf_indoor_off=0
                    lvf_addIN_on=0
                    lvf_addIN_off=0
                   lvf_addIN_off=gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8)
                   lvf_addIN_on=gvf_WaterDemand_acft(i,gvi_order,9)+ gvf_WaterDemand_acft(i,gvi_order,10)
                   if(0 < lvf_addIN_off)lvf_indoor_off= gvf_WaterDemand_acft(i,gvi_order,7)/lvf_addIN_off
                   if(0 < lvf_addIN_on)lvf_indoor_on= gvf_WaterDemand_acft(i,gvi_order,9)/lvf_addIN_on


                  if(0 <  gvf_WaterDemand_acft(i,1,3))then
                    gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,2)=  anint(gvf_WaterDemand_acft(i,gvi_order,2) + lvf_add)
                    gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3) + anint(lvf_Vadose(i)-lvf_add)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,9)=  gvf_WaterDemand_acft(i,gvi_order,9) + lvf_indoor_on     * (lvf_Vadose(i)* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,10)= gvf_WaterDemand_acft(i,gvi_order,10) + (1-lvf_indoor_on)* (lvf_Vadose(i)* (1-lvf_proportion ))
                    gvf_WaterDemand_acft(i,gvi_order,7)= gvf_WaterDemand_acft(i,gvi_order,7)+  lvf_indoor_off     * (lvf_Vadose(i)* lvf_proportion )
                    gvf_WaterDemand_acft(i,gvi_order,8)= gvf_WaterDemand_acft(i,gvi_order,8)+  (1-lvf_indoor_off) * (lvf_Vadose(i)* lvf_proportion )
                    !
                  else
                    gvf_WaterDemand_acft(i,gvi_order,1)=  gvf_WaterDemand_acft(i,gvi_order,1) + lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,2)=  gvf_WaterDemand_acft(i,gvi_order,2) + lvf_Vadose(i) 
                    gvf_WaterDemand_acft(i,gvi_order,3)=  gvf_WaterDemand_acft(i,gvi_order,3)
                    !
                    gvf_WaterDemand_acft(i,gvi_order,7)=  gvf_WaterDemand_acft(i,gvi_order,7) +  lvf_indoor_off * lvf_Vadose(i)
                    gvf_WaterDemand_acft(i,gvi_order,8)=   gvf_WaterDemand_acft(i,gvi_order,8) + lvf_Vadose(i) - (lvf_indoor_off * lvf_Vadose(i))
                    !
                  endif

                endif
                !
              endif
                  !
                    call sDemandWrap(i,gvi_order)
                  !
               !
               gvf_WaterDemand_acft(i,gvi_order,6)=code
               !
            end do
            !
            gvi_order=gvi_order+1
            !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=8
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
          !
      return
    end subroutine pWaterForVadose_s
    ! ------------------------------

   ! ------------------------------------------
    subroutine pNewWaterSupplies_k(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !
        ! --------------------------- Types -------------------------
         integer :: i
         integer(1) :: gvi_order

         real :: lvf_NewSupplies(gvi_maxProV)
         real :: code=gpi_newSupplies
        ! ===========================================================
        !
    
        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !
          ! =======================================================
          !    Note: only annual CityModel time step covered here.  Will need to change        
            ! This output was added on 03.02.14 as water augmentation- I choose to use the
            ! output variable so that I do not need to create a new internal variable. The
            ! variable is called "go_WaterFromAgSurface_acft_a(i)"
            !
            ! 10.11.15. das- I removed Ag surface water from this routine and put it with
            ! added credits to the groundwater credit bucket in Water_CityModel.f90
            ! --------------------------------
            do i = 1,gvi_Providers,1
                if(T%atStartOfSimulation)then
                   gvf_newSuppliesBalance_acft(T%year,i)=0
                else
                  if(gvi_baseYear <= T%year)then
!                     gvf_newSuppliesBalance_acft(T%year,i)= &
!                    gvf_newSuppliesBalance_acft(T%year,i)+gvi_newWaterSupplies_acft_a(i)!+go_WaterFromAgSurface_acft_a(i)
                    ! 01.10.2016 DAS
                    if( 0 < gvi_newWaterSupplies_acft_a(i))then
                     gvf_newSuppliesBalance_acft(T%year,i)= gvi_newWaterSupplies_acft_a(i)
                    endif

                  else
                    gvf_newSuppliesBalance_acft(T%year,i)=0
                  endif
                endif
                !
                lvf_NewSupplies(i)=0
                gvf_newSuppliesUsed_acft_a(i)=0
                !
                if(0 < gvf_newSuppliesBalance_acft(T%year,i))then
                    if(gvf_newSuppliesBalance_acft(T%year,i) < gvf_WaterDemand_acft(i,gvi_order,1))then
                        !
                        lvf_NewSupplies(i)=gvf_newSuppliesBalance_acft(T%year,i)
                        !
                    else
                        !
                        lvf_NewSupplies(i)=gvf_WaterDemand_acft(i,gvi_order,1)
                        !
                    endif
                      !
                else
                    !
                    lvf_NewSupplies(i)=0
                    !
                endif
                !
                ! Used
                ! 
                 gvf_newSuppliesUsed_acft_a(i)=lvf_NewSupplies(i)
                gvf_newSuppliesBalance_acft(T%year+1,i)=gvf_newSuppliesBalance_acft(T%year,i)-lvf_NewSupplies(i)
                !
            end do
            !
               gvl_InOutDemandArray=.false.
             call sCWaccountingIndoorOut(gvi_order,code,lvf_NewSupplies,gvl_InOutDemandArray)

          !
          gvi_order=gvi_order+1
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=16
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !
        return
    end subroutine pNewWaterSupplies_k
    ! ----------------------------------

   ! --------------------------
    subroutine pCleanUp_k(T)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !  
        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !

          !
      return
    end subroutine pCleanUp_k
    ! ----------------------------------

   ! --------------------------
    subroutine pUnusedNonPotable(T,gvi_order)
      use lms_CitiWaterBudgets
       use gm_ModelControl
        !  
        !
        ! --------------------------- Types -------------------------
         integer :: i
         integer(1) :: gvi_order
         real :: code=gpi_unusedNonPotable
        ! ===========================================================

        ! - TYpe Constructs -
        type(runTime)T
        ! ===================
          !
            do i = 1,gvi_Providers,1
               gvf_WaterDemand_acft(i,gvi_order,6)=code
            end do
          !
            if(T%year .EQ. T%startyear)then
                if(gvl_writeLog)then
                    string=25
                    LU=0
                    call sOnceThrough(string,streamString)
                    call sWrite(streamString,LU)
                endif
            endif
         !
      return
    end subroutine pUnusedNonPotable
    ! ----------------------------------


    ! --------------------------------------------------------
    subroutine sCWaccounting(gvi_order,lvf_in_1,lvf_in_2,code)
     use lms_CitiWaterBudgets
        !
        ! ------------------------ Types ---------------------
        integer :: i
        integer(1) :: gvi_order

        real :: lvf_in_1(gvi_maxProV),lvf_in_2(gvi_maxProV)
        real :: code
        ! ====================================================     
          !  
          do i = 1,gvi_Providers,1
           ! in_1 is "off-project", in_2 is "on-project"
           gvf_WaterDemand_acft(i,gvi_order,4)= anint(max(0,lvf_in_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,5)= anint(max(0,lvf_in_2(i)))
           gvf_WaterDemand_acft(i,gvi_order,6)=code
           !  
              ! -------------------------------------------
               gvf_WaterDemand_acft(i,gvi_order+1,2)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,2))then
                   gvf_WaterDemand_acft(i,gvi_order+1,2)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,2)- gvf_WaterDemand_acft(i,gvi_order,4))))
              endif
              !
               gvf_WaterDemand_acft(i,gvi_order+1,3)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
                 gvf_WaterDemand_acft(i,gvi_order+1,3)= &
                  anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,3)- gvf_WaterDemand_acft(i,gvi_order,5))))
              endif
            !
               gvf_WaterDemand_acft(i,gvi_order+1,1)=0
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             gvf_WaterDemand_acft(i,gvi_order+1,1)= anint( &
                max(0,gvf_WaterDemand_acft(i,gvi_order,1) &
                - (gvf_WaterDemand_acft(i,gvi_order,4) + gvf_WaterDemand_acft(i,gvi_order,5)  ) ))
            endif
            !      
          end do
          !
        return
    end subroutine sCWaccounting
    ! --------------------------

    ! ---------------------------------------
    subroutine sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
      use lms_CitiWaterBudgets
        !
        ! ------------------- Types ------------------------
        integer :: i
        integer(1) :: gvi_order
        !real :: lvf_propDemand,lvf_propDemandOutdoor
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: lvf_in_2(gvi_maxProV),lvf_out_2(gvi_maxProV)
        real :: on,off,off_out,on_out,off_in,on_in,total
        ! ==================================================
        !
        off=0
        on=0
        off_out=0
        on_out=0
        off = gvf_WaterDemand_acft(i,gvi_order,7) + gvf_WaterDemand_acft(i,gvi_order,8)
        on = gvf_WaterDemand_acft(i,gvi_order,9) + gvf_WaterDemand_acft(i,gvi_order,10)
        total=off+on
        !
        if(0 < total)off_out= gvf_WaterDemand_acft(i,gvi_order,8)/total
        if(0 < total)on_out = gvf_WaterDemand_acft(i,gvi_order,10)/total
        if(0 < total)on_in = gvf_WaterDemand_acft(i,gvi_order,9)/total
        if(0 < total)off_in = gvf_WaterDemand_acft(i,gvi_order,7)/total

        !
         if(0 < lvf_inputs(i))then
              if(0 < on)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,9))then
                 lvf_in_2(i)=lvf_inputs(i) * on_in !  Indoor water use On
!                  lvf_in_2(i)=min(gvf_WaterDemand_acft(i,gvi_order,9), lvf_inputs(i) * on_in) !  Indoor water use On

                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,10))then
                 lvf_out_2(i)= lvf_inputs(i) * on_out
                 !lvf_out_2(i)=min(gvf_WaterDemand_acft(i,gvi_order,10), lvf_inputs(i) * on_out)   ! Outdoor water use On
                endif
              endif
              !
              if(0 < off)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,7))then
                 lvf_in_1(i)=  lvf_inputs(i) * off_in
                 !lvf_in_1(i)=  min(gvf_WaterDemand_acft(i,gvi_order,7),(lvf_inputs(i)* off_in)) !* (1-lvf_dynamic)   !  Indoor Off
                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,8))then
                 lvf_out_1(i) = lvf_inputs(i) * off_out
                 !lvf_out_1(i)=min(gvf_WaterDemand_acft(i,gvi_order,8),lvf_inputs(i) * off_out ) ! Outdoor water use Off    
                endif
              endif
         endif

        ! ------
     return
    end subroutine sOptimize
    ! ---------------------------------------

  ! ---------------------------------------
    subroutine sOptimize_OFF(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1)
      use lms_CitiWaterBudgets
        !
        ! ------------------- Types ------------------------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: off,off_in
        ! ==================================================
        !
        off_in=0
        off = gvf_WaterDemand_acft(i,gvi_order,7) + gvf_WaterDemand_acft(i,gvi_order,8)
        !
        if(0 < off)off_in = gvf_WaterDemand_acft(i,gvi_order,7)/off
        !
         if(0 < lvf_inputs(i))then
              if(0 < off)then
                if(0 < gvf_WaterDemand_acft(i,gvi_order,7))then
                 lvf_in_1(i)= min( gvf_WaterDemand_acft(i,gvi_order,7),  lvf_inputs(i) * off_in)
                endif
                if(0 < gvf_WaterDemand_acft(i,gvi_order,8))then
                 lvf_out_1(i)= min(gvf_WaterDemand_acft(i,gvi_order,8), lvf_inputs(i)  - lvf_in_1(i))
                endif
              endif
         endif

        ! ------
     return
    end subroutine sOptimize_OFF
    ! ---------------------------------------

    ! -----------------------------------------------------
    subroutine sOn_inRounding(i,gvi_order,lvf_in_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        lvf_difference=0
       ! Catch rounding errors
        if(lvf_in_2(i) < gvf_WaterDemand_acft(i,gvi_order,9))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,9)-lvf_in_2(i)
            if(lvf_difference <= 1)then
              lvf_in_2(i)=gvf_WaterDemand_acft(i,gvi_order,9)
            endif
        endif
        !
      return
    end subroutine sOn_inRounding
    ! ---------------------------------------
  ! -----------------------------------------------------
    subroutine sOn_outRounding(i,gvi_order,lvf_out_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_out_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        if(lvf_out_2(i) < gvf_WaterDemand_acft(i,gvi_order,10))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,10)-lvf_out_2(i)
            if(lvf_difference <= 1)then
              lvf_out_2(i)=gvf_WaterDemand_acft(i,gvi_order,10)
            endif
        endif
      return
    end subroutine sOn_outRounding
    ! ---------------------------------------
  ! -----------------------------------------------------
    subroutine sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_2(gvi_maxProV)  
        real :: lvf_out_2(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        ! Catch rounding errors
        if(lvf_in_2(i) < gvf_WaterDemand_acft(i,gvi_order,9))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,9)-lvf_in_2(i)
            if(lvf_difference <= 1)then
              lvf_in_2(i)=gvf_WaterDemand_acft(i,gvi_order,9)
            endif
        endif
        !
        lvf_difference=0
        if(lvf_out_2(i) < gvf_WaterDemand_acft(i,gvi_order,10))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,10)-lvf_out_2(i)
            if(lvf_difference <= 1)then
              lvf_out_2(i)=gvf_WaterDemand_acft(i,gvi_order,10)
            endif
        endif
      return
    end subroutine sOn_Rounding
    ! ---------------------------------------

  ! -----------------------------------------------------
    subroutine sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
      use lms_CitiWaterBudgets

        ! ------- Types ----------
        integer :: i
        integer(1) :: gvi_order
        real :: lvf_in_1(gvi_maxProV)  
        real :: lvf_out_1(gvi_maxProV)
        real :: lvf_difference
        ! ========================
        !
        lvf_difference=0
        ! Catch rounding errors
        if(lvf_in_1(i) < gvf_WaterDemand_acft(i,gvi_order,7))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,7)-lvf_in_1(i)
            if(lvf_difference <= 1.5)then
              lvf_in_1(i)=gvf_WaterDemand_acft(i,gvi_order,7)
            endif
        endif
        !
        lvf_difference=0
        if(lvf_out_1(i) < gvf_WaterDemand_acft(i,gvi_order,8))then
            lvf_difference=gvf_WaterDemand_acft(i,gvi_order,8)-lvf_out_1(i)
            if(lvf_difference <= 1.5)then
              lvf_out_1(i)=gvf_WaterDemand_acft(i,gvi_order,8)
            endif
        endif
      return
    end subroutine sOff_Rounding
    ! ---------------------------------------

    ! ------------------------------------------------------------------
    subroutine sCWaccountingIndoorOut(gvi_order,code,lvf_inputs,lvl_InOut)
     use lms_CitiWaterBudgets
        !
        ! ------------------------ Types ---------------------
        integer :: i,j,case_code
        integer(1) :: gvi_order
        real :: lvf_propDemandOutdoor
        real :: lvf_propDemand
        real :: lvf_inputs(gvi_maxProV)
        real :: lvf_on(gvi_maxProV), lvf_off(gvi_maxProV)
        real :: lvf_temp_IN,lvf_temp_OUT

        real :: lvf_in_1(gvi_maxProV),lvf_out_1(gvi_maxProV)
        real :: lvf_in_2(gvi_maxProV),lvf_out_2(gvi_maxProV)
        real :: lvf_demand

        real :: code

        logical :: lvl_InOut
        ! ====================================================     
        !  
        do i = 1,gvi_Providers,1
            lvf_temp_IN=0
            lvf_temp_OUT=0
            !
            lvf_on(i)=0
            lvf_off(i)=0
            lvf_propDemandOutdoor=0
            lvf_propDemand=0
            !         
            lvf_in_1(i)=0
            lvf_in_2(i)=0
            lvf_out_1(i)=0
            lvf_out_2(i)=0
            !
            ! --------------------------------
            !
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3)/gvf_WaterDemand_acft(i,gvi_order,1)
              if(lvl_InOut)then
                ! On project, but outdoor demand driven
               lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,10)/ &
                 (gvf_WaterDemand_acft(i,gvi_order,10)+gvf_WaterDemand_acft(i,gvi_order,8))
              endif
            endif
            !
               lvf_temp_IN= gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,9)
               lvf_temp_OUT=gvf_WaterDemand_acft(i,gvi_order,8)+ gvf_WaterDemand_acft(i,gvi_order,10)
              if(0 < lvf_temp_OUT)lvf_propDemandOutdoor= lvf_temp_OUT/(lvf_temp_IN+lvf_temp_OUT)
            !
             case_code= nint(code)
            select case(case_code)
                !if(code == gpi_classA)then
                case (gpi_classA)
                  !
                  lvf_on(i) =lvf_inputs(i)
                  !
                  ! ---------------------------
                  lvf_in_2(i)=  lvf_on(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    !
                  lvf_out_2(i)= lvf_on(i)  - lvf_in_2(i) !* lvf_propDemandOutdoor  ! Outdoor water use On
                    call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
                    !
                  !
                !else if( code == gpi_ClassBC)then
                case (gpi_ClassBC)
                  lvf_on(i) =lvf_inputs(i)
                  !
                  ! ---------------------------
                  lvf_in_2(i)=  lvf_on(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    call sOn_inRounding(i,gvi_order,lvf_in_2)
                    !
                  lvf_out_2(i)= lvf_on(i) - lvf_in_2(i) ! * lvf_propDemandOutdoor  ! Outdoor water use On
                    call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)

                  !
                !else if(code == gpi_cap)then
                case (gpi_cap)
                  !
                  if(0 < gvf_WaterDemand_acft(i,gvi_order,2))then
                    if(lvf_inputs(i) <= gvf_WaterDemand_acft(i,gvi_order,2))then
                      lvf_off(i)=lvf_inputs(i)
                    else
                      lvf_off(i)= gvf_WaterDemand_acft(i,gvi_order,2)
                      lvf_on(i) = lvf_inputs(i)- lvf_off(i)
                    endif
                      !
                        if(lvf_inputs(i) <= gvf_WaterDemand_acft(i,gvi_order,7)+ gvf_WaterDemand_acft(i,gvi_order,8))then
                        !
                         call sOptimize_OFF(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1)
                          call sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
                        !
                        else
                        !
                         call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                        !
                        endif
                        !
                      !
                  else
                    lvf_on(i)=lvf_inputs(i)
                    !
                    lvf_in_2(i)=lvf_inputs(i)  * (1-lvf_propDemandOutdoor)! lvf_propDemandIndoor)  ! Indoor water use On
                    lvf_out_2(i)= lvf_inputs(i) -  lvf_in_2(i) !* lvf_propDemandOutdoor  ! Outdoor water use On
                      call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)

                    !
                  endif
                  !
                  ! -------------------
                  !
                !else if(code == gpi_groundWater)then
                case(gpi_groundWater)
                    !
                    lvf_demand=gvf_WaterDemand_acft(i,gvi_order,1)
                    ! Total demand > 0
                    if(0 < lvf_demand)then
                        ! Check on-project first
                       if(0 < gvf_WaterDemand_acft(i,gvi_order,3))then
                         !
                         if( 1 <= gvf_WaterDemand_acft(i,gvi_order,3))then
                          lvf_propDemand=gvf_WaterDemand_acft(i,gvi_order,3) * (1./lvf_demand)
                          ! 
                          lvf_on(i)=lvf_propDemand* lvf_inputs(i)
                          lvf_off(i)=lvf_inputs(i)-lvf_on(i)
                          !
                            !
                            call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                            !
                         else
                            ! give the 1 AF demand to off-project
                            lvf_off(i)=lvf_inputs(i)
                            !
                            lvf_in_1(i)=min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use Off
                            lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8),lvf_inputs(i) - lvf_in_1(i))  ! Outdoor water use Off
                            !
                         endif
                       else
                        lvf_off(i)=lvf_inputs(i)
                        !
                        lvf_in_1(i)= min(gvf_WaterDemand_acft(i,gvi_order,7),lvf_inputs(i)  * (1-lvf_propDemandOutdoor))! lvf_propDemandIndoor)  ! Indoor water use On
                        lvf_out_1(i)=max(gvf_WaterDemand_acft(i,gvi_order,8), lvf_inputs(i)  - lvf_in_1(i)) ! Outdoor water use On
                        !
                       endif
                    else
                    endif           
                !else
                case default
                !
                  if(0 < lvf_inputs(i))then
                      lvf_on(i) =lvf_inputs(i)*lvf_propDemand
                      lvf_off(i)=lvf_inputs(i)-lvf_on(i)
                      !
                      ! For reclaimed water and rainwater and storm water
                      if(lvl_InOut)then
                       lvf_out_1(i)= lvf_off(i)  ! Outdoor water use Off
                       lvf_out_2(i)= lvf_on(i)   ! Outdoor water use On

                       lvf_in_1(i)=  0  ! Indoor water use Off Project
                       lvf_in_2(i)=  0  ! Indoor water use On
                        !
!                        if(0 < gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1))then
!                         gvf_WaterDemand_acft(i,gpi_unusedNonPotable,10)=lvf_propDemand*gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)
!                         gvf_WaterDemand_acft(i,gpi_unusedNonPotable,8)=(1-lvf_propDemand)*gvf_WaterDemand_acft(i,gpi_unusedNonPotable,1)
!                        endif
                      else
                        !
                        call sOptimize(i,gvi_order,lvf_inputs,lvf_in_1,lvf_out_1,lvf_in_2,lvf_out_2)
                         if(0 < lvf_propDemand)then
                          call sOn_Rounding(i,gvi_order,lvf_in_2,lvf_out_2)
                         else
                          call sOff_Rounding(i,gvi_order,lvf_in_1,lvf_out_1)
                         endif
                        !
                      endif   
                     !
                  endif
                                              
            !endif
            end select
            !
         end do
        !
          do i = 1,gvi_Providers,1
           ! in_1 is "off-project", in_2 is "on-project"
           gvf_WaterDemand_acft(i,gvi_order,4)= anint(max(0,lvf_off(i)))
           gvf_WaterDemand_acft(i,gvi_order,5)= anint(max(0,lvf_on(i)))
           gvf_WaterDemand_acft(i,gvi_order,6)=code
           !  
           gvf_WaterDemand_acft(i,gvi_order,11)= anint(max(0,lvf_in_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,12)= anint(max(0,lvf_out_1(i)))
           gvf_WaterDemand_acft(i,gvi_order,13)= anint(max(0,lvf_in_2(i)))
           gvf_WaterDemand_acft(i,gvi_order,14)= anint(max(0,lvf_out_2(i)))
           !
              ! -------------------------------------------
            do j = 2,3,1
               gvf_WaterDemand_acft(i,gvi_order+1,j)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,j))then
                   gvf_WaterDemand_acft(i,gvi_order+1,j)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,j)- gvf_WaterDemand_acft(i,gvi_order,j+2))))
              endif
            end do
              !
               gvf_WaterDemand_acft(i,gvi_order+1,1)=0
            if(0 < gvf_WaterDemand_acft(i,gvi_order,1))then
             gvf_WaterDemand_acft(i,gvi_order+1,1)= anint( &
                max(0,gvf_WaterDemand_acft(i,gvi_order,1) &
                - (gvf_WaterDemand_acft(i,gvi_order,4) + gvf_WaterDemand_acft(i,gvi_order,5)  ) ))
            endif
            !    
                ! -------------------------------------------
            do j = 7,10,1
               gvf_WaterDemand_acft(i,gvi_order+1,j)=0
              if(0 < gvf_WaterDemand_acft(i,gvi_order,j))then
                   gvf_WaterDemand_acft(i,gvi_order+1,j)= &
                    anint(max(0,(gvf_WaterDemand_acft(i,gvi_order,j)- gvf_WaterDemand_acft(i,gvi_order,j+4))))
              endif
            end do
                ! 
          end do
          !
        return
    end subroutine sCWaccountingIndoorOut
    ! ------------------------------------

!
! =======================================================================================================
! E.O.F. Demand.f90