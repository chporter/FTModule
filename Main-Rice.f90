!   Driver for testing Progress Rate toward Flowering Module
!   Written, 2016-09-27 CHPorter, Jim Jones
!------------------------------------------------------------------------                                  
Program Main
!------------------------------------------------------------------------                                  
  integer DOY, SDat, YRDOY, YR, Fdoy, RunNo
  real x
  real DLi, Sradi, Tmeani, Vti, ADAP
  real SumRFi
  real, dimension(70) :: Xi           !Vector of genotype markers / parameters
  character(8) CultivarID
  character(12) Weather
  character(1) text
!------------------------------------------------------------------------  
! Sensitivity tests using all markers =2, all markers=1, and all markers =0.
! But, the dataset only included genotypes with all markers=2, not 0 or 1
! So, these runs would represent 2 genotypes that did not appear in the data
! Also, even though Gezan found 19 significant markers, the model fitting process (backward) eliminated two of them (X66 and X25)
!------------------------------------------------------------------------  

  CultivarID = " Gen102 "
  Xi = 0.0

  Xi(5)  = +2.0
  Xi(7)  = +2.0
  Xi(10) = +2.0
  Xi(13) = +2.0
  Xi(17) = +2.0
  Xi(18) = +2.0
  Xi(24) = +2.0
  Xi(26) = +2.0
  Xi(29) = +2.0
  Xi(30) = +2.0
  Xi(37) = +2.0
  Xi(40) = +2.0
  Xi(43) = +2.0  
  Xi(44) = +2.0
  Xi(56) = +2.0
  Xi(60) = +2.0
  Xi(68) = +2.0
  
!------------------------------------------------------------------------                                  
!  Weather = "CCPO1201.WTH"
! initialization, Open Files for output
!------------------------------------------------------------------------                                  
  open(30,File="Anthesis.OUT",status='REPLACE')
  write(30,'(a)') "  adap yr doy      srad     tmean        Vt    SumRDi"
  open(40,File='Sens.Temp.OUT',status='REPLACE')
  write(40,'(a)') "  Run#   Cultivar     DAYL      Srad     Tmean        Vt      SumRFi      ADAP      ADOY"

!------------------------------------------------------------------------                                  
! Set sowing/start day of year for flowering model to start
!  Initialize progress toward flowering, SumDRi, & Day of First Flower, Fdoy
!------------------------------------------------------------------------                                  
  SDat = 41   !sowing date
  ADAP = 0.0    !counter for number of days from sowing to anthesis
  SumRDi = 0.0
  Fdoy = 0.0
  RunNo = 0
  YR = 00
!------------------------------------------------------------------------                                  
!Set constant values for weather inputs for sensitivity analysis
!------------------------------------------------------------------------  
! doesn't iterate for Vti or Sradi; currently, only Tmeani and DLi, uses mean values                                
  Sradi = 21.55  
  Vti = 1.6      
  DLi = 10.5  
  Tmeani  = 14.0
!------------------------------------------------------------------------                                  
!Daylength (DLi) variations loop, m counter, and set
!Temperature variations simulation loop, k counter 
!------------------------------------------------------------------------                                  
  do m = 1,15  !Daylength loop
!   Increment daylength
    DLi   = DLi + 0.5
    Tmeani  = 13.0

    do k = 1,15  !Tmean loop
!     Increment temperature
      Tmeani  = Tmeani + 1

!     Initialize accumulators
      SumRFi = 0.0
      ADAP   = 0.0  
      RunNo = RunNo + 1

!      do i = 1, 365
!        DOY = i
!      
!        If (DOY > SDat-1) Then
!          ADAP = ADAP + 1
!          YRDOY = SDat + ADAP  !no development on sowing day

!------------------------------------------------------------------------                                  
!         Development calls Rate Module in a time loop for integration (daily)
! ** NOTE: this will give the same result with each call. Could just call once and invert to get DAP

          call RFlower_rate (DLi, Sradi, Tmeani, Vti, Xi, SumRFi)

!   call one time and calculate ADAP
          If (SumRFi > 1.E-3) then
            ADAP = 1.0 / SumRFi
            DOY = SDAT + int(adap)
          else
            ADAP = -99.
            DOY = -99
          endif
          FDOY = DOY

         ! write(30,'(i6,1X,i2,1X,I3.3,3F10.2,F10.3)') ADAP, YR, DOY, Sradi, Tmeani, Vti, SumRFi

!          if (SumRFi > 1.0) then
!            Fdoy = YRDOY
!            exit
!          else
!            Fdoy = -99
!          endif
!!------------------------------------------------------------------------                                  
!        endif
!      enddo

      write(40,'(i6,2x,A8, 3f10.1,F10.2,2X,F10.4,F10.1,I10))') RunNo, CultivarID, DLi,Sradi,Tmeani, Vti,SumRFi,ADAP,Fdoy
   !   write(30,'(A,4x,f10.2))') CultivarID, SumRFi 

    enddo  !Tmean loop
  enddo  !Daylength loop

  Stop
End Program Main