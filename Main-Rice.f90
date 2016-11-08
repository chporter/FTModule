!   Driver for testing Progress Rate toward Flowering Module
!   Written, 2016-09-27 CHPorter, Jim Jones
!------------------------------------------------------------------------                                  
Program Main
!------------------------------------------------------------------------ 
  implicit none
  
  integer DOY, SDat, YR, Fdoy, RunNo
  integer Tmeani_N, DLi_N, k, m
  real x
  real DLi, Sradi, Tmeani, Vti, ADAP
  real DLi_incr, DLi_start
  real Tmeani_incr, Tmeani_start
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
!  open(30,File="Anthesis.OUT",status='REPLACE')
!  write(30,'(a)') "  adap yr doy      srad     tmean        Vt    SumRDi"
  open(40,File='Sens.Temp.OUT',status='REPLACE')
  
  write(40,'(A,A,17(/,2X,A,F8.3))') &
    "Marker values for genotype: ", CultivarID, &
    "Xi(5)  =", Xi(5) ,  &
    "Xi(7)  =", Xi(7) ,  &
    "Xi(10) =", Xi(10),  &
    "Xi(13) =", Xi(13),  &
    "Xi(17) =", Xi(17),  &
    "Xi(18) =", Xi(18),  &
    "Xi(24) =", Xi(24),  &
    "Xi(26) =", Xi(26),  &
    "Xi(29) =", Xi(29),  &
    "Xi(30) =", Xi(30),  &
    "Xi(37) =", Xi(37),  &
    "Xi(40) =", Xi(40),  &
    "Xi(43) =", Xi(43),  &  
    "Xi(44) =", Xi(44),  &
    "Xi(56) =", Xi(56),  &
    "Xi(60) =", Xi(60),  &
    "Xi(68) =", Xi(68)  

    
  write(40,'(/,a)') "  Run#   Cultivar     DAYL      Srad     Tmean        Vt      SumRFi      ADAP      ADOY"

!------------------------------------------------------------------------                                  
! Set sowing/start day of year for flowering model to start
!  Initialize progress toward flowering, SumDRi, & Day of First Flower, Fdoy
!------------------------------------------------------------------------                                  
  SDat = 41   !sowing date
  ADAP = 0.0    !counter for number of days from sowing to anthesis
  SumRFi = 0.0
  Fdoy = 0.0
  RunNo = 0
  YR = 00
!------------------------------------------------------------------------                                  
!Set constant values for weather inputs for sensitivity analysis
!------------------------------------------------------------------------  
! doesn't iterate for Vti or Sradi; currently, only Tmeani and DLi, uses mean values                                
  Sradi = 21.55  
  Vti = 1.6      

  Tmeani_start = 14.0
  Tmeani_incr  = 1.0
  Tmeani_N     = 15

  DLi_start = 11.0
  DLi_incr  = 0.5
  DLi_N     = 15
  
!------------------------------------------------------------------------                                  
!Daylength (DLi) variations loop, m counter, and set
!Temperature variations simulation loop, k counter 
!------------------------------------------------------------------------ 
  DLi = DLi_start                                 
  do m = 1, DLi_N  !Daylength loop
!   Increment daylength
    DLi   = DLi + DLi_incr

    Tmeani  = Tmeani_start
    do k = 1, Tmeani_N  !Tmean loop
!     Increment temperature
      Tmeani  = Tmeani + Tmeani_incr

!     Initialize accumulators
      SumRFi = 0.0
      ADAP   = 0.0  
      RunNo = RunNo + 1

!     call flowering module one time and calculate ADAP
      call RFlower_rate (DLi, Sradi, Tmeani, Vti, Xi, SumRFi)

      If (SumRFi > 1.E-3) then
        ADAP = 1.0 / SumRFi
        DOY = SDAT + int(adap)
      else
        ADAP = -99.
        DOY = -99
      endif
      FDOY = DOY

      write(40,'(i6,2x,A8, 3f10.1,F10.2,2X,F10.5,F10.1,I10)') RunNo, CultivarID, DLi, Sradi, Tmeani, Vti, SumRFi, ADAP, Fdoy

    enddo  !Tmean loop
  enddo  !Daylength loop

  Stop
End Program Main