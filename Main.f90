!   Driver for testing Flowering Time Module
!   Written, 2016-09-27 CHPorter, Jim Jones

Program Main

!  USE AnthesisRate

  integer DOY, SDat, YRDOY, YR
  real x
  real Dayi, Sradi, Tmini, Tmaxi
  real SumRDi, Fdoy, ADAP
  real, dimension(12) :: TFi           !Vector of cultivar parameters
  character(8) CultivarID
  character(12) Weather
  character(1) text

  CultivarID = " Calima "
  TFi(1)=+1.0
  TFi(2)=+1.0
  TFi(3)=+1.0
  TFi(4)=+1.0
  TFi(5)=+1.0
  TFi(6)=+1.0
  TFi(7)=+1.0
  TFi(8)=+1.0
  TFi(9)=+1.0
  TFi(10)=+1.0
  TFi(11)=+1.0
  TFi(12)=+1.0  
  
!  Weather = "CCPO1201.WTH"

! initialization

  open(30,File="Anthesis.OUT",status='REPLACE')
  write(30,'(a)') "adap yr doy  srad  tmin  tmax  SumRDi"
  open(40,File='Sens.Temp.OUT',status='REPLACE')
  write(40,'(a)') "   Cultivar'  DayLength  Srad     Tmin     Tmax    SumRDi    Days to FL  Day 1st Flower"!  call TFlower_init(CultivarID, TFi, SumRDi)
  
! Set sowing/start day of year for flowering model to start
  SDat = 41   !sowing date
  ADAP = 0    !counter for number of days from sowing to anthesis
!  Initialize progress toward flowering, SumDRi, & Day of First Flower, Fdoy
  SumRDi = 0.0
  Fdoy = 0.0
  
!Set constant values for weather inputs for sensitivity analysis
  Dayi = 11.0  
  Sradi = 12.0  
  Tmaxi = 20.0
  Tmini = 10.0
  
!Daylength (Dayi) variations loop
  do m = 1,15
  
!Temperature variations simulation loop  
  do k = 1,15
! Development calls Rate Module in a time loop for integration (daily)
  YR = 00
  do i = 1, 365
  DOY = i
      
    If (DOY > SDat-1) Then
      ADAP = ADAP + 1
      YRDOY = SDat + ADAP  !no development on sowing day
      call TFlower_rate (Dayi, Sradi, Tmini, Tmaxi, TFi, SumRDi)
      write(30,'(f4.1,1X,i2,1X,I3.3,3f6.2,f8.4)') ADAP, YR, DOY, Sradi, Tmini, Tmaxi, SumRDi
      if (SumRDi > 1.0) then
        Fdoy = YRDOY
        write(40,'(3x,A8,4X,4(f5.1,4X),f5.3,7x,2(f5.1,4x))') CultivarID,Dayi,Sradi,Tmini,Tmaxi,SumRDi,ADAP,Fdoy
        
        exit
      endif
    endif

  enddo
      write(30,'(A8,4x,12(F5.2))') CultivarID, TFi 
!   Reinitialize timer and integrator for delta temperature loop
  Tmaxi  = Tmaxi + 1
  Tmini  = Tmini + 1
  SumRDi = 0.0
  ADAP   = 0.0
  enddo
!   Increment Dayi; Reinitialize timer and integrator for delta daylength loop 
  Tmaxi  = 20.0
  Tmini  = 10.0
  Dayi   = Dayi + 0.5
  
  SumRDi = 0.0
  ADAP   = 0.0  
  enddo
  Stop
End Program Main