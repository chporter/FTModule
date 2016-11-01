!   Driver for testing Flowering Time Module
!   Written, 2016-09-27 CHPorter, Jim Jones

Program Main

!  USE AnthesisRate

  integer DOY, SDat, YRDOY, YR, ADAP
  real x
  real Dayi, Sradi, Tmini, Tmaxi
  real SumRDi, Fdoy
  real, dimension(12) :: TFi           !Vector of cultivar parameters
  character(8) CultivarID
  character(12) Weather
  character(1) text

  CultivarID = "00000000"
!  Weather = "CCPO1201.WTH"

! initialization

  open(30,File="Anthesis.OUT",status='REPLACE')
  write(30,'(a)') "adap yr doy  srad  tmin  tmax  SumRDi"
  open(40,File='Sens.Temp.OUT',status='old')
  write(40,'(a)') "Cultivar' DayLength  Srad  Tmin  Tmax  SumRDi   Day 1st Flower"!  call TFlower_init(CultivarID, TFi, SumRDi)
  
! Set sowing/start day of year for flowering model to start
  SDat = 41   !sowing date
  ADAP = 0    !counter for number of days from sowing to anthesis
!  Initialize progress toward flowering, SumDRi, & Day of First Flower, Fdoy
  SumDRi = 0.0
  Fdoy = 0.0
  
!Set constant values for weather inputs for sensitivity analysis
  Dayi = 12.0  
  Sradi = 15.0  
  Tmaxi = 26.0
  Tmini = 16.0

! Development calls Rate Module in a time loop for integration (daily)
  YR = 00
  do i = 1, 365
  DOY = i
      
    If (DOY > SDat-1) Then
      ADAP = ADAP + 1
      YRDOY = SDat + ADAP  !no development on sowing day
      call TFlower_rate (Dayi, Sradi, Tmini, Tmaxi, TFi, SumRDi)
      write(30,'(i4,1X,i2,1X,I3.3,3f6.2,f8.4)') ADAP, YR, DOY, Sradi, Tmini, Tmaxi, SumRDi
      if (SumRDi > 1.0) then
        Fdoy = YRDOY
        write(40,'(3x,A8,4X,4(f5.1,4X),f5.3,9x,f5.1)') CultivarID,Dayi,Sradi,Tmini,Tmaxi,SumRDi,Fdoy
        
        exit
      endif
    endif

  enddo
  Stop
End Program Main