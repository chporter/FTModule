!   Driver for testing Flowering Time Module

!   Written, 2016-09-27 CHPorter, Jim Jones


Program Main

  USE AnthesisRate

  integer doy, SDat, YRDOY, YR, ADAP
  real x
  real Dayi, Sradi, Tmini, Tmaxi
  real SumRDi
  real, dimension(500) :: TFi           !Vector of cultivar parameters
  character(30) CultivarID
  character(12) Weather
  character(1) text

  CultivarID = "Jamapa"
  Weather = "CCPO1201.WTH"

! initialization
  call TFlower_init(CultivarID, TFi, SumRDi)
  
! Set sowing/start day of year for flowering model to start
  SDat = 41   !sowing date
  ADAP = 0    !counter for number of days to anthesis
  
  open(20,File=Weather,Status='OLD')  !DSSAT format weather data
  read(20,'(4/,a)') text              !skip first 5 lines
  open(30,File="Anthesis.OUT",status='REPLACE')
  write(30,'(a)') "adap yr doy  srad  tmin  tmax  SumRDi"

  Dayi = 12.0  !temporarily set daylength to 12 hours

! rate
  do i = 1, 365
    read(20,'(i5,3F6.0)') YRDOY, Sradi, Tmaxi, Tmini 
    DOY = MOD(YRDOY,1000)
    YR = (YRDOY - DOY)/1000
    If (DOY > SDat-1) Then
      ADAP = ADAP + 1
      call TFlower_rate (Dayi, Sradi, Tmini, Tmaxi, TFi, SumRDi)
      write(30,'(i4,1X,i2,1X,I3.3,3f6.2,f8.4)') ADAP, YR, DOY, Sradi, Tmini, Tmaxi, SumRDi
      if (SumRDi > 1.0) then
        exit
      endif
    endif
  enddo

  Stop
End Program Main