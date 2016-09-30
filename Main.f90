!   Driver for testing Flowering Time Module

Program Main

  USE AnthesisRate

  integer doy
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

  open(20,File=Weather,Status='OLD')
  read(20,'(4/,a)') text    !skip first 5 lines
  open(30,File="Anthesis.OUT",status='REPLACE')
  write(30,'(a)') "  yrdoy  srad  tmin  tmax  SumRDi"

! rate
  do i = 1, 365

    Dayi = 12.0
    read(20,'(i5,3F6.0)') DOY, Sradi, Tmaxi, Tmini   
    
    !Sradi = 12.0
    !call RANDOM_NUMBER(x) !uniform dist, 0-1
    !Tmini = 20 + (x-0.5)*2.
    !Tmaxi = Tmini + 5
    
    call TFlower_rate (Dayi, Sradi, Tmini, Tmaxi, TFi, SumRDi)
    write(30,'(i7,3f6.2,f8.4)') DOY, Sradi, Tmini, Tmaxi, SumRDi
    if (SumRDi > 1.0) then
      exit
    endif
  enddo

  Stop
End Program Main