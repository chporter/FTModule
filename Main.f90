!   Driver for testing Flowering Time Module

Program Main

  USE AnthesisRate

  integer doy
  real x
  real Dayi, Sradi, Tmini, Tmaxi
  real SumRDi
  real, dimension(500) :: TFi           !Vector of cultivar parameters
  character(30) CultivarID

  CultivarID = "Jamapa"

! initialization
  call TFlower_init(CultivarID, TFi, SumRDi)

! rate
  do doy = 1, 365
    Dayi = 12.0
    Sradi = 12.0
    call RANDOM_NUMBER(x) !uniform dist, 0-1
    Tmini = 20 + (x-0.5)*2.
    Tmaxi = Tmini + 5
    
    call TFlower_rate (Dayi, Sradi, Tmini, Tmaxi, TFi, SumRDi)
    if (SumRDi > 1.0) then
      print *,"DOY = ", DOY, SumRDi, "    Anthesis"
      exit
    else
      print *,"DOY = ", DOY, SumRDi
    endif
  enddo

  Stop
End Program Main