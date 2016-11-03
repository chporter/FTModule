 !   Written, 2016-09-27 Jim Jones
!   Compiled and modified by CH Porter
!   
!   Module for Rate of Development toward first flower in Rice, using G, E, and G x E inputs
!     Integrating RF vs. time to simulate first flower event
!   Based on data and relationahip reported by Chris Hwang et al., using data from 10 locations, 
!   provided by CIRAD and IRRI, following the logic used for the Bean model. Linear mixed effects model
!     fitting RF = (1/TF) depending on E, G, and GxE factors
!------------------------------------------------------------------------      

       Subroutine RFlower_rate (DLi, Sradi, Tmeani, Vti, Xi, SumRFi)
       
!------------------------------------------------------------------------                                  
!   INPUTS to Module:
!------------------------------------------------------------------------                                  
!     E(Daily Environmental variables): DLi, Sradi, Tmeani, Vti
!     G (Genetic Variables): X(1) thru X(70); only 17 are used-backward selection model
!   The numerical coefficients in the equation were developed for this specific population 
!     These are either coded in the eequation or above the equation, with numerical values that cannot be changed by users for other genotypes
!     Changing them would cause results outside the confines of the data used to estimate them
!   Initial value of progress toward development
!     SumRFi0 (initially, SumRFi0 = 0.0 at time of transplanting)
!
!   Modul Outputs, Dynamic Variables:
!     SumRFi = current progress toward first flowering from emergence (dimensionless), integral of RDi from crop emergence or planting to current day
!     The starting time for accumulation of time to first flower depends on the data used to fit the model. Ideally, it will be emergence.
!
!   Local variables
!     RFi = Daily rate toward development, fraction such that when integrated over time and a value of 1.0 is reached (or exceeded
!           First Flower occurs on that day, this is the mixed effects dependent variable 
!     RFi = Rate of progress from transplanting to first flower appearance for selected genotype and environmental factors on current day 
!------------------------------------------------------------------------                                  

        Real, intent(in) :: DLi, Sradi, Tmeani, Vti !daily weather data
        Real, dimension(70) :: Xi                  !Vector of cultivar parameters, genotype i
        Real, intent(out) :: SumRFi                !progression towards anthesis

        Real RFi, RFmean, DLm, Sradm, Tmeanm, Vtm

!        Mean values of environmental variables, estimated during fitting process for specific dataset used
         RFmean = 0.0098
         DLm = 12.07
         Sradm = 21.55
         Tmeanm = 25.08
         Vtm    = 1.60
!------------------------------------------------------------------------                                  
!       The dynamic gene-based mixed effects linear model, Backward selection
!------------------------------------------------------------------------                                  
        RFi = RFmean
     &    - 1.607e-03 * (Tmeani-Tmeanm)
     &    - 1.950e-02 * (Vti - Vtm)
     &    + 1.518e-03 * (Sradi - Sradm)
     &    - 1.287e-02 * (DLi - DLm)
     &    + 5.173e-06 * Xi(5)
     &    + 4.985e-04 * Xi(7)
     &    - 5.457e-05 * Xi(10)
     &    + 1.800e-04 * Xi(13)
     &    - 1.025e-03 * Xi(17)
     &    + 3.528e-05 * Xi(18)
     &    + 4.805e-04 * Xi(24)
     &    + 5.007e-04 * Xi(26)
     &    + 1.478e-04 * Xi(29)
     &    - 1.067e-04 * Xi(30)
     &    + 5.285e-04 * Xi(37)
     &    + 2.550e-04 * Xi(40)
     &    - 2.709e-04 * Xi(43)
     &    + 2.870e-04 * Xi(44)
     &    - 8.195e-04 * Xi(56)                   
     &    - 5.763e-05 * Xi(60)
     &    - 4.118e-04 * Xi(68)
     &    + 2.993e-04 * (Tmeani-Tmeanm) * Xi(5)
     &    + 3.037e-05 * (Tmeani-Tmeanm) * Xi(7)
     &    + 1.607e-04 * (Tmeani-Tmeanm) * Xi(10)
     &    + 9.603e-05 * (Tmeani-Tmeanm) * Xi(24)
     &    + 1.834e-04 * (Tmeani-Tmeanm) * Xi(29)
     &    + 3.211e-04 * (Tmeani-Tmeanm) * Xi(43)
     &    + 1.428e-04 * (Tmeani-Tmeanm) * Xi(44)
     &    - 2.821e-04 * (Tmeani-Tmeanm) * Xi(56)
     &    + 3.719e-04 * (Tmeani-Tmeanm) * Xi(60)            	  
     &    + 2.050e-03 * (Vti - Vtm) * Xi(5)
     &    + 3.874e-04 * (Vti - Vtm) * Xi(7)
     &    + 9.691e-04 * (Vti - Vtm) * Xi(10)
     &    + 4.923e-04 * (Vti - Vtm) * Xi(18)
     &    + 7.247e-04 * (Vti - Vtm) * Xi(24)
     &    + 1.434e-03 * (Vti - Vtm) * Xi(29)
     &    + 2.147e-03 * (Vti - Vtm) * Xi(43)
     &    + 9.815e-04 * (Vti - Vtm) * Xi(44)
     &    - 1.461e-03 * (Vti - Vtm) * Xi(56)
     &    + 2.806e-03 * (Vti - Vtm) * Xi(60)
     &    - 2.236e-04 * (Sradi - Sradm) * Xi(5)
     &    - 1.103e-04 * (Sradi - Sradm) * Xi(10)
     &    - 1.324e-04 * (Sradi - Sradm) * Xi(29)
     &    - 2.409e-04 * (Sradi - Sradm) * Xi(43)
     &    - 1.018e-04 * (Sradi - Sradm) * Xi(44)
     &    + 1.649e-04 * (Sradi - Sradm) * Xi(56)
     &    - 3.192e-04 * (Sradi - Sradm) * Xi(60)
     &    + 1.895e-03 * (DLi - DLm) * Xi(5)
     &    + 8.223e-04 * (DLi - DLm) * Xi(10)
     &    + 1.075e-03 * (DLi - DLm) * Xi(29)
     &    + 2.325e-03 * (DLi - DLm) * Xi(43)
     &    + 7.737e-04 * (DLi - DLm) * Xi(44)
     &    - 1.115e-03 * (DLi - DLm) * Xi(56)
     &    + 2.218e-03 * (DLi - DLm) * Xi(60)
!     
!------------------------------------------------------------------------                                  
!     Compute time integral of development to pass back the cumulative progress toward development each day  
!     In the equation for computing SumRFi, the time step is assumed to be 1.0 d for this module (fixed)
!------------------------------------------------------------------------                                  

      SumRFi = SumRFi + RFi*1.0
      
      Return
      End Subroutine RFlower_rate 
  
!------------------------------------------------------------------------                                  
!   Variable Definitions:                                                                                  
!------------------------------------------------------------------------                                  
!                                                                                                          
!   0.0098 is the mean rate toward flowering (1/day) across the ten site-years in the IRRI-CIRAD rice data                  
!   DLi = day length during time from sowing to flowering observed for the ith genotype (hours),               
!   DLm = mean day  length across all five sites, all genotypes (12.07 hrs),                                             
!   Sradi = average solar radiation from sowing to flowering observed by the ith genotype (Srad, MJ/m2d),  
!   Sradm = mean solar radiation across all sites, genotypes and transplanting dates in these data (21.55 MJ/m2d),
!   Tmeani = average daily temperatures each day during time from transplanting to first flower for ith genotype                                    
!   Tmeanm = average of Tmeani from transplanting to first flower across all genotypes, sites, years, (25.08 °C), 
!   Vti    = daily vapor pressure deficit average, mb
!   Vtm    = mean of the daily vapor pressure deficits across all genotypes, sites, sowing dates (1.60 mb)     
!   Xi(1):Xi(70) = alleles at QTL Xi1 :Xi70 in ith genotype