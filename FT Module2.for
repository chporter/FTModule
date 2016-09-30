      Module AnthesisRate

C   Module for Development toward first flower in Common Bean, using G, E, and G x E inputs
C   Based on data and relationahip reported by Bahkta et al. (???, submitted), 5 locations, 187 bean lines
C   Bean lines developed by Vallejos et al. (???) using a bi-parental family of Common Bean
C     (Calima and Jamapa)
C   
          real Daym, Sradm, Tminm, Tmaxm
C     Module-specific values of coefficients, specific to this function estimated from data reported by Bakhta et al.
          parameter 
       Daym = 12.37,
       Sradm = 18.218,
       Tminm = 16.128,
       Tmaxm = 27.458

      contains

       Subroutine(Time, Dayi, Sradi, Tmini, Tmaxi,
      &   TF1i, TF2i, TF3i, TF4i, TF5i, TF6i, TF7i, TF8i, TF9i, TF10i, 
              &   TF11i, TF12i,
      &   TFi, RDi, SumRDi)
      
C   INPUTS to Module:
C     E(Environmental variables): Dayi, Sradi, Tmini, Tmaxi
C     G (Genetic Variables): TF1i, TF2i, TF3i, TF4i, TF5i, TF6i, TF7i, TF8i, TF9i, TF10i, TF11i, TF12i
C   Thenumerical coefficients in the equation were developed for this specific population 
C     These are either coded in the eequation or above the equation, with numerical values that cannot be changed by users for other genotypes
C     Changing them would cause results outside the confines of the data used to estimate them
C   Initial value of progress toward development
C     SumRDi0 (initially, SumRDi0 = 0.0 at time of plant emergence
C   

C   Modul Outputs, Dynamic Variables:
C   
C   Daily environmental variables (see above), and computations of the rate of development
C     RDi = Daily rate toward development, fraction such that when integrated over time and a value of 1.0 is reached (or exceeded
C           First Flower occurs on that day
C     RDi = (1/TFi), where
C     TFi = predicted time for the plants to reach first flower for the selected genotype and the environmental factors on current day
C     SumRDi = current progress toward first flowering from emergence (dimensionless), integral of RDi from crop emergence to current day


C     The dynamic gene-based mixed effects linear model
       FTi = 44.18 + 4.026*(Dayi - Daym) + 0.1895*(Sradi - Sradm) 
        - 1.363*(Tmaxi - Tmaxm) - 0.6091*(Tmini - Tminm) - 1.31*(TF1i) 
        - 2.279*(TF2i) + 1.59*(TF3i) - 0.5576 (TF4i) + 0.04971*(TF5i) 
        - 0.8937*(TF6i) + 0.8774*(TF7i) + 0.3658*(TF8i) + 0.6629*(TF9i) 
        + 0.3565*(TF10i)-0.5583*(TF11i)+0.326*(TF12i)-0.3337*(TF1i*TF2i) 
        + 0.3031*((Tmini – Tminm)* TF2i) + 1.808*((Dayi – Daym)*TF3i) 
        + 0.1974*((Tmini – Tminm)*TF3i) - 0.1495*((Tmaxi – Tmaxm)*TF5i) 
        + 0.4997*((Dayi – Daym)*TF7i) + 0.0266*((Sradi – Sradm)*TF12i) 
        - 0.2764*((Dayi – Daym)*TF12i)  

       RDi = (1/FTi)

C     Compute integral of development to pass back the cumulative progress toward development each day       
       SumRDi = SumRDi0 + RDi*1.0

C     In the equation for computing SumRDi, the time step is assumed to be 1.0 d for this module (fixed)
      
       Return
       End 

      End Module AnthesisRate
C   
C
C   Variable Definitions:
C   FTi = flowering time of  the ith genotype, 
C   44.18 is the mean flowering time (day) across the five sites,see Bakhta et al., 2016 
C   Dayi = average day length from sowing to flowering observed by the ith genotype (hours), 
C   Daym = mean day length across all five sites (12.37 hrs), 
C   Sradi = average solar radiation from sowing to flowering observed by the ith genotype (Srad, MJ/m2d), 
C   Sradm = mean solar radiation across all five sites (18.218 MJ/m2d), 
C   Tmini = average minimum temperature from sowing to flowering observed by the ith genotype (°C), 
C   Tminm = mean solar radiation across all five sites (16.128 °C), 
C   Tmaxi =  average maximum temperature from sowing to flowering observed by ith genotype (°C), 
C   Tmaxm = mean solar radiation across all five sites (27.458 °C), 
C   TF1i :TF12i = alleles at QTL TF1:TF12 in ith genotype (Calima alleles = “+1” and Jamapa allele = “-1”). 
