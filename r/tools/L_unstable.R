# This is a function to calculate the boundary layer parameters from 
# temperature values at two heights and a wind speed value at 
# one height.
# Author: Yusri Yusup, PhD
# Version: 1.0
# Date: 2018-10-23
L_unstable <- function(z0h = 0.001, #roughness length for heat [m]
                       z0m = 0.001, #roughness length for momentum [m]
                       zT1 = 1, #height of temperature 1 [m]
                       zT2 = 10, #height of temperature 2 aloft [m]
                       zU = 10, #height of wind [m]
                       U = 1.0, #wind speed [m s-1]
                       T1 = 30.0, #temperature at height 1 [celsius]
                       T2 = 29.5) #temperature at height 2 aloft [celsius]
  {
  # constants
  g = 9.80 #gravitational acceleration constant [m s-2]
  gamma = 1 #assuming gamma is 1
  k = 0.40 #assuming von Karman constant is 0.4
  
  #Initialize M to 0
  M_new <- 0 
  
  # Difference randomly chosen to be > 0.001
  delta <- 1 
  while (delta >= 0.0001) {
    #Calculate thetaS
    X1 = (1 - (M_new * gamma * (zT1 + z0h)))^(1/4)
    X2 = (1 - (M_new * gamma * (zT2 + z0h)))^(1/4)
    thetaS = (k * (T2 - T1))/(log((zT2 + z0h)/(zT1 + z0h)) + 
                                log(1 + X2^2) + log(1 + X1^2))
    #Calculate Us
    X3 = (1 - (M_new * gamma * (zU + z0m)))^(1/4)
    Us = (k * U)/(log((zU + z0m)/z0m) - log(((1 + X3^2) * (1 + X3)^2)/8) 
                  + (2 * tanh(X3)) + (pi/2))
    
    L_1 <- (1 * Us^2 * T2)/(k * g * thetaS) #Assumption that T2 is virtual temperature
    M <- 1/L_1                              #T2 should be > T actual by 2-3 C.
    delta <- abs(M - M_new)
    M_new <- M
  }
  # Return three parameters in order
  # 1. 1/Obukhov length = 1/L [m-1]
  # 2. Friction velocity scale [m s-1]
  # 3. Friction temperature scale [celsius]
  
  param <- c(M, Us, thetaS)
  return(param)
}