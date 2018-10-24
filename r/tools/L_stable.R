
# This is a function to calculate the boundary layer parameters under stable cond.
# from temperature values at two heights and a wind speed value at 
# one height.
# Author: Yusri Yusup, PhD
# Version: 1.0
# Date: 2018-10-24
L_stable <- function(z0h = 0.001, #roughness length for heat [m]
                     z0m = 0.001, #roughness length for momentum [m]
                     zT1 = 1, #height of temperature 1 [m]
                     zT2 = 10, #height of temperature 2 aloft [m]
                     zU = 10, #height of wind [m]
                     U = 1.0, #wind speed [m s-1]
                     T1 = 29.0, #temperature at height 1 [celsius]
                     T2 = 30.0) #temperature at height 2 aloft [celsius]
{
  # constants
  g = 9.80 #gravitational acceleration constant [m s-2]
  beta = 1 #assuming beta is 1
  k = 0.40 #assuming von Karman constant is 0.4
  
  # Using the quadratic equation to solve for L
  A <- (g * (T2 - T1)) / (U^2 * T2)
  # Calculate the quadratic coefficients
  a <- ((beta * (zT2 - zT1)) - ((A * beta^2) * (zU + z0m)^2)) 
  b <- (log((zT2 + z0h)/(zT1 + z0h)) - ((2 * A * beta) * (zU + z0m) * log((zU + z0m)/z0m)))
  c <- -A * (log((zU + z0m)/z0m))^2
  # Calculate delta
  delta <- b^2 - (4 * a * c)
  
  # Calculate the roots
  x_1 <- (-b + sqrt(delta))/(2 * a)
  x_2 <- (-b - sqrt(delta))/(2 * a)
  
  if (x_1 > 0) { M <- x_1 }
  if (x_2 > 0) { M <- x_2 }
  
  #L <- 1 / M
  return(c(x_1,x_2))
}
