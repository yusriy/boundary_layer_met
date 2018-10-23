source('r/tools/L_unstable.R')
U <- seq(from = 0.1, to = 10, by = 0.1)

len <- 0
Us <- 0
thetaS <- 0
for (i in 1:length(U)) {
  len[i] <- L_unstable(U = U[i])[1]
  Us[i] <- L_unstable(U = U[i])[2]
  thetaS[i] <- L_unstable(U = U[i])[3]
}

plot(U, len, type = 'l')
plot(U, Us, type = 'l')
plot(U, thetaS, type = 'l')

T1 <- 30
T2 <- seq(from = 28.0, to = 29.9, by = 0.1)
len <- 0
Us <- 0
thetaS <- 0
for (i in 1:length(T2)) {
  len[i] <- L_unstable(T1 = T1, T2 = T2[i])[1]
  Us[i] <- L_unstable(T1 = T1, T2 = T2[i])[2]
  thetaS[i] <- L_unstable(T1 = T1, T2 = T2[i])[3]
}

plot(T2, len, type = 'l', col ='red')
plot(T2, Us, type = 'l', col ='red')
plot(T2, thetaS, type = 'l', col ='red')
