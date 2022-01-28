library(rcosmo)
library(fractal)

start.time <- Sys.time()

# Link to download the CMB data set with resolution 2048
# URL = 'http://irsa.ipac.caltech.edu/data/Planck/release_2/all-sky-maps/maps/component-maps/cmb/COM_CMB_IQU-smica-field-Int_2048_R2.01_full.fits'
# downloadCMBMap(foreground = "smica", nside = 2048)

# Generating CMB data frame with nside=2048
cmbdf <- CMBDataFrame("CMB_map_smica2048.fits", ordering = "ring")
df1 <- coords(cmbdf, new.coords = "spherical")

# CMB_coord-coordinates of the CMB data
CMB_coord <- data.frame(cbind(theta1 = df1$theta, phi1 = df1$phi))

# Storing the intensities of pixels into a vector
Int1 <- cmbdf$I

# Computing the scaled CMB intensities
Scale <- max(abs(Int1 - mean(Int1)))
Int1 <- Int1 / max(abs(Int1))

# d-dimension
d <- 1

N_side <- 2048

# Tot_Ring is the total number of rings having the particular N_side value
Tot_Ring <- ((4 * (N_side)) - 1)

# Tot_RingP is the total number of pixels in the CMB sky sphere
Tot_RingP <- (12 * (N_side)^2)

# Tot_UPR is the total number of rings in the upper part
Tot_UPR <- (N_side - 1)

# Tot_ULPRP is the total number of pixels in the upper part = last pixel number of the upper part = Total number of pixels in the lower part
Tot_ULPRP <- (2 * (N_side) * (N_side - 1))

# Tot_MPR is the total number of rings in the middle part
Tot_MPR <- ((2 * N_side) + 1)

# Tot_UMPRP is the total number of pixels in the upper and middle parts together
Tot_UMPRP <- ((Tot_RingP) - (Tot_ULPRP))

# Tot_LPR is the total number of rings in the lower part
Tot_LPR <- (N_side - 1)

# These functions give the ring number of the specific pixel
# Upperpolar_Ringno function gives the ring number of the pixel if the pixel belongs to the upper part
Upperpolar_Ringno <- function(CMB_row) {
  U_ring <- 0
  ppixel_count <- 0
  pixelring_count <- 0
  for (i in 1:(N_side - 1)) {
    pixelring_count <- pixelring_count + 4
    ppixel_count <- ppixel_count + pixelring_count
    if (CMB_row <= ppixel_count) {
      U_ring <- i
      break
    }
  }
  return(U_ring)
}

# Middlepolar_Ringno function gives the ring number of the pixel if the pixel belongs to the middle part
Middlepolar_Ringno <- function(CMB_row) {
  M_ring <- as.integer((CMB_row - Tot_ULPRP - 1) / (4 * N_side)) + N_side
  return(M_ring)
}

# Lowerpolar_Ringno function gives the ring number of the pixel if the pixel belongs to the lower part
# upCMB_row is the updated pixel number of the lower part but in reverse order
Lowerpolar_Ringno <- function(CMB_row) {
  upCMB_row <- (Tot_RingP - CMB_row) + 1
  U_ring <- Upperpolar_Ringno(upCMB_row)
  L_ring <- ((Tot_Ring - (U_ring)) + 1)
  return(L_ring)
}

# These functions find the first and last members of the ring and get all their member pixels into a vector
Upperpolar_firstlast <- function(Ring_no) {
  first_member <- (2 * (Ring_no - 1) * Ring_no) + 1
  last_member <- (2 * Ring_no * (Ring_no + 1))
  FL <- c(first_member, last_member)
  return(FL)
}

Middlepolar_firstlast <- function(Ring_no) {
  first_member <- ((Tot_ULPRP) + (((Ring_no - N_side) * (4 * N_side)) + 1))
  last_member <- ((Tot_ULPRP) + (((Ring_no - N_side) + 1) * (4 * N_side)))
  FL <- c(first_member, last_member)
  return(FL)
}

Lowerpolar_firstlast <- function(Ring_no) {
  first_member <- Tot_RingP - (2 * (Tot_Ring - Ring_no + 1) * (Tot_Ring - Ring_no + 2)) + 1
  last_member <- Tot_RingP - ((2 * (Tot_Ring - Ring_no) * (Tot_Ring - Ring_no + 1)) + 1) + 1
  FL <- c(first_member, last_member)
  return(FL)
}

# Checking upper part rings
# [I]-Checking the random pixel "552300", where ring_no=525
# The function Ring_pix gives the ring number, total number of pixels and the member pixels into which the given pixel belongs
Ring_pix <- function(CMB_row) {
  if (CMB_row <= Tot_ULPRP) {
    Ring_no <- Upperpolar_Ringno(CMB_row)
    Tot_pixelR <- (4 * Ring_no)
    FLm <- c(Upperpolar_firstlast(Ring_no))
    MemberP <- c(seq(FLm[1], FLm[2], 1))
    print("Upper Polar")
  } else if (CMB_row > Tot_UMPRP) {
    Ring_no <- Lowerpolar_Ringno(CMB_row)
    Tot_pixelR <- (4 * (Tot_Ring - Ring_no + 1))
    FLm <- c(Lowerpolar_firstlast(Ring_no))
    MemberP <- c(seq(FLm[1], FLm[2], 1))
    print("Lower polar")
  } else {
    Ring_no <- Middlepolar_Ringno(CMB_row)
    Tot_pixelR <- (4 * N_side)
    FLm <- c(Middlepolar_firstlast(Ring_no))
    MemberP <- c(seq(FLm[1], FLm[2], 1))
    print("Middle part")
  }
  return(cbind(Ring_no, Tot_pixelR, MemberP))
}
Ring_info <- data.frame(Ring_pix(552300))
Ring_no <- Ring_info$Ring_no[1] 
Tot_pixelR <- Ring_info$Tot_pixelR[1] 
MemberP <- Ring_info$MemberP 

# Once we know the ring number of the given pixel, finding the radius of the ring into which the given pixel belongs
# Let Rd be the ring deviation from center
Rd <- abs(Ring_no - (2 * N_side))

# Let R be the unit radius of the sphere
R <- 1

# Let Rp be the radius of the ring into which the given pixel belongs
Rp <- (R * cos((pi / (4 * N_side)) * Rd))

# Finding the number of pixels in the half-circle
N1 <- round((Tot_pixelR / (2)), 0)

# Let lp be the distance between two pixels in this interval
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D1 <- rep(0, N2)
HExp1D1c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D1[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D1c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D1
min(HExp1D1)
max(HExp1D1)
max(HExp1D1) - min(HExp1D1)
mean(HExp1D1)

HExp1D1c
min(HExp1D1c)
max(HExp1D1c)
max(HExp1D1c) - min(HExp1D1c)
mean(HExp1D1c)

# This figure gives the plot of scaled CMB intensities of ring 525
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 525
plot(x = df1, y = HExp1D1, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 525 by R/S method
plot(x = df1, y = HExp1D1c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [II]-Checking the random pixel "1533000", where ring_no=875
Ring_info <- data.frame(Ring_pix(1533000))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D2 <- rep(0, N2)
HExp1D2c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D2[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D2c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D2
min(HExp1D2)
max(HExp1D2)
max(HExp1D2) - min(HExp1D2)
mean(HExp1D2)

HExp1D2c
min(HExp1D2c)
max(HExp1D2c)
max(HExp1D2c) - min(HExp1D2c)
mean(HExp1D2c)

# This figure gives the plot of scaled CMB intensities of ring 875
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 875
plot(x = df1, y = HExp1D2, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 875 by R/S method
plot(x = df1, y = HExp1D2c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [III]-Checking the random pixel "3253800", where ring_no=1275
Ring_info <- data.frame(Ring_pix(3253800))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D3 <- rep(0, N2)
HExp1D3c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D3[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D3c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D3
min(HExp1D3)
max(HExp1D3)
max(HExp1D3) - min(HExp1D3)
mean(HExp1D3)

HExp1D3c
min(HExp1D3c)
max(HExp1D3c)
max(HExp1D3c) - min(HExp1D3c)
mean(HExp1D3c)

# Figure 2(a)-This figure gives the plot of scaled CMB intensities of ring 1275
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# Figure 2(c)-This figure gives the plot of H values of ring 1275
plot(x = df1, y = HExp1D3, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 1275 by R/S method
plot(x = df1, y = HExp1D3c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# Checking middle part rings
# [IV]-Checking the random pixel "10047488", where ring_no=2250
Ring_info <- data.frame(Ring_pix(10047488))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd)) 
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D4 <- rep(0, N2)
HExp1D4c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D4[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D4c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D4
min(HExp1D4)
max(HExp1D4)
max(HExp1D4) - min(HExp1D4)
mean(HExp1D4)

HExp1D4c
min(HExp1D4c)
max(HExp1D4c)
max(HExp1D4c) - min(HExp1D4c)
mean(HExp1D4c)

# This figure gives the plot of scaled CMB intensities of ring 2250
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 2250
plot(x = df1, y = HExp1D4, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 2250 by R/S method
plot(x = df1, y = HExp1D4c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [V]-Choosing a pixel near the equator/great circle, where ring_no=5000
Ring_info <- data.frame(Ring_pix(32575488))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D5 <- rep(0, N2)
HExp1D5c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D5[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D5c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D5
min(HExp1D5)
max(HExp1D5)
max(HExp1D5) - min(HExp1D5)
mean(HExp1D5)

HExp1D5c
min(HExp1D5c)
max(HExp1D5c)
max(HExp1D5c) - min(HExp1D5c)
mean(HExp1D5c)

# This figure gives the plot of scaled CMB intensities of ring 5000
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 5000
plot(x = df1, y = HExp1D5, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 5000 by R/S method
plot(x = df1, y = HExp1D5c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [VI]-Checking the random pixel "39948288", where ring_no=5900
Ring_info <- data.frame(Ring_pix(39948288))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D6 <- rep(0, N2)
HExp1D6c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D6[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D6c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D6
min(HExp1D6)
max(HExp1D6)
max(HExp1D6) - min(HExp1D6)
mean(HExp1D6)

HExp1D6c
min(HExp1D6c)
max(HExp1D6c)
max(HExp1D6c) - min(HExp1D6c)
mean(HExp1D6c)

# Figure 2(b)-This figure gives the plot of scaled CMB intensities of ring 5900
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# Figure 2(d)-This figure gives the plot of H values of ring 5900
plot(x = df1, y = HExp1D6, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 5900 by R/S method
plot(x = df1, y = HExp1D6c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# Checking lower part rings
# [VII]-Checking the random pixel "47656664", where ring_no=7035
Ring_info <- data.frame(Ring_pix(47656664))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D7 <- rep(0, N2)
HExp1D7c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D7[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D7c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D7
min(HExp1D7)
max(HExp1D7)
max(HExp1D7) - min(HExp1D7)
mean(HExp1D7)

HExp1D7c
min(HExp1D7c)
max(HExp1D7c)
max(HExp1D7c) - min(HExp1D7c)
mean(HExp1D7c)

# This figure gives the plot of scaled CMB intensities of ring 7035
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7035
plot(x = df1, y = HExp1D7, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7035 by R/S method
plot(x = df1, y = HExp1D7c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [VIII]-Checking the random pixel "48651704, where ring_no=7275
Ring_info <- data.frame(Ring_pix(48651704))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D8 <- rep(0, N2)
HExp1D8c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D8[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D8c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D8
min(HExp1D8)
max(HExp1D8)
max(HExp1D8) - min(HExp1D8)
mean(HExp1D8)

HExp1D8c
min(HExp1D8c)
max(HExp1D8c)
max(HExp1D8c) - min(HExp1D8c)
mean(HExp1D8c)

# This figure gives the plot of scaled CMB intensities of ring 7275
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7275
plot(x = df1, y = HExp1D8, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7275 by R/S method
plot(x = df1, y = HExp1D8c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# [XI]-Checking the random pixel "49375304", where ring_no=7500
Ring_info <- data.frame(Ring_pix(49375304))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP

Rd <- abs(Ring_no - (2 * N_side))
R <- 1
Rp <- (R * cos((pi / (4 * N_side)) * Rd))
N1 <- round((Tot_pixelR / (2)), 0)
lp <- (1 / N1)

r <- 0.08
gamma <- (-(log(r) / log(N1)))
Pix_int <- as.integer(r / lp)
MemberP1 <- MemberP[(Pix_int + 1):((Pix_int) + N1)]
Int2 <- Int1[MemberP1]
df1 <- MemberP1[(Pix_int + 1):(N1 - (Pix_int))]
N2 <- length(df1)

HExp1D9 <- rep(0, N2)
HExp1D9c <- rep(0, N2)
l <- 1
for (CMB_pix in 1:N2) {
  N <- length(seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1))
  A <- seq(df1[CMB_pix] - Pix_int, df1[CMB_pix] + Pix_int, 1)
  tot <- 0
  for (j in 1:(N - 2)) {
    Increment1 <- (((Int1[as.integer(A[j])]) - 2 * (Int1[as.integer(A[j + 1])]) + (Int1[as.integer(A[j + 2])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp1D9[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  HExp1D9c[l] <- RoverS(Int1[A])
  l <- l + 1
}
HExp1D9
min(HExp1D9)
max(HExp1D9)
max(HExp1D9) - min(HExp1D9)
mean(HExp1D9)

HExp1D9c
min(HExp1D9c)
max(HExp1D9c)
max(HExp1D9c) - min(HExp1D9c)
mean(HExp1D9c)

# This figure gives the plot of scaled CMB intensities of ring 7500
plot(x = MemberP1, y = Int1[MemberP1], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7500
plot(x = df1, y = HExp1D9, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# This figure gives the plot of H values of ring 7500 by R/S method
plot(x = df1, y = HExp1D9c, xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

end.time <- Sys.time()
time.taken <- end.time - start.time

save.image(file = "HolderExponent1D_2048.RData")
load("HolderExponent1D_2048.RData")