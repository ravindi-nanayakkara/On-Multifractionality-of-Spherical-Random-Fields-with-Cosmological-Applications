library(rcosmo)

start.time <- Sys.time()

# Link to download the CMB data set with resolution 2048
# URL = 'http://irsa.ipac.caltech.edu/data/Planck/release_2/all-sky-maps/maps/component-maps/cmb/COM_CMB_IQU-smica-field-Int_2048_R2.01_full.fits'
# downloadCMBMap(foreground = "smica", nside = 2048)

# Generating CMB data frame with nside=2048
cmbdf <- CMBDataFrame("CMB_map_smica2048.fits", ordering = "ring")
df1 <- coords(cmbdf, new.coords = "spherical")

# CMB_coord-coordinates of the real CMB data
CMB_coord <- data.frame(cbind(theta1 = df1$theta, phi1 = df1$phi))

# Storing the intensities of pixels into a vector
Int1 <- cmbdf$I

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

# Equator ring analysis
# Checking the random pixel "25161729", where Ring_no = 4096
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
# Equator ring
CMB_row <- (2 * (N_side) * (N_side - 1)) + (4 * ((N_side)^2)) + 1

Ring_info <- data.frame(Ring_pix(25161729))
Ring_no <- Ring_info$Ring_no[1]
Tot_pixelR <- Ring_info$Tot_pixelR[1]
MemberP <- Ring_info$MemberP
mean(cmbdf$I[MemberP])

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

HExp1De <- rep(0, N2)
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
  HExp1De[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N1)))
  l <- l + 1
}
HExp1De
min(HExp1De)
max(HExp1De)
max(HExp1De) - min(HExp1De)
mean(HExp1De)

# Figure 8(a)-This figure gives the plot of scaled CMB intensities of ring 4096
plot(x = MemberP1[721:3769], y = Int1[MemberP1[721:3769]], xlab = "CMB pixel", ylab = "I", type = "l", lwd = 1, col = "blue")

# Figure 8(b)-This figure gives the plot of H values of ring 4096
plot(x = df1[394:3442], y = HExp1De[394:3442], xlab = "CMB pixel", ylab = "H", type = "l", lwd = 1, col = "blue")

# The Holder exponent range which is constant = HExp1De[826:1470]
# The CMB pixel range in the original file = 25163208:25163852

# Table 6-Table 6 gives the analysis of CMB intensities near the equator region
CMB_coord_abnormal <- CMB_coord[25163208:25163852, ]
min(cmbdf$I[25163208:25163852])
max(cmbdf$I[25163208:25163852])
max(cmbdf$I[25163208:25163852]) - min(Int1[25163208:25163852])
mean(cmbdf$I[25163208:25163852])
var(cmbdf$I[25163208:25163852])

min(HExp1De[826:1470])
max(HExp1De[826:1470])
max(HExp1De[826:1470]) - min(HExp1De[826:1470])
mean(HExp1De[826:1470])

# Finding the galactic coordinates of the unusual H range in one-dimensional space
# Finding the first pair of coordinates in the unusual H range
l1 <- 65.02
b1 <- (90 - 89.99)
# (l1,b1) = (65.02, 0.01)

# Finding the last pair of coordinates in the unusual H range
l2 <- 93.32
b2 <- (90 - 89.99)
# (l2,b2) = (93.32, 0.01)

# One-dimensional region excluding the region of unusual values
Remain_int <- c(df1[1:825], df1[1471:3442])
min(cmbdf$I[Remain_int])
max(cmbdf$I[Remain_int])
max(cmbdf$I[Remain_int]) - min(Int1[Remain_int])
mean(cmbdf$I[Remain_int])
var(cmbdf$I[Remain_int])

Remain_H <- c(HExp1De[1:825], HExp1De[1471:3442])
min(Remain_H)
max(Remain_H)
max(Remain_H) - min(Remain_H)
mean(Remain_H)

save.image(file = "HolderExponent1De1_2048.RData")
load("HolderExponent1De1_2048.RData")