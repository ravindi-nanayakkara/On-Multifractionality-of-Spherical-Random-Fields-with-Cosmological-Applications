library(rcosmo)
library(rgl)
library(akima)

start.time <- Sys.time()

# Link to download the CMB data set with resolution 2048
# URL = 'http://irsa.ipac.caltech.edu/data/Planck/release_2/all-sky-maps/maps/component-maps/cmb/COM_CMB_IQU-smica-field-Int_2048_R2.01_full.fits'
# downloadCMBMap(foreground = "smica", nside = 2048)

# Generating CMB data frame with nside=2048
cmbdf <- CMBDataFrame("CMB_map_smica2048.fits")
df1 <- coords(cmbdf, new.coords = "cartesian")

# CMB_coord-coordinates of the real CMB data
CMB_coord <- data.frame(cbind(x = df1$x, y = df1$y, z = df1$z))

# Storing the intensities of pixels into a vector
Int1 <- cmbdf$I
Int1 <- Int1 / max(abs(Int1))

# k is changing with nside. If nside=2048=2^(11)=2^(k). Therefore, k=11
k <- 11

# d-dimension
d <- 2

# N_pix is the no. of pixels in which we compute the Holder exponent values
N_pix <- 1000

# For the equator region
CMB_row <- (23439718)
win <- CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23)
cmbdf11 <- window(cmbdf, new.window = win)
length(pix(cmbdf11))
min(cmbdf11$I)
max(cmbdf11$I)
avg1 <- mean(cmbdf11$I)
df11e <- coords(cmbdf11, new.coords = "cartesian")
df_sample1e <- df11e[seq(1, length(pix(cmbdf11)), by = (length(pix(cmbdf11)) / N_pix)), ]
df12e <- coords(df_sample1e, new.coords = "cartesian")
df13e <- data.frame(cbind(x = df12e$x, y = df12e$y, z = df12e$z))

r <- 0.01
N11e <- length(pix(cmbdf11))
N12e <- sqrt(N11e)
gamma <- (-(log((sqrt(pi) * r) / 2) / log((N12e))))
HExp2De <- rep(0, N_pix)
l <- 1
for (CMB_row in 1:N_pix) {
  win1 <- CMBWindow(x = df13e[CMB_row, ]$x, y = df13e[CMB_row, ]$y, z = df13e[CMB_row, ]$z, r = 0.01)
  cmbdf1 <- window(cmbdf, new.window = win1)
  N <- length(pix(cmbdf1))
  Pix_number <- as.integer(pix(cmbdf1))
  tot <- 0
  for (i in 1:N) {
    A <- neighbours(Pix_number[i], k)
    B <- neighbours(as.integer(A[1]), k)
    Increment1 <- (((Int1[as.integer(B[1])]) - (2 * Int1[as.integer(B[2])]) + (Int1[as.integer(B[3])]) -
      (2 * Int1[as.integer(B[4])]) + (Int1[as.integer(B[5])]) - (2 * Int1[as.integer(B[6])]) +
      (Int1[as.integer(B[7])]) - (2 * Int1[as.integer(B[8])]) + (4 * Int1[as.integer(B[9])]))^2)
    tot <- tot + Increment1
  }
  VNt <- tot
  HExp2De[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N12e)))
  l <- l + 1
}
HExp2De
min(HExp2De)
max(HExp2De)
max(HExp2De) - min(HExp2De)
mean(HExp2De)

df14e <- data.frame(coords(df_sample1e, new.coords = "spherical"))
n_interpolation <- 500

x <- df14e$theta
y <- df14e$phi
z <- HExp2De

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 8(d)-This figure gives the plot of $\hat{H}(t)$ values from the equator region
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "H", col = color[zcol])

df_sample2 <- df11e[seq(1, length(pix(cmbdf11)), by = (length(pix(cmbdf11)) / 100000)), ]
df15 <- data.frame(coords(df_sample2, new.coords = "spherical"))
n_interpolation <- 500

x <- df15$theta
y <- df15$phi
z <- df15$I / max(abs(df15$I))

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 8(c)-This figure gives the plot of scaled CMB intensities of the equator region
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "I", col = color[zcol])

df16 <- coords(cmbdf, new.coords = "spherical")
df17 <- data.frame(cbind(theta1 = df16$theta, phi1 = df16$phi))
equi_region <- data.frame(subset(df17, (theta1 > 1.50 & theta1 < 1.60) & (phi1 > 1.34 & phi1 < 1.50), select = c(theta1, phi1)))

min(equi_region$phi1)
max(equi_region$phi1)

min(equi_region$theta1)
max(equi_region$theta1)

# Finding the galactic coordinates of the unusual H range in two-dimensional space
# Finding the first pair of coordinates in the unusual H range of spherical surface,theta1=1.599772, phi1=1.499466
# Pix_value = 23404309
l1 <- 85.91
b1 <- (90 - 91.66)
# (l1,b1) = (85.91, -1.66)

# Finding the second pair of coordinates in the unusual H range of spherical surface, theta1=1.599772, phi1=1.340699
# Pix_value = 23391936
l2 <- 76.82
b2 <- (90 - 91.66)
# (l2,b2) = (76.82, -1.66)

# Finding the third pair of coordinates in the unusual H range of spherical surface, theta1=1.500099, phi1=1.340699
# Pix_value = 23564929
l3 <- 76.82
b3 <- (90 - 85.95)
# (l3,b3) = (76.82, 4.05)

# Finding the final pair of coordinates in the unusual H range of spherical surface, theta1=1.500099, phi1=1.499466
# Pix_value = 24158424
l4 <- 85.91
b4 <- (90 - 85.95)
# (l4,b4) = (85.91, 4.05)

# Table 6 - Table 6 gives the analysis of CMB intensities near the equator region
equidata_total <- cbind(coords(df_sample1e, new.coords = "spherical"), H = HExp2De)

# Analysis of two-dimensional region around unusual values
equidata_unusual <- data.frame(subset(equidata_total, (theta > 1.50 & theta < 1.60) & (phi > 1.34 & phi < 1.50), select = c(theta, phi, I, H)))

min(equidata_unusual$I)
max(equidata_unusual$I)
max(equidata_unusual$I) - min(equidata_unusual$I)
avg_eq <- mean(equidata_unusual$I)
var_eq <- var(equidata_unusual$I)

min(equidata_unusual$H)
max(equidata_unusual$H)
max(equidata_unusual$H) - min(equidata_unusual$H)
mean(equidata_unusual$H)

# Analysis of two-dimensional region excluding the region of unusual values
win_eq_exterior <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699), set.minus = TRUE)
CMB_row <- (23439718)
win_remain <- list(win_eq_exterior, CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23))
cmbdf_eqrem <- window(cmbdf, new.window = win_remain)
plot(cmbdf_eqrem)

equidata_remain <- data.frame(subset(equidata_total, !((theta > 1.50 & theta < 1.60) & (phi > 1.34 & phi < 1.50)), select = c(theta, phi, I, H)))

min(equidata_remain$I)
max(equidata_remain$I)
max(equidata_remain$I) - min(equidata_remain$I)
avg_eqr <- mean(equidata_remain$I)
var_eqr <- var(equidata_remain$I)

min(equidata_remain$H)
max(equidata_remain$H)
max(equidata_remain$H) - min(equidata_remain$H)
mean(equidata_remain$H)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

save.image(file = "H-2Deqregion2-smica2048.RData")
load("H-2Deqregion2-smica2048.RData")