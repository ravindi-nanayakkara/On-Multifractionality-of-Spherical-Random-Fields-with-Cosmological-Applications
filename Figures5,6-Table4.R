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

# CMB_coord-coordinates of the CMB data
CMB_coord <- data.frame(cbind(x = df1$x, y = df1$y, z = df1$z))

# Storing the intensities of pixels into a vector
Int1 <- cmbdf$I
# Computing the scaled CMB intensities
Int1 <- Int1 / max(abs(Int1))

# k is changing with nside. If nside=2048=2^(11)=2^(k). Therefore, k=11
k <- 11

# d-dimension
d <- 2

# N_pix is the no. of pixels in which we compute the Holder exponent values
N_pix <- 1000

# For a warm region
start.time1 <- Sys.time()

# Figure 5(a)-This figure gives the plot of a sky window from the warm region
plot(cmbdf, back.col = "white", ylab = "", xlab = "", zlab = "")
CMB_row <- (29990264)
win <- CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23)
plot(win, col = "red", lwd = 3)

cmbdf11 <- window(cmbdf, new.window = win)
length(pix(cmbdf11))
min(cmbdf11$I)
max(cmbdf11$I)
avg1 <- mean(cmbdf11$I)
df11 <- coords(cmbdf11, new.coords = "cartesian")
df_sample1 <- df11[seq(1, length(pix(cmbdf11)), by = (length(pix(cmbdf11)) / N_pix)), ]
df12 <- coords(df_sample1, new.coords = "cartesian")
df13 <- data.frame(cbind(x = df12$x, y = df12$y, z = df12$z))

r <- 0.01
N11 <- length(pix(cmbdf11))
N12 <- sqrt(N11)
gamma <- (-(log((sqrt(pi) * r) / 2) / log((N12))))
HExp2D1 <- rep(0, N_pix)
l <- 1
for (CMB_row in 1:N_pix) {
  win1 <- CMBWindow(x = df13[CMB_row, ]$x, y = df13[CMB_row, ]$y, z = df13[CMB_row, ]$z, r = 0.01)
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
  HExp2D1[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N12)))
  l <- l + 1
}
HExp2D1
min(HExp2D1)
max(HExp2D1)
max(HExp2D1) - min(HExp2D1)
mean(HExp2D1)

df14 <- coords(df_sample1, new.coords = "spherical")
n_interpolation <- 500

x <- df14$theta
y <- df14$phi
z <- HExp2D1

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 6(a)-This figure gives the plot of $\hat{H}(t)$ values from the warm region
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "H", col = color[zcol])

end.time1 <- Sys.time()
time.taken1 <- end.time1 - start.time1
time.taken1

# For a cold region
start.time2 <- Sys.time()

# Figure 5(b)-This figure gives the plot of a sky window from the cold region
plot(cmbdf, back.col = "white", ylab = "", xlab = "", zlab = "")
CMB_row <- (45045200)
win <- CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23)
plot(win, col = "red", lwd = 3)

cmbdf12 <- window(cmbdf, new.window = win)
length(pix(cmbdf12))
min(cmbdf12$I)
max(cmbdf12$I)
avg2 <- mean(cmbdf12$I)
df21 <- coords(cmbdf12, new.coords = "cartesian")
df_sample2 <- df21[seq(1, length(pix(cmbdf12)), by = (length(pix(cmbdf12)) / N_pix)), ]
df22 <- coords(df_sample2, new.coords = "cartesian")
df23 <- data.frame(cbind(x = df22$x, y = df22$y, z = df22$z))

r <- 0.01
N21 <- length(pix(cmbdf12))
N22 <- sqrt(N21)
gamma <- (-(log((sqrt(pi) * r) / 2) / log((N22))))
HExp2D2 <- rep(0, N_pix)
l <- 1
for (CMB_row in 1:N_pix) {
  win1 <- CMBWindow(x = df23[CMB_row, ]$x, y = df23[CMB_row, ]$y, z = df23[CMB_row, ]$z, r = 0.01)
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
  HExp2D2[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N22)))
  l <- l + 1
}
HExp2D2
min(HExp2D2)
max(HExp2D2)
max(HExp2D2) - min(HExp2D2)
mean(HExp2D2)

df24 <- coords(df_sample2, new.coords = "spherical")
n_interpolation <- 500

x <- df24$theta
y <- df24$phi
z <- HExp2D2

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 6(b)-This figure gives the plot of $\hat{H}(t)$ values from the cold region
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "H", col = color[zcol])

end.time2 <- Sys.time()
time.taken2 <- end.time2 - start.time2
time.taken2

# For a mixture of warm and cold regions
start.time3 <- Sys.time()

# Figure 5(c)-This figure gives the plot of a sky window from the mixture region
plot(cmbdf, back.col = "white", ylab = "", xlab = "", zlab = "")
CMB_row <- (25163208)
win <- CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23)
plot(win, col = "red", lwd = 3)

cmbdf13 <- window(cmbdf, new.window = win)
length(pix(cmbdf13))
min(cmbdf13$I)
max(cmbdf13$I)
avg3 <- mean(cmbdf13$I)
df31 <- coords(cmbdf13, new.coords = "cartesian")
df_sample3 <- df31[seq(1, length(pix(cmbdf13)), by = (length(pix(cmbdf13)) / N_pix)), ]
df32 <- coords(df_sample3, new.coords = "cartesian")
df33 <- data.frame(cbind(x = df32$x, y = df32$y, z = df32$z))

r <- 0.01
N31 <- length(pix(cmbdf13))
N32 <- sqrt(N31)
gamma <- (-(log((sqrt(pi) * r) / 2) / log((N32))))
HExp2D3 <- rep(0, N_pix)
l <- 1
for (CMB_row in 1:N_pix) {
  win1 <- CMBWindow(x = df33[CMB_row, ]$x, y = df33[CMB_row, ]$y, z = df33[CMB_row, ]$z, r = 0.01)
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
  HExp2D3[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N32)))
  l <- l + 1
}
HExp2D3
min(HExp2D3)
max(HExp2D3)
max(HExp2D3) - min(HExp2D3)
mean(HExp2D3)

df34 <- coords(df_sample3, new.coords = "spherical")
n_interpolation <- 500

x <- df34$theta
y <- df34$phi
z <- HExp2D3

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 6(c)-This figure gives the plot of $\hat{H}(t)$ values from the region with mixture of temperatures
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "H", col = color[zcol])

end.time3 <- Sys.time()
time.taken3 <- end.time3 - start.time3
time.taken3

# For a borderline region
start.time4 <- Sys.time()

# Figure 5(d)-This figure gives the plot of a sky window from the borderline region
plot(cmbdf, back.col = "white", ylab = "", xlab = "", zlab = "")
CMB_row <- (42662192)
win <- CMBWindow(x = df1[CMB_row, ]$x, y = df1[CMB_row, ]$y, z = df1[CMB_row, ]$z, r = 0.23)
plot(win, col = "red", lwd = 3)

cmbdf14 <- window(cmbdf, new.window = win)
length(pix(cmbdf14))
min(cmbdf14$I)
max(cmbdf14$I)
avg4 <- mean(cmbdf14$I)
df41 <- coords(cmbdf14, new.coords = "cartesian")
df_sample4 <- df41[seq(1, length(pix(cmbdf14)), by = (length(pix(cmbdf14)) / N_pix)), ]
df42 <- coords(df_sample4, new.coords = "cartesian")
df43 <- data.frame(cbind(x = df42$x, y = df42$y, z = df42$z))

r <- 0.01
N41 <- length(pix(cmbdf14))
N42 <- sqrt(N41)
gamma <- (-(log((sqrt(pi) * r) / 2) / log((N42))))
HExp2D4 <- rep(0, N_pix)
l <- 1
for (CMB_row in 1:N_pix) {
  win1 <- CMBWindow(x = df43[CMB_row, ]$x, y = df43[CMB_row, ]$y, z = df43[CMB_row, ]$z, r = 0.01)
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
  HExp2D4[l] <- (1 / 2) * ((d * (1 - gamma)) - (log(VNt) / log(N42)))
  l <- l + 1
}
HExp2D4
min(HExp2D4)
max(HExp2D4)
max(HExp2D4) - min(HExp2D4)
mean(HExp2D4)

df44 <- coords(df_sample4, new.coords = "spherical")
n_interpolation <- 500

x <- df44$theta
y <- df44$phi
z <- HExp2D4

spline_interpolated <- interp(x, y, z, xo = seq(min(x), max(x), length = n_interpolation), yo = seq(min(y), max(y), length = n_interpolation))

x.si <- spline_interpolated$x
y.si <- spline_interpolated$y
z.si <- spline_interpolated$z

nbcol <- 50
color <- rev(rainbow(nbcol, start = 0, end = 1))
zcol <- cut(z.si, nbcol)

# Figure 6(d)-This figure gives the plot of $\hat{H}(t)$ values from the borderline region
persp3d(x.si, y.si, z.si, xlab = expression(theta), ylab = expression(varphi), zlab = "H", col = color[zcol])

end.time4 <- Sys.time()
time.taken4 <- end.time4 - start.time4
time.taken4

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

save.image(file = "HolderExponent2Dall_2048.RData")
load("HolderExponent2Dall_2048.RData")