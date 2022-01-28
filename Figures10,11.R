# Figure 10(b)
library(rcosmo)
library(R.matlab)
library(rgl)

CMB_probe_smica <- readMat("ACDSMICA2015_L1500_HL1024_lag10.mat")
ACD_smica <- CMB_probe_smica[[1]]
cmbdf <- CMBDataFrame(nside = 1024, I = ACD_smica[1, ], ordering = "ring")
cmbdf$ACD1 <- ifelse(cmbdf$I > quantile(cmbdf$I, 0.95), 1, -1)

# Figure 10(b)-This figure gives the plot of the AC discrepancy map from SMICA 2015
plot(cmbdf, intensities = "ACD1", back.col = "white", ylab = "", xlab = "", zlab = "")
win_eq <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699))
plot(win_eq, col = "white", lwd = 3)

um <- matrix(c(
  -0.98451775, 0.17489178, -0.01173002, 0,
  -0.02096882, -0.05107139, 0.99847484, 0,
  0.17402618, 0.98326194, 0.05394801, 0,
  0.00000000, 0.00000000, 0.0000000, 1
), byrow = TRUE, nrow = 4, ncol = 4)
view3d(userMatrix = um)
rgl.snapshot("Figure10b.png")

# Figure 10(a)
library(rcosmo)
library(rgl)
library(RcppRoll)

# Link to download the CMB data set with resolution 2048
# URL = 'http://irsa.ipac.caltech.edu/data/Planck/release_2/all-sky-maps/maps/component-maps/cmb/COM_CMB_IQU-smica-field-Int_2048_R2.01_full.fits'
# downloadCMBMap(foreground = "smica", nside = 2048)

# Generating CMB data frame with nside=2048
cmbdf <- CMBDataFrame("CMB_map_smica2048.fits", ordering = "ring")

L <- length(cmbdf$I)
Int1 <- cmbdf$I
Scale <- max(abs(Int1 - mean(Int1)))
Int1 <- Int1 / max(abs(Int1))

N_side <- 2048

d <- 1
N1 <- 8192 / 2
# Let lp be the distance between two pixels in this interval
lp <- (1 / N1)

Pix_int <- 30
r <- Pix_int * lp
gamma <- (-(log(r) / log(N1)))

Increment1 <- rep(0, L - 2)
Increment1 <- (Int1[1:(L - 2)] - 2 * Int1[2:(L - 1)] + Int1[3:L])^2

anomshift <- roll_sum(Increment1, 2 * Pix_int + 1)
anom <- rep(mean(anomshift), L)
anom <- rep(1, L)
anom[(Pix_int + 1):(L - 2 - (Pix_int))] <- anomshift

HExp1De <- rep(0, L)
HExp1De <- (1 / 2) * ((d * (1 - gamma)) - (log(anom) / log(N1)))

cmbdf$H <- HExp1De - mean(HExp1De)
cmbdf$H1 <- ifelse(cmbdf$H < quantile(cmbdf$H, 0.05), 1, -1)

# Figure 10(a)-This figure gives the plot of the Holder exponent map from SMICA 2015
plot(cmbdf, intensities = "H1", back.col = "white", ylab = "", xlab = "", zlab = "")
win_eq <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699))
plot(win_eq, col = "white", lwd = 3)

um <- matrix(c(
  -0.98451775, 0.17489178, -0.01173002, 0,
  -0.02096882, -0.05107139, 0.99847484, 0,
  0.17402618, 0.98326194, 0.05394801, 0,
  0.00000000, 0.00000000, 0.0000000, 1
), byrow = TRUE, nrow = 4, ncol = 4)
view3d(userMatrix = um)
rgl.snapshot("Figure10a.png")

# Figure 11(a)
cmbdf$Had <- rep(0, L)

cmbdf$Had[1:(L - 20)] <- pmin(
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[11:(L - 10)]), abs(cmbdf$H[1:(L - 20)] - cmbdf$H[12:(L - 9)]), abs(cmbdf$H[1:(L - 20)] - cmbdf$H[13:(L - 8)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[14:(L - 7)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[15:(L - 6)]), abs(cmbdf$H[1:(L - 20)] - cmbdf$H[16:(L - 5)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[17:(L - 4)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[18:(L - 3)]), abs(cmbdf$H[1:(L - 20)] - cmbdf$H[19:(L - 2)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[20:(L - 1)]),
  abs(cmbdf$H[1:(L - 20)] - cmbdf$H[21:(L)])
)
cmbdf$H2 <- ifelse(cmbdf$Had > quantile(cmbdf$Had, 0.95), 1, -1)

# Figure 11(a)-This figure gives the plot of the ${\hat{H}}_{\Delta}$ discrepancy map from SMICA 2015
plot(cmbdf, intensities = "H2", back.col = "white", ylab = "", xlab = "", zlab = "")
win_eq <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699))
plot(win_eq, col = "white", lwd = 3)
view3d(userMatrix = um)
rgl.snapshot("Figure11a.png")

# Figure 11(b)
cmbdf1 <- CMBDataFrame("CMB_map_smica2048.fits", include.masks = TRUE, ordering = "ring")
cmbdf$H3 <- cmbdf$H2 * pmax(cmbdf1$TMASK, cmbdf$H2)

# Figure 11(b)-This figure gives the plot of the ${\hat{H}}_{\Delta}$ discrepancies over TMASK from SMICA 2015
plot(cmbdf, intensities = "H3", back.col = "white", ylab = "", xlab = "", zlab = "")
win_eq <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699))
plot(win_eq, col = "black", lwd = 3)
view3d(userMatrix = um)
rgl.snapshot("Figure11b.png")