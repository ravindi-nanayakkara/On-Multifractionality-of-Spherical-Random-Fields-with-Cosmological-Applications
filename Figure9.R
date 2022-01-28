library(rcosmo)
library(rgl)

start.time <- Sys.time()

# Link to download the CMB data set with resolution 2048
# URL = 'http://irsa.ipac.caltech.edu/data/Planck/release_2/all-sky-maps/maps/component-maps/cmb/COM_CMB_IQU-smica-field-Int_2048_R2.01_full.fits'
# downloadCMBMap(foreground = "smica", nside = 2048)

cmbdf <- CMBDataFrame("CMB_map_smica2048.fits", include.masks = TRUE)
cmbdf$TMASK1 <- (1 - cmbdf$TMASK) + cmbdf$I

# Figure 9(a)-This figure gives the plot of non-inpainted Planck 2015 CMB map with the anomalous sky window
plot(cmbdf, intensities = "TMASK1", back.col = "white", ylab = "", xlab = "", zlab = "")
win_eq <- CMBWindow(theta = c(1.599772, 1.599772, 1.500099, 1.500099), phi = c(1.340699, 1.499466, 1.499466, 1.340699))
plot(win_eq, col = "white", lwd = 3)

cmbdf_eq <- window(cmbdf, new.window = win_eq)
# Figure 9(b)-This figure gives the plot of the enlarged anomalous sky window
plot(cmbdf_eq, back.col = "white", ylab = "", xlab = "", zlab = "")