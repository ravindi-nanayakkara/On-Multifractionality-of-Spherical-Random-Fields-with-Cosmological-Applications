library(rcosmo)
library(rgl)

# Figure 1(a)-This figure gives the plot of the HEALPix ring ordering visualization
# Generating a CMB data frame with nside=8 and "ring" ordering
cmbdf <- CMBDataFrame(nside = 8, ordering = "ring")
plot(cmbdf, type = "l", col = "black", back.col = "white", xlab = "", ylab = "", zlab = "")
# Labeling the HEALPix values 1, 100:107 and 768
tolabel <- c(1, 100:107, 768)
plot(cmbdf[tolabel, ], labels = tolabel, col = "red", add = TRUE)

um <- matrix(c(
  0.8948848, -0.4459228, -0.01829224, 0,
  0.1114479, 0.1835916, 0.97666484, 0,
  -0.4321588, -0.8760409, 0.21399030, 0,
  0.0000000, 0.0000000, 0.0000000, 1
), byrow = TRUE, nrow = 4, ncol = 4)

view3d(userMatrix = um)
rgl.snapshot("Figure1a.png")

# Figure 1(b)-This figure gives the plot of the HEALPix nested ordering visualization with nside=2
ns <- 256
rand <- rnorm(12 * ns^2)
cmbdf <- CMBDataFrame(nside = 64, I = rnorm(12 * 64 ^ 2), ordering = "nested")
w21 <- window(CMBDataFrame(nside = ns, I = rand, ordering = "nested"), in.pixels = 1)
displayPixelBoundaries(nside = 2, ordering = "nested", incl.labels = c(1, 2, 3, 4), nums.col = "red", col = "black")

plot(w21, col = "light blue", back.col = "white", add = TRUE, size = 1.2)
plot(window(cmbdf, in.pixels = 2), col = "green", add = TRUE)
plot(window(cmbdf, in.pixels = 4), col = "purple", add = TRUE)
plot(window(cmbdf, in.pixels = 5), col = "orange", add = TRUE)
plot(window(cmbdf, in.pixels = 6), col = "red", add = TRUE)

um <- matrix(c(
  -0.6893843, 0.7242531, -0.01436349, 0,
  -0.5146448, -0.4757210, 0.71332407, 0,
  0.5097938, 0.4991464, 0.70068759, 0,
  0.0000000, 0.0000000, 0.0000000, 1
), byrow = TRUE, nrow = 4, ncol = 4)
view3d(userMatrix = um)
rgl.snapshot("Figure1b.png")
