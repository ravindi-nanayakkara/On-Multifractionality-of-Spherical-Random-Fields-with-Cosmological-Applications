library(rcosmo)
library(rgl)

# This function plots the neighbouring pixels for a given base pixel
demoNeighbours <- function(p, j, fcol) {
  neighbours(p, j)
  displayPixels(
    boundary.j = j, j = j, plot.j = j + 3,
    spix = neighbours(p, j),
    boundary.col = "grey48",
    boundary.lwd = 1,
    incl.labels = neighbours(p, j),
    col = toString(fcol),
    size = 3
  )
}

# Figure 4-This figure gives the plot of examples of pixels with 7 and 8 neighbours for N_side=4
# Plotting the neighbouring pixels for pixel index 6 at N_side=4
demoNeighbours(6, 2, 3)

# Plotting the neighbouring pixels for pixel index 72 at N_side=4
demoNeighbours(72, 2, 4)

um <- matrix(c(
  0.7560294, -0.6384937, -0.1440324, 0,
  -0.1466970, -0.3797444, 0.9133860, 0,
  -0.6378868, -0.6694176, -0.3807631, 0,
  0.00000000, 0.00000000, 0.0000000, 1
), byrow = TRUE, nrow = 4, ncol = 4)
view3d(userMatrix = um)
clipplanes3d(1, 1, 0.65, d = 0.6)
rgl.snapshot("Figure4.png")

start.time1 <- Sys.time()

# Generating CMB data frame with nside=2048
Nside <- 2048
df <- CMBDataFrame(
  nside = Nside, I = rep(0, 12 * Nside^2),
  ordering = "nested"
)
N <- 12 * Nside^2
j1 <- as.integer(log(Nside) / log(2))

# This function computes the number of neighbouring pixels of a given pixel
fincr2d <- function(x) {
  length(neighbours(x, j1))
}

# Vectorizing the "fincr2d" function
fincr2dv <- Vectorize(fincr2d)

# Computing the number of neighbouring pixels of all pixel indices
df$I <- fincr2dv(1:N)

# Finding the number of pixels having 8 and 9 neighbours respectively
sum(df$I == 8)
sum(df$I == 9)

end.time1 <- Sys.time()
time.taken1 <- end.time1 - start.time1