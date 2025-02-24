
##Simulate the MCMC posterior samples
library(ggplot2)
# Set seed for reproducibility
set.seed(123)
# Subset posterior data (remove burn-in period)
posterior1 <- ott50000.139[-(1:10000)] 
posterior2 <- ptt50000.148[-(1:10000)] 
posterior3 <- itt50000.110[-(1:10000)] 

# Density estimation
dens1 <- density(posterior1, n = 512)
dens2 <- density(posterior2, n = 512)
dens3 <- density(posterior3, n = 512)
# Determine the common range for interpolation
#common_x <- seq(max(min(dens1$x), min(dens2$x), min(dens3$x)), min(max(dens1$x), max(dens2$x), max(dens3$x)), length.out = 512)
#Normalize the Densities
dens1$y <- dens1$y / max(dens1$y)
dens2$y <- dens2$y / max(dens2$y)
dens3$y <- dens3$y / max(dens3$y)
# Interpolate densities at common_x points using approx()
common_dens1 <- approx(dens1$x, dens1$y, xout = common_x, rule = 2)$y
common_dens2 <- approx(dens2$x, dens2$y, xout = common_x, rule = 2)$y
common_dens3 <- approx(dens3$x, dens3$y, xout = common_x, rule = 2)$y
# Clip small values after interpolation
numerical_threshold <- 1e-10
common_dens1[common_dens1 < numerical_threshold] <- 0
common_dens2[common_dens2 < numerical_threshold] <- 0
common_dens3[common_dens3 < numerical_threshold] <- 0
# Handle NA values (if any)
common_dens1[is.na(common_dens1)] <- 0
common_dens2[is.na(common_dens2)] <- 0
common_dens3[is.na(common_dens3)] <- 0

# Function to calculate KL Divergence (Discrete Approximation)
kl_divergence12 <- sum(common_dens1 * log(common_dens1 / common_dens2), na.rm = TRUE)
kl_divergence13 <- sum(common_dens1 * log(common_dens1 / common_dens3), na.rm = TRUE)
kl_divergence23 <- sum(common_dens2 * log(common_dens2 / common_dens3), na.rm = TRUE)
# Function to calculate Hellinger Distance
hellinger_distance12 <- sqrt(1 - sum(sqrt(common_dens1 * common_dens2)))
# Function to calculate Total Variation Distance
total_variation_distance12 <- sum(abs(common_dens1 - common_dens2)) / 2

# Print results
cat("KL Divergence (Discrete Approximation):", kl_divergence, "\n")
cat("Hellinger Distance:", hellinger_distance, "\n")
cat("Total Variation Distance:", total_variation_distance, "\n")


# Optional
#Smooth densities using a moving average
smooth_dens1 <- filter(common_dens1, rep(1/5, 5), sides = 2)
smooth_dens2 <- filter(common_dens2, rep(1/5, 5), sides = 2)
smooth_dens3 <- filter(common_dens3, rep(1/5, 5), sides = 2)

# Ensure no NAs after smoothing
smooth_dens1[is.na(smooth_dens1)] <- 0
smooth_dens2[is.na(smooth_dens2)] <- 0
smooth_dens3[is.na(smooth_dens3)] <- 0

# Add a small epsilon to prevent numerical issues
epsilon <- 1e-10
smooth_dens1 <- smooth_dens1 + epsilon
smooth_dens2 <- smooth_dens2 + epsilon
smooth_dens3 <- smooth_dens3 + epsilon

# Normalize the smoothed densities
smooth_dens1 <- smooth_dens1 / sum(smooth_dens1)
smooth_dens2 <- smooth_dens2 / sum(smooth_dens2)
smooth_dens3 <- smooth_dens3 / sum(smooth_dens3)

# proceed with the KL divergence calculation on the cleaned densities
epsilon <- 1e-10
kl_divergence_12 <- sum(smooth_dens1 * log((smooth_dens1 + epsilon) / (smooth_dens2 + epsilon)), na.rm = TRUE)
kl_divergence_13 <- sum(smooth_dens1 * log((smooth_dens1 + epsilon) / (smooth_dens3 + epsilon)), na.rm = TRUE)
kl_divergence_23 <- sum(smooth_dens2 * log((smooth_dens2 + epsilon) / (smooth_dens3 + epsilon)), na.rm = TRUE)

# Modify the Hellinger distance calculation to prevent precision issues
epsilon <- 1e-10
hellinger_distance_12 <- sqrt(1 - sum(sqrt((common_dens1 + epsilon) * (common_dens2 + epsilon))))
hellinger_distance_13 <- sqrt(1 - sum(sqrt((common_dens1 + epsilon) * (common_dens3 + epsilon))))
hellinger_distance_23 <- sqrt(1 - min(sum(sqrt((common_dens2 + epsilon) * (common_dens3 + epsilon))), 1 - epsilon))   # Clamp the sum to a maximum of 1 to avoid precision issues

# Total Variation Distance Calculation
total_variation_distance_12 <- sum(abs(common_dens1 - common_dens2)) / 2
total_variation_distance_13 <- sum(abs(common_dens1 - common_dens3)) / 2
total_variation_distance_23 <- sum(abs(common_dens2 - common_dens3)) / 2

##########################################################################




