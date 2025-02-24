#require(shapes)
#require(Morpho)
#require(BPviGM1)
#require(scales)
#library(readxl)
# Define parameters
On= 139     # Sample number in our data (C. oxystoma)
Pn= 148     # sample number in our data (C. peregrinus)
In= 110     #sample number in our data (C.innoxius)
# Initialize the arrays with one
#oq <- array(rep(1, rows * cols * num_samples), dim = c (rows, cols, num_samples))
#pq <- array(rep(1, rows * cols * num_samples), dim = c (rows, cols, num_samples))
#iq <- array(rep(1, rows * cols * num_samples), dim = c (rows, cols, num_samples))

#For our landmark data
oq <- array(rep(1, 11*2*On), dim=c(11, 2, On))
pq <- array(rep(1, 11*2*Pn), dim=c(11, 2, Pn))
iq <- array(rep(1, 11*2*In), dim=c(11, 2, In))



# Function to read and update a sample
update_sample <- function(sample_index, file_path, oq_array) 
update_sample <- function(sample_index, file_path, pq_array)
update_sample <- function(sample_index, file_path, iq_array) 
  
# Read the Excel file into a matrix
Excel_file <- as.matrix(read_excel(file_path, col_names = FALSE))

# Example from our data
B1Ox1 <- as.matrix(read_excel(file_path, col_names = FALSE))  # C.oxystoma sample
P1 <- as.matrix(read_excel(file_path, col_names = FALSE))     # C.peregrinus sample
ADIN1 <- as.matrix(read_excel(file_path, col_names = FALSE))  # C.innoxius sample

# Update the specific slice of oq with the values from B1Ox1
#for (i in 1:rows) 
#{
#    for (j in 1:cols) 
#    {
#     array[i, j, sample_index] <- sampleid[i, j]
#    }
#}

# Example from our data
B1Ox1 <- as.matrix(B1Ox1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    oq[i,j,1]=B1Ox1[i,j]
  }
}

P1 <- as.matrix(P1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    pq[i,j,1]=P1[i,j]
  }
}
ADIN1 <- as.matrix(ADIN1)
for(i in 1:11)
{
  for(j in 1:2)
  {
    iq[i,j,1]=ADIN1[i,j]
  }
}
#dimention calculation
datao <- oq
datap <- pq
datai <- iq

dimo=dim(oq)
dimp=dim(pq)
dimi=dim(iq)
ott=oq;
ptt=pq;
itt=iq;

#Procrustes Superimposition
#oqqq <- procGPA(oq, scaling = TRUE)
#my data = oq,pq,iq

oqqq=procGPA(oq)
oqq=oqqq$rotated
dim(oq)
dim(oqq)
pqqq=procGPA(pq)
pqq=pqqq$rotated
dim(pq)
dim(pqq)
iqqq=procGPA(iq)
iqq=iqqq$rotated
dim(iq)
dim(iqq)


#MCMC posterior sampling for 2D landmark data 
#(Gaussian likelihood with Isotropic Error Variance)
#Draws posterior from 5 parameter "Sigma" from Whole Data with pre-shape spaced landmarks
#"Sigma" = Isotropic error variation parameter
# here we are assuming Isotropy of error variance
#'@param tune Tuning value of MCMC sampler
#'@param myData  3D array containing 2 dimensional landmark
#'@param Nsample Number of MCMC sample desired
#'@param initial The start value of  parameter sigma for MCMC run

#MCMCpostPsample2D=function(initial,tune,myData, Nsample)
n_values <- c(10, 50, 100, 139)
ott50000.10=MCMCpostPsample2D(20,rep(7,1),oqq[,,1:10],50000)    
ott50000.50=MCMCpostPsample2D(20,rep(7,1),oqq[,,1:50],50000)
ott50000.100=MCMCpostPsample2D(20,rep(7,1),oqq[,,1:100],50000)
ott50000.139=MCMCpostPsample2D(20,rep(7,1),oqq[,,1:139],50000)
ott50000=cbind(ott50000.10,ott50000.50, ott50000.100, ott50000.139);
#print(ott50000.139)
n_values <- c(10, 50, 100, 148)
ptt50000.10=MCMCpostPsample2D(20,rep(7,1),pqq[,,50:60],50000)
ptt50000.50=MCMCpostPsample2D(20,rep(7,1),pqq[,,50:100],50000)
ptt50000.100=MCMCpostPsample2D(20,rep(7,1),pqq[,,1:100],50000)
ptt50000.148=MCMCpostPsample2D(20,rep(7,1),pqq[,,1:148],50000)
ptt50000=cbind(ptt50000.10,ptt50000.50, ptt50000.100, ptt50000.148);
#print(ptt50000.148)
n_values <- c(10, 50, 100, 110)
itt50000.10=MCMCpostPsample2D(20,rep(7,1),iqq[,,60:70],50000)
itt50000.50=MCMCpostPsample2D(20,rep(7,1),iqq[,,1:50],50000)
itt50000.100=MCMCpostPsample2D(20,rep(7,1),iqq[,,1:100],50000)
itt50000.110=MCMCpostPsample2D(20,rep(7,1),iqq[,,1:110],50000)
itt50000=cbind(itt50000.10,itt50000.50, itt50000.100, itt50000.110);
#print(itt50000.110)

#For output validation
stopifnot(dim(ott50000)[2] == 4)
stopifnot(dim(ptt50000)[2] == 4)
stopifnot(dim(itt50000)[2] == 4)

#PPLOTpostvar2D
#theta=1.5;
#plot(density(ress_10[1001:10000,2]), xlim=c(0,8),ylim=c(0,5),col="red",ylab="density (with vague prior)", xlab="Sample")
plot(density(ott50000[-(1:10000),1]), xlim=c(0,50),ylim=c(0,1.5),col="pink1", xlab=expression(paste(tilde(sigma)~~(" \n data : C.oxystoma Wings"))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of ")~~tilde(sigma)~~("Procrustes Variance"))))
abline(v=mean(ott50000[-(1:10000),4]), col="red",lwd=2, lty=2)
#lines(density(vvt5000[,2]), ylim=c(0,12),col="blue")
lines(density(ott50000[-(1:10000),2]), ylim=c(0,1.5),col="pink4", lwd=2)
lines(density(ott50000[-(1:10000),3]), ylim=c(0,1.5),col="maroon1", lwd=2)
lines(density(ott50000[-(1:10000),4]), ylim=c(0,1.5),col="red4", lwd=2)
legend("topright",cex=0.9, c("n=10","n=50","n=100","n=139", "mean"), lty = c(1,1,1,1,1), col = c("pink1","pink4","maroon1","red4", "red"), lwd = c(1,1,1,1,2))
grid()

#C.peregrinus

plot(density(ptt50000[-(1:10000),1]), xlim=c(0,50),ylim=c(0,1.5),col="skyblue", xlab=expression(paste(tilde(sigma)~~(" \n data : C.peregrinus Wings"))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of ")~~tilde(sigma)~~("Procrustes Variance"))))
abline(v=mean(ptt50000[-(1:10000),4]), col="blue3",lwd=2, lty=2)
lines(density(ptt50000[-(1:10000),2]), ylim=c(0,1.5),col="purple", lwd=2)
lines(density(ptt50000[-(1:10000),3]), ylim=c(0,1.5),col="blue", lwd=2)
lines(density(ptt50000[-(1:10000),4]), ylim=c(0,1.5),col="darkblue", lwd=2)
legend("topright",cex=0.9, c("n=10","n=50","n=100","n=148", "mean"), lty = c(1,1,1,1,1), col = c("skyblue","purple","blue","darkblue", "blue3"), lwd = c(1,1,1,1,2))
grid()

#C.innoxius
plot(density(itt50000[-(1:10000),1]), xlim=c(0, 50),ylim=c(0,1.5),col="green", xlab=expression(paste(tilde(sigma)~~(" \n data : C.innoxius Wings "))),ylab="MCMC Posterior Sigma Density",main=expression(paste(bold("MCMC  posterior of")~~tilde(sigma)~~("Procrustes Variance"))))
abline(v=mean(itt50000[-(1:10000),4]), col="yellow4",lwd=3, lty=2)
lines(density(itt50000[-(1:10000),2]), ylim=c(0,1.5),col="green4", lwd=2)
lines(density(itt50000[-(1:10000),3]), ylim=c(0,1.5),col="orange", lwd=2)
lines(density(itt50000[-(1:10000),4]), ylim=c(0,1.5),col="maroon4", lwd=2)
legend("topleft",cex=0.9, c("n=10","n=50","n=100","n=110", "mean"), lty = c(1,1,1,1,1), col = c("green","green4","orange","maroon4", "yellow4"), lwd = c(1,1,1,1,2))
grid()

##Simulate the MCMC posterior samples
library(ggplot2)
# Set seed for reproducibility
set.seed(123)
# Subset posterior data (remove burn-in period)
posterior1 <- ott50000.139[-(1:10000)] 
posterior2 <- ptt50000.148[-(1:10000)] 
posterior3 <- itt50000.110[-(1:10000)] 

ggplot() +
  geom_density(aes(x = posterior1, color = "Posterior 1")) +
  geom_density(aes(x = posterior2, color = "Posterior 2")) +
  geom_density(aes(x = posterior3, color = "Posterior 3")) +
  labs(x = "Value", y = "Density", title = "Raw MCMC Posterior Distributions") +
  theme_minimal()

print(posterior1)
print(posterior2)
print(posterior3)

####95% credible interval calculation#############
# Remove burn-in of the first 10,000 samples
ott_posterior <- ott50000[-(1:10000), 4]
ptt_posterior <- ptt50000[-(1:10000), 4]
itt_posterior <- itt50000[-(1:10000), 4]

# 95% credible interval for C. oxystoma
ci_oxystoma <- quantile(ott_posterior, probs = c(0.025, 0.975))
print(ci_oxystoma)

# 95% credible interval for C. peregrinus
ci_peregrinus <- quantile(ptt_posterior, probs = c(0.025, 0.975))
print(ci_peregrinus)

# 95% credible interval for C. innoxius
ci_innoxius <- quantile(itt_posterior, probs = c(0.025, 0.975))
print(ci_innoxius)



# Density estimation with n = 512 points
dens1 <- density(posterior1, n = 512)
dens2 <- density(posterior2, n = 512)
dens3 <- density(posterior3, n = 512)
print(dens1)
print(dens2)
print (dens3)

# Define the common x range (intersection of ranges)
#common_x <- seq(max(min(dens1$x), min(dens2$x), min(dens3$x)), min(max(dens1$x), max(dens2$x), max(dens3$x)), length.out = 512)
# Check the common_x range
cat("common_x range:", range(common_x), "\n")
cat("dens1 range:", range(dens1$x), "\n")
cat("dens2 range:", range(dens2$x), "\n")
cat("dens3 range:", range(dens3$x), "\n")
common_x <- seq(min(c(dens1$x, dens2$x, dens3$x)), max(c(dens1$x, dens2$x, dens3$x)), length.out = 100)
print(common_x)

#Normalize the Densities
dens1$y <- dens1$y / max(dens1$y)
dens2$y <- dens2$y / max(dens2$y)
dens3$y <- dens3$y / max(dens3$y)
# Interpolate densities at common_x points using approx()
common_dens1 <- approx(dens1$x, dens1$y, xout = common_x, rule = 2)$y
common_dens2 <- approx(dens2$x, dens2$y, xout = common_x, rule = 2)$y
common_dens3 <- approx(dens3$x, dens3$y, xout = common_x, rule = 2)$y

plot(common_x, common_dens1, type = "l", col = "blue", main = "Interpolated Densities", xlab = "x", ylab = "Density")
lines(common_x, common_dens2, col = "red")
lines(common_x, common_dens3, col = "green")
legend("topright", legend = c("Dens1", "Dens2", "Dens3"), col = c("blue", "red", "green"), lty = 1)

# Check the ranges and values of densities
cat("Range of dens1$x:", range(dens1$x), "\n")
cat("Range of dens2$x:", range(dens2$x), "\n")
cat("Range of dens3$x:", range(dens3$x), "\n")

cat("Summary of dens1$y:", summary(dens1$y), "\n")
cat("Summary of dens2$y:", summary(dens2$y), "\n")
cat("Summary of dens3$y:", summary(dens3$y), "\n")

# Clip small values after interpolation
numerical_threshold <- 1e-10
common_dens1[common_dens1 < numerical_threshold] <- 0
common_dens2[common_dens2 < numerical_threshold] <- 0
common_dens3[common_dens3 < numerical_threshold] <- 0

plot(common_x, common_dens1, col = "red", main = "Posterior 1 Interpolated")
plot(common_x, common_dens2, col = "green", main = "Posterior 2 Interpolated")
plot(common_x, common_dens3, col = "blue", main = "Posterior 3 Interpolated")


# Check lengths of common densities
cat("Length of common_dens1:", length(common_dens1), "\n")
cat("Length of common_dens2:", length(common_dens2), "\n")
cat("Length of common_dens3:", length(common_dens3), "\n")

# Check for NA values
cat("NA values in common_dens1:", sum(is.na(common_dens1)), "\n")
cat("NA values in common_dens2:", sum(is.na(common_dens2)), "\n")
cat("NA values in common_dens3:", sum(is.na(common_dens3)), "\n")



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

# Verify sums of normalized densities
cat("Sum of normalized smooth_dens1:", sum(smooth_dens1), "\n")
cat("Sum of normalized smooth_dens2:", sum(smooth_dens2), "\n")
cat("Sum of normalized smooth_dens3:", sum(smooth_dens3), "\n")

# Prepare data for plotting with ggplot2
smooth_df <- data.frame(
  x = rep(common_x, 3),
  y = c(smooth_dens1, smooth_dens2, smooth_dens3),
  group = rep(c("Posterior1", "Posterior2", "Posterior3"), each = length(common_x))
)

# Plot densities
ggplot(smooth_df, aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  labs(title = "Smoothed and Normalized Densities", x = "X", y = "Density") +
  theme_minimal()




###########################################################################






