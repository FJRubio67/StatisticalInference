##################################################################################################
# Robust outlier detection
# Location estimated with the median
# Scale estimated with the normalised mean absolute deviation
##################################################################################################
rm(list=ls())

# Normalised Median Absolute Deviation
b <- 1/qnorm(0.75) # Normalisation constant
NMAD <- function(data)  b*median( abs(data - median(data)))

# Data
data <- c(3, 5, 5, 7, 9, 11, 11, 1000)

# Number of NMADS from the Median
a <- 3

# NMAD and Median
NMAD.data <- NMAD(data)
med <- median(data)

c(med,NMAD.data)

# Robust outlier detection
Rob.Out.detect <- Vectorize(function(x){
  val <- abs((x - med)/NMAD.data)
  return(ifelse(val>a, TRUE, FALSE))
}  )

# Identifying outliers in the data with the robust method
Rob.Out.detect(data)

##################################################################################################
# Non-Robust outlier detection
# Location estimated with the mean
# Scale estimated with the standard deviation
##################################################################################################


# Sample standard deviation
s.sigma <- function(data) sqrt( mean((data - mean(data))^2) )

# Number of SDs from the Mean
a <- 3

# Mean and standard deviation
mean.data <- mean(data)
sd.data <- s.sigma (data)

c(mean.data,sd.data)

# Non-Robust outlier detection
Out.detect <- Vectorize(function(x){
  val <- abs((x - mean.data)/sd.data)
  return(ifelse(val>a, TRUE, FALSE))
}  )

# Identifying outliers in the data with the non-robust method
Out.detect(data)
