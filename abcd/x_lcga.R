library(lcmm)

set.seed(123)
n <- 200  # number of patients

# Simulate latent classes (3 classes)
class <- sample(1:3, n, replace = TRUE, prob = c(0.4, 0.35, 0.25))

# Simulate trajectory data for each class
time <- rep(0:2, times = n)
id <- rep(1:n, each = 3)

y <- numeric(length(id))

for(i in 1:n){
  cl <- class[i]
  for(t in 0:2){
    idx <- (i - 1) * 3 + t + 1
    if(cl == 1) {
      y[idx] <- 5 + 0.5 * t + rnorm(1, 0, 0.5)        # slowly increasing
    } else if(cl == 2) {
      y[idx] <- 8 - 0.7 * t + rnorm(1, 0, 0.5)        # decreasing
    } else {
      y[idx] <- 3 + 1.2 * t + rnorm(1, 0, 0.5)        # rapidly increasing
    }
  }
}

data <- data.frame(id = id, time = time, y = y)
head(data)

# Fit a LCGA with 3 classes, linear time effect
lcga_model <- hlme(fixed = y ~ time,
                   random = ~ 0,        # No random effects (LCGA)
                   mixture = ~ time,    # class-specific time effect
                   subject = 'id',
                   ng = 3,              # number of latent classes
                   data = data,
                   nwg = TRUE)          # class-specific residual variances

summary(lcga_model)
