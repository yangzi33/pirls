#######################################
# Assignment 2 Functions
# STA314H1, Fall 2020
# Author: Ziyue Yang
# Student Number: 1004804759
# Contact: ziyue.yang@mail.utoronto.ca
#######################################

library(tidyverse)
library(tidymodels)

fit_logistic_lasso <- function(x, y, lambda, beta0 = NULL, eps = 0.0001, iter_max = 100) {
  # Inputs:
  #         x: matrix of predictors (not including the intercept)
  #         y: vector of data
  #         beta0: initial guess
  #         eps: parameter for stopping criterion
  #         iter_max: maximum number number of iterations
  # Output:
  #         Returns a list containing the members `intercept`, `beta`, 
  #         and `lambda`.
  #
  ####################### Function Body begins here #######################
  
  m <- dim(x)[1]
  n <- dim(x)[2]
  
  converged <- FALSE
  
  if (is.null(beta0)) {
    beta0 <- rep(0, n)
  }
  
  beta1 <- beta0
  for (i in 1:iter_max) {
    xbeta0 <- x %*% beta0
    p <- exp(1 + exp(xbeta0)) ** -1
    w <- p * (1 - p)
    z <- xbeta0 + (y - p) / w
    
    # Update beta1 using coordinate descent until convergence
    for (ii in 1:iter_max) {
    loss <- quadratic_approx(x, w, z, lambda, beta0)
      beta1_old <- beta1
      for (j in 1:n){
        beta1[j] <- soft_thresh(sum(w * x[,j] * (z - beta1_old[1] - x[,c(-1, -j)] %*% beta1_old[c(-1, -j)])),
                                lambda * sum(w))
      }
      # Calculate and compare the quadratic approximation
      loss_new <- quadratic_approx(x, w, z, lambda, beta1)
      # print(paste("beta0:", beta0, sep = " "))
      # print(paste("beta1:", beta1, sep = " "))
      # print(paste("loss0:", loss, sep = " "))
      # print(paste("loss1:", loss_new, sep = " "))

      # Check coordinate divergence
      if (loss <= loss_new) {
        beta1 <- beta0
        break
      }
    }
    
    if (ii == iter_max && loss_new <= loss) {
      warning("Coordinate descent did not converge.")
    }
    
    if (sqrt(as.vector(Matrix::crossprod(beta1 - beta0))) < eps) {
      converged <- TRUE
      break
    }
    beta0 <- beta1
  }
  
  if (i == iter_max & !converged) {
    warning(paste("Algorithm not converged in", iter_max, "iterations", sep = " "))
  }
  
  return(list(intecept = beta1[1], beta = beta1[-1], lambda = lambda))
}

ret <- fit_logistic_lasso(A, yy, 0.6)
ret

predict_logistic_lasso <- function(object, new_x) {
  # Inputs:
  #         object: Output from fit_logistic_lasso
  #         new_x: Data to predict at (may be more than one point)
  # Output:
  #         Returns a list containing the intercept and beta.
  #
  ####################### Function Body begins here #######################
  
  pred <- as.numeric(as.matrix(new_x) %*% object$beta1 >= 0)
  return(list(
    intercept = object$intercept,
    beta1 = object$beta1,
    lambda = object$lambda
  ))
}

### Helper functions
quadratic_approx <- function(x, w, z, lambda, beta1) {
  # Given input beta, update it using the coordinate descent.
  # Inputs:
  #       x: the matrix of predictors
  #       w, z: terms in the quadratic approximation
  #       lambda: penalty term
  #       beta0: old beta
  # Outputs:
  #       Returns the quadratic approximation using Newton's method (multi-dim)
  #
  ####################### Function Body begins here #######################
  # ret <- 0
  # for (i in 1:dim(x)[1]) {
  #   ret <- ret + w[i] * (z[i] - beta0[1] - (x[i,] %*% beta0)) ** 2
  # }
  # return(ret + lambda * sum(abs(beta0)))
  return(1/dim(x)[1]/2 * sum(w*(z - beta1[1] - x[,-1] %*% beta1[-1]) ** 2) - 
    lambda * sum(abs(beta1))
    )
}

soft_thresh <- function(a, b) {
  a <- as.vector(a)
  a[abs(a) < b] <- 0
  a[a < 0] <- a[a < 0] + b
  a[a > 0] <- a[a > 0] - b
  return(a)
}

coordinate_descent <- function(x, w, z, y, lambda, beta1, iter_max) {
  # Given input beta0, beta. Update them using the coordinate descent.
  # Inputs:
  #       x: the matrix of predictors
  #       w, z: terms in the quadratic approximation
  #       y: vector of data
  #       lambda: penalty term
  #       beta: the beta to update
  # Outputs:
  #       new_beta: Updated (beta0, beta) using coordinate descent
  #
  ####################### Function Body begins here #######################

  for (ii in 1:iter_max) {
     beta_old <- beta1
     for (j in 1:length(beta1)) {
       rj <- y
       for (k in 1:length(beta1)) {
         if (j != ii) {
           rj <- rj - (beta1[k] * x[,k])
         }
       }
       beta1[j] <- sign(x[,j] %*% rj) * max(0, abs(x[,j] %*% rj) - lambda)
     }
     if ( ii == 1) {
      loss_old <- quadratic_approx(x, w, z, lambda, beta1, beta_old)
     }
    else if (ii > 1) {
      loss_new <- quadratic_approx(x, w, z, lambda, beta1, beta_old)
      # print(loss_new)
      # print(loss_old)
      # if (loss_new > loss_old) {
      #   beta1 <- beta_old
      #   break
      # }
      loss_old <- loss_new
    }
  }
  if (ii == iter_max && loss_new <= loss_old) {
    warning("Coordinate descent did not coverge.")
  }
  
  return(beta1)
}












data("airquality")
airquality <- airquality %>% remove_missing() %>% transmute(log_ozone = log(Ozone),
                                                            solar = Solar.R,
                                                            wind = Wind,
                                                            temp = Temp,
                                                            month = factor(Month), day = factor(Day))
rec <- recipe(log_ozone ~ . , data = airquality) %>%
  step_normalize(all_numeric()) %>% step_dummy(all_nominal())

dat <- rec %>% prep %>% juice # make the recipe!
x <- dat %>% select( -log_ozone) %>% as.matrix
y <- dat$log_ozone

###
len = 20
A <- matrix(0, len, 4)
A[cbind(1:m, sample(ncol(A), m, TRUE))] <- rnorm(4)
ww <- as.matrix(3*cos(3*seq(-pi,pi, length.out = len)))
yy <- rbinom(len, size = 1, prob = 1 / (1 + exp(-ww + 2 * A[,1]))) %>% as.numeric
A <- as.matrix(A)
pen <- 0.3
bt <- rep(0, 4)


