library(glmnet)
source('functions.R')
# source('make_tidy.R')


### TRUE FIT
spec <- logistic_reg(penalty = 0.3) %>% set_engine("glmnet")
spec



######


n <- 1000
dat <- tibble(x = seq(-3,3, length.out = n),
       w = 3*cos(3*seq(-pi,pi, length.out = n)),
       y = rbinom(n,size = 1, prob = 1/(1 + exp(-w+2*x)) )%>% as.numeric %>% factor,
       cat = sample(c("a","b","c"), n, replace = TRUE)
)


a <- as.matrix(dat$x)
b <- as.matrix(dat$y)

lambda <- 0.3
# compute coefficients and intercept using glmnet and store as int_true and
# beta_true.
ret <- fit_logistic_lasso(x=a,y=b,lambda = 0.3)
ret

truefit <- glm.fit(x=a, y=b)

if (abs(int_true - ret$intercept) > 0.01) { ## this may not be the tolerance!
  print(glue::glue("Test failed. Expected intercept {int_true} but got 
                   {ret$intercept}\n"))
}

if (mean(abs(beta_true - ret$beta)) > 0.01) { ## this may not be the tolerance!
      print(glue::glue("Test failed. Expected computed beta had an error of
                       {mean(abs(beta_true - ret$beta))}.\n"))
}
