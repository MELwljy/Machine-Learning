fit_logistic_lasso <- function(x, y, lambda, beta0 = NULL, eps=0.0001,iter_max = 100) {
  ## Return a list containing the members intercept, beta, and lambda
  ## 
  ## Input:
  ## - x: matrix of predictors (not including the intercept)
  ## - y: vector of data
  ## - lambda: penalty
  ## - beta0: initial guess
  ## - eps: parameter for stopping critereon
  ## - iter_max: maximium number of iterations
  ##
  ## Output:
  ## It will return a list including the following information:
  ## - beta: all the beta values including beta0 (intercept)
  ## - intercept: the value of beta0
  ## - lambda: tuning parameter of penalty
  ## - fct_levels: factor level of response variable
  ## - iter: number of iteration
  ## - error: regression error
  ## - converged: if the estimated beta converge,use True; False,otherwise
  ##
  ## Example:
  ##   fit_logistic_lasso(x,y,lambda=1, beta0 = NULL, eps=0.0001,max_iter = 1000)
  
  x <- cbind(1, x) # add intercept
  n <- dim(x)[1]
  p <- dim(x)[2] # num of beta include intercept
  
  if(is.null(beta0)){
    beta0 <- rep(0, p)
  }
  
  beta <- beta0
  
  ## Process the factor to be 0/1
  ## Make sure you save the names of the factor levels so we can
  ## use them in the predictions
  fct_levels <- levels(y)
  y <- as.numeric(y) - 1
  
  x_beta0 <- (x %*% beta0) %>% as.numeric
  p <- 1/(1 + exp(-x_beta0)) # logistic function (nx1)
  
  
  for (i in 1:iter_max){
    w <- p * (1 - p)
    z <- x_beta0 + (y - p)/w
    
    # find beta_i for i =0...p
    for(j in 1:length(beta)) {
      r = z - x[,-j] %*% beta[-j] 
      A<-sum(w*r*x[,j])
      beta[j] = sign(A) * max(abs(A)-lambda, 0) / sum(w*x[,j]^2)
    }
    
    x_beta0 <- (x %*% beta) %>% as.numeric
    p <- 1/(1 + exp(-x_beta0))
    
    names(beta) <- colnames(x)
    
    # condition for stopping
    if (max(abs(beta-beta0))/max(abs(beta)) < eps) { # converged
      return(
        list(beta = beta[-1], intercept=beta[1], lambda=lambda,
             fct_levels = fct_levels, iter = i, converged = TRUE)
      )
    }
    beta0 = beta #keep track of the old beta for convergence check
    
  }
  # If we get here we have exceeded iter_max
  warning(paste("Method did not convergence in",iter_max, "iterations",sep=" "))
  return(
    list(beta = beta[-1], intercept=beta[1], lambda=lambda, 
         fct_levels = fct_levels, iter = i, converged = FALSE)
  )
}

    

predict_logistic_lasso <- function(fit, new_x) {
  ## Return a list containing the intercept and beta
  ##
  ## Input:
  ## - object: Output from fit_logistic_lasso
  ## - new_x: Data to predit at (may be more than one point!)
  ##
  ## Output:
  ## - fct_levels: factor levels of y^hat
  ## - beta: all the beta values including beta0 (intercept)
  ## - lambda: penalty parameter
  ## - iter: number of iteration
  ## - converged: True if the estimated beta converge
  ## - error: regression error
  ##
  ## Example:
  ##   return<-fit_logistic_lasso(x,y,lambda=1, beta0 = NULL, eps=0.0001,max_iter = 1000)
  ##   predict_logistic_lasso(return,new_x)
  ##
  
  new_x <- cbind(1, new_x) # add intercept
  beta <- c(fit$intercept,fit$beta)
  numeric_pred <- ((new_x %*% beta) >= 0) %>% as.numeric
  return(fit$fct_levels[numeric_pred+1] %>%factor)
}

