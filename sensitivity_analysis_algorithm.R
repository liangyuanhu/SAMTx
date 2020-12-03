##########################################################################################
## sensitivity analysis to unmeasured confounding with multiple treatments by BART
##
## Jungang Zou and Jiayi Ji
## Last updated: 12/02/2020
##
##########################################################################################

sensitivity_analysis = function(covariates, y, A, alpha, n_p = 10, sample_gap = 10, nposterior = 1000, sensitivity_correction = F){
  # change the type of y and A as the input parameter of bart function
  covariates = as.matrix(covariates)
  y = as.numeric(y)
  A = as.integer(A)
  A_unique_length <- length(unique(A))
  alpha = as.matrix(alpha)
  n_alpha = nrow(alpha)
  # if we don`t need to adjust the sensitivity, then a simple bart model will be fitted and return a list of ATE
  if (!sensitivity_correction) {
    # fit binary bart
    bart_mod = pbart(x.train = cbind(covariates, A),  y.train = y,  ndpost = nposterior, printevery = 10000)
    for (j in 1:(A_unique_length)){
      assign(paste0("predict_",j), pnorm(pwbart(cbind(covariates, A = j), bart_mod$treedraws)))
    }
    
    for (i in 1:(A_unique_length-1)){
      for (j in (i + 1):A_unique_length){
        assign(paste0("ATE_",i,j), rowMeans(eval(parse(text =(paste0("predict_",i)))) - eval(parse(text =(paste0("predict_",j))))))
      }
    }
    result <- NULL
    for (i in 1:(A_unique_length-1)){
      for (j in (i + 1):A_unique_length){
        
        assign(paste0("ATE_",i,j), list(eval(parse(text =(paste0("ATE_",i,j))))))
        assign(paste0("ATE_",i,j), stats::setNames(eval(parse(text =(paste0("ATE_",i,j)))), paste0("ATE_",i,j)))
        result <- c(result, (eval(parse(text =(paste0("ATE_",i,j))))))
      }
    }
    return(result)
  }
  
  
  
  #### from here, the function of sensitivity correction will begin.
  
  # fit the treatment assigment model, to use gap-sampling, we over sample n * sample_gap samples, and select a sample per sample_gap turns
  assign_mod = mbart2(x.train = covariates, as.integer(as.factor(A)), x.test = covariates, ndpost = n_p * sample_gap, nskip = 1000)
  
  # assign the estimated assignment probability to each sample, the size is (n, #treatment, sample_size)
  p = array(assign_mod$prob.test[seq(1, nrow(assign_mod$prob.test), sample_gap),], dim = c(n_p, A_unique_length, length(A)))
  
  # start to calculate causal effect by n * n times
  # for (j in 1:(A_unique_length)){
  #   assign(paste0("causal_effect_",j), matrix(NA, nrow = n_alpha, ncol = n_p * nposterior))
  # }
  step = 1
  train_x = cbind(covariates, A)  
  for (m in 1:(A_unique_length-1)){
    for (n in (m + 1):A_unique_length){
      assign(paste0("ATE_",m,n), c())
    }
  }
  for (i in 1:n_alpha) {
    for (m in 1:(A_unique_length-1)){
      for (n in (m + 1):A_unique_length){
        assign(paste0("ate_",m,n), c())
      }
    }
    for (j in 1:n_p) {
      print(paste("step :", step, "/", n_alpha*n_p))
      sort(unique(train_x[, "A"]))
      # correct the binary outcome based on A, alpha, p
      
        train_y = ifelse(train_x[, "A"] == sort(unique(train_x[, "A"]))[1], y - (unlist(alpha[i, 1]) * p[j, 2, ] + unlist(alpha[i, 4]) * p[j, 3, ]), 
                         ifelse(train_x[, "A"] == sort(unique(train_x[, "A"]))[2], y - (unlist(alpha[i, 2]) * p[j, 1, ] + unlist(alpha[i, 3]) * p[j, 3, ]), 
                                y - (unlist(alpha[i, 5]) * p[j, 1, ] + unlist(alpha[i, 6]) * p[j, 2, ]))) # A = 3

      # fit the bart model to estimate causal effect                          
      bart_mod = wbart(x.train = cbind(covariates, A),  y.train = train_y,  ndpost = nposterior, printevery = 10000)
      
      for (z in 1:(A_unique_length)){
        assign(paste0("predict_",z), pnorm(pwbart(cbind(covariates, A = z), bart_mod$treedraws)))
      }
      
      for (m in 1:(A_unique_length-1)){
        for (n in (m + 1):A_unique_length){
          assign(paste0("ate_",m,n), c(eval(parse(text =(paste0("ate_",m,n)))), rowMeans(eval(parse(text =(paste0("predict_",m)))) - eval(parse(text =(paste0("predict_",n)))))))
        }
      }
      
      step = step + 1
    }
    for (m in 1:(A_unique_length-1)){
      for (n in (m + 1):A_unique_length){
        assign(paste0("ATE_",m,n), rbind(eval(parse(text =(paste0("ATE_",m,n)))),
                                         eval(parse(text =(paste0("ate_",m,n))))))
      }
    }
  }
  # return the result
  result <- NULL
  for (i in 1:(A_unique_length-1)){
    for (j in (i + 1):A_unique_length){
      
      assign(paste0("ATE_",i,j), list(eval(parse(text =(paste0("ATE_",i,j))))))
      assign(paste0("ATE_",i,j), stats::setNames(eval(parse(text =(paste0("ATE_",i,j)))), paste0("ATE_",i,j)))
      result <- c(result, (eval(parse(text =(paste0("ATE_",i,j))))))
    }
  }
  return(result)
}