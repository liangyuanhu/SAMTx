##########################################################################################
## JSON parser for causal inference simulation
##
## Jungang Zou
## Last updated: 08/01/2020
##
##########################################################################################
library("rjson")
library("tidyverse")
library("R.oo")
library("cowplot")
library("extraDistr")

# the allowed distributions in JSON
allowed_distribution = c("normal", "uniform", "binomial", "bernoulli", "poisson", "multinomial", "uniform integer", "sequence", "t", "gamma", "cauchy", "inverse gamma", "half cauchy", "half normal", "beta", "truncnorm")


####################### the main function ########################
json_parser = function(data_path, sensitivity_path = NULL, save_result = F, overlap_plot = T){
  
  
  input <- fromJSON(file = data_path) # read the json
  
  #### data simulation ####
  if ("random_seed" %in% names(input))
    set.seed(input[["random_seed"]])
  X = covariates_parser(input[["covariates"]])
  sample_size = input[["covariates"]][["sample_size"]]
  context = cbind(X, sample_size) # context environment for formula parser
  assignment = assignment_parser(input[["assignment"]], context, overlap_plot)
  table(assignment)
  context = cbind(context, assignment)
  y = outcome_parser(input[["outcome"]], context)
  context = cbind(context, y)
  obs_y = apply(cbind(y, assignment), 1, function(row) row[row[length(row)]])
  context = cbind(context, obs_y)
  print(table(assignment))
  
  # save the assignment proportions and true ATE into the original file
  if (save_result) {
    input[["results"]] = list()
    input[["results"]][["assignment_proportion"]] = table(assignment)
    input[["results"]][["true_ATE"]] = list(ATE_01 = mean(y[, 1] - y[, 2]), ATE_02 = mean(y[, 1] - y[, 3]), ATE_12 = mean(y[, 2] - y[, 3]))
    write(rjson::toJSON(input, indent = 4), file = data_path)
  }
  
  
  #### sensitivity simulation ####
  
  # if sensitivity path is specified, parse the file
  if (!is.null(sensitivity_path)) {
    
    # read file
    sensitivity_input = fromJSON(file = sensitivity_path)
    input["sensitivity_analysis"] = list(sensitivity_input["sensitivity_analysis"]$sensitivity_analysis)
    
    
    if ("random_seed" %in% names(sensitivity_input))
      set.seed(input[["random_seed"]])
    
    # simulation
    sensitivity_analysis = sensitivity_parser(sensitivity_input[["sensitivity_analysis"]], context, X)
    return(list(covariates = X, assignment = assignment, y = y, obs_y = obs_y, input_parameters = input, measured_covariates = sensitivity_analysis$measured_covariates, alpha_repeat = sensitivity_analysis$alpha_repeat, alpha = sensitivity_analysis$alpha_simulation, true_ATE = list(ATE_01 = mean(y[, 1] - y[, 2]), ATE_12 = mean(y[, 2] - y[, 3]), ATE_02 = mean(y[, 1] - y[, 3])), assigment_proportion = table(assignment)))
  }
  
  # if sensitivity parameters are in the data simulation configuration file, then parse the parameters
  else if (!is.null(input[["sensitivity_analysis"]])) {
    # simulation
    sensitivity_analysis = sensitivity_parser(input[["sensitivity_analysis"]], context, X)
    return(list(covariates = X, assignment = assignment, y = y, obs_y = obs_y, input_parameters = input, measured_covariates = sensitivity_analysis$measured_covariates, alpha_repeat = sensitivity_analysis$alpha_repeat, alpha = sensitivity_analysis$alpha_simulation, true_ATE = list(ATE_01 = mean(y[, 1] - y[, 2]), ATE_12 = mean(y[, 2] - y[, 3]), ATE_02 = mean(y[, 1] - y[, 3])), assigment_proportion = table(assignment)))
  }
  return(list(covariates = X, assignment = assignment, y = y, obs_y = obs_y, input_parameters = input, true_ATE = list(ATE_01 = mean(y[, 1] - y[, 2]), ATE_12 = mean(y[, 2] - y[, 3]) , ATE_02 = mean(y[, 1] - y[, 3])), assigment_proportion = table(assignment)))
}






############ parse covariates ##############
covariates_parser = function(covariates){
  
  # generate samples by the sub-function
  sample_generate = function(sample_size, variable_parameters){
    
    distribution = variable_parameters[["distribution"]]
    distribution_parameter = as.list(variable_parameters[["parameters"]])
    
    # if the distribution is not allowed, then throw this error
    if (!distribution %in% allowed_distribution)
      throw("Please use allowed distribution(", paste(allowed_distribution, collapse = ", "), ") instead of ", distribution)
    
    
    # sample by different distributions
    switch(distribution,
           "normal" = rnorm(sample_size, mean = distribution_parameter[[1]], sd = distribution_parameter[[2]]),
           "uniform" = runif(sample_size, distribution_parameter[[1]], distribution_parameter[[2]]),
           "binomial" = rbinom(sample_size, size = distribution_parameter[[1]], prob = distribution_parameter[[2]]),
           "bernoulli" = rbernoulli(sample_size, p = distribution_parameter[[1]]),
           "poisson" = rpois(sample_size, lambda = distribution_parameter[[1]]),
           "multinomial" = sample(seq(length(distribution_parameter)), size = sample_size, replace = T, prob = unlist(distribution_parameter)),
           "uniform integer" = rdunif(sample_size, distribution_parameter[[1]], distribution_parameter[[2]]),
           "t" = rt(sample_size,  df = distribution_parameter[[1]], ncp = distribution_parameter[[2]]),
           "gamma" = rgamma(sample_size,  shape = distribution_parameter[[1]], rate = distribution_parameter[[2]]),
           "cauchy" = rcauchy(sample_size,  location = distribution_parameter[[1]], scale = distribution_parameter[[2]]),
           "inverse gamma" = extraDistr::rinvgamma(sample_size, alpha = distribution_parameter[[1]], beta = distribution_parameter[[2]]),
           "half cauchy" = rhcauchy(sample_size, sigma = distribution_parameter[[1]]),
           "half normal" = rhnorm(sample_size, sigma = distribution_parameter[[1]]),
           "beta" = rbeta(sample_size, shape1 = distribution_parameter[[1]], shape2 = distribution_parameter[[2]], ncp = distribution_parameter[[3]]),
           "truncnorm" = rtruncnorm(sample_size, a = distribution_parameter[[1]], b = distribution_parameter[[2]], mean = distribution_parameter[[3]], sd = distribution_parameter[[4]]),
           rnorm(sample_size)
    )
  }
  
  # start sample covariates
  sample_size = covariates[["sample_size"]]
  variable_list = names(covariates)[names(covariates) != "sample_size"]
  names(variable_list) <- variable_list
  map_df(variable_list, ~sample_generate(sample_size, covariates[[.x]]))
}




############ parse assignment ##############

assignment_parser = function(assignment, context, overlap_plot){
  
  # this sub-function serve as the multinomial regression
  softmax <- function(x){
    x = x - apply(x, 1, function(x) max(x))
    score.exp <- exp(x) 
    probs <- sweep(score.exp, 1, rowSums(score.exp), '/')
    return(probs)
  }
  
  # read the parameters
  if ("treatment" %in% names(assignment)) {
    if ("overlap_beta" %in% names(assignment)){
      overlap_beta = assignment[["overlap_beta"]]
      covariates = cbind(covariates, overlap_beta)
    }
    assignment = assignment[["treatment"]]
  }
  treatment = names(assignment)
  names(treatment) = treatment
  
  # calculate the multinomial regression 
  prob = softmax(map_df(treatment, ~eval(parse(text = assignment[[.x]]), envir = context))) # parse the formula
  
  prob_ = prob -  min(prob) # to avoid the stack-overflow error by exponential function
  
  # simulate assignment
  assignment =  apply(apply(prob_, 1, function(x) rmultinom(1, 1, x)), 2, function(d) which(d == 1))
  
  # plot the overlap box-plot
  if (overlap_plot)
    print(covariate_overlap(assignment, prob))

  return(assignment)
}


# plot the overlap box-plot
covariate_overlap = function(assignment, prob){
  assignment = as.factor(assignment)
  df = tibble(cbind(assignment, prob))
  plot_list = list()
  for (i in 1:length(unique(assignment))) {
    # gather boxplot for each assignment
    plot_list[[paste("P(A = ", i, ")", sep = "")]] = local({
        i <- i
        ggplot(aes(y = prob[, i], x = assignment, group = assignment), data = df) + 
        geom_boxplot() + 
        ggtitle(paste("P(A = ", i, "| X)", sep = "")) +
        xlab("Assignment") + ylab("Probability")
        }
      ) +
      ylim(0, 1) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # set title
  title <- ggdraw() + 
    draw_label(
      "Covariate Overlap",
      fontface = 'bold',
      x = 0.42,
      hjust = 0
    ) 
  
  # if the number of assignment equal or less than 3, then plot them in a row together
  if (length(unique(assignment)) <= 3){
    plot_grid(title, plot_grid(plotlist = plot_list, nrow = 1), ncol = 1, rel_heights = c(0.1, 1))
  }
  else
    plot_grid(title, plot_grid(plotlist = plot_list), ncol = 1, rel_heights = c(0.1, 1))
}




############ parse outcome ##############

outcome_parser = function(outcome, context){
  type = outcome[["type"]]
  y = outcome[["y"]]
  y = map_df(y, ~eval(parse(text = .x), envir = context)) # parse the formula
  switch(type,
      "binary" = rbernoulli(length(which(!is.na(y))), sigmoid::sigmoid(y)),
      y + rnorm(length(which(!is.na(y))), 0, 1)
  )
}


############ parse sensitivity ##############

sensitivity_parser = function(sensitivity_analysis, context, X){
  
  # generate alpha samples by the sub-function
  alpha_generate = function(alpha_repeat, variable_parameters, context){
    distribution = variable_parameters[["distribution"]]
    distribution_parameter = as.list(variable_parameters[["parameters"]])
    grid = variable_parameters[["grid"]]
    
    # if distribution is not allowed, throw the error
    if (!distribution %in% allowed_distribution)
      throw("Please use allowed distribution(", paste(allowed_distribution, collapse = ", "), ") instead of ", distribution)
    
    
    # if the grid is specified, sample the data for "grid" number of times
    if (!is.null(grid)){
      sample = switch(distribution,
                      "normal" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rnorm(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "uniform" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) runif(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "binomial" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbinom(length(x), size = x, prob = eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "bernoulli" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbernoulli(length(x), p = x)),
                      "poisson" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rpois(length(x), lambda = x)),
                      "multinomial" = t(apply(matrix(unlist(lapply(map(.x = distribution_parameter, ~eval(parse(text = .x), envir = context)), function(x) if (length(x) == 1) rep(x, nrow(context)) else x)), nrow = nrow(context)), 1, function(x) sample(seq(length(distribution_parameter)), size = grid, replace = T, prob = x))),
                      "uniform integer" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rdunif(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "sequence" = seq(eval(parse(text = distribution_parameter[[1]]), envir = context), eval(parse(text = distribution_parameter[[2]]), envir = context), length.out = grid),
                      "t" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rt(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "gamma" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rgamma(length(x), shape = x, rate = eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "cauchy" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rcauchy(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "inverse gamma" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) extraDistr::rinvgamma(length(x), alpha = x, beta = eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      "half cauchy" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rhcauchy(length(x), sigma = x)),
                      "half normal" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rhnorm(length(x), sigma = x)),
                      "beta" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbeta(length(x), shape1 = x, shape2 = eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      # "rtruncnorm" = apply(t(replicate(grid, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbeta(length(x), shape1 = x, shape2 = eval(parse(text = distribution_parameter[[2]]), envir = context))),
                      rnorm(alpha_repeat)
      )
      return(sample)
    }
    
    # if the grid is not specified, sample the data for "alpha_repeat" number of times
    else
      sample = switch(distribution,
             "normal" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rnorm(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "uniform" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) runif(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "binomial" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbinom(length(x), size = x, prob = eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "bernoulli" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbernoulli(length(x), p = x)),
             "poisson" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rpois(length(x), lambda = x)),
             "multinomial" = t(apply(matrix(unlist(lapply(map(.x = distribution_parameter, ~eval(parse(text = .x), envir = context)), function(x) if (length(x) == 1) rep(x, nrow(context)) else x)), nrow = nrow(context)), 1, function(x) sample(seq(length(distribution_parameter)), size = alpha_repeat, replace = T, prob = x))),
             "uniform integer" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rdunif(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "sequence" = seq(eval(parse(text = distribution_parameter[[1]]), envir = context), eval(parse(text = distribution_parameter[[2]]), envir = context), length.out = alpha_repeat),
             "t" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rt(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "gamma" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rgamma(length(x), shape = x, rate = eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "cauchy" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rcauchy(length(x), x, eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "inverse gamma" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) extraDistr::rinvgamma(length(x), alpha = x, beta = eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "half cauchy" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rhcauchy(length(x), sigma = x)),
             "half normal" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rhnorm(length(x), sigma = x)),
             "beta" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rbeta(length(x), shape1 = x, shape2 = eval(parse(text = distribution_parameter[[2]]), envir = context))),
             "truncnorm" = apply(t(replicate(alpha_repeat, eval(parse(text = distribution_parameter[[1]]), envir = context))), 1, function(x) rtruncnorm(length(x), a = x, b = eval(parse(text = distribution_parameter[[2]]), envir = context))),
             rnorm(alpha_repeat)
      )
    if (distribution == "sequence")
      return(sample)
    
    
    # if the sample is conditional on other variables, we need to keep the sampled alphas same if the conditioned variables are same
    if (alpha_repeat == 1 && length(sample) != alpha_repeat) {
      grouped_parameter = as_tibble(matrix(unlist(lapply(map(.x = distribution_parameter, ~eval(parse(text = .x), envir = context)), function(x) if (length(x) == 1) rep(x, nrow(context)) else x)), nrow = nrow(context)))
      sample = cbind(grouped_parameter, sample) %>% group_by_at(vars(starts_with("V"))) %>% mutate_at(vars(!starts_with("V")), first) %>% ungroup() %>%  tidyverse::select(!starts_with("V"))
      sample = as.list(sample)
    }
    if (alpha_repeat > 1 && nrow(sample) != alpha_repeat) {
      grouped_parameter = as_tibble(matrix(unlist(lapply(map(.x = distribution_parameter, ~eval(parse(text = .x), envir = context)), function(x) if (length(x) == 1) rep(x, nrow(context)) else x)), nrow = nrow(context)))
      sample = cbind(grouped_parameter, sample) %>% group_by_at(vars(starts_with("V"))) %>% mutate_at(vars(!starts_with("V")), first) %>% ungroup() %>%  dplyr::select(!starts_with("V"))
      sample = as.matrix(sample)
      sample = lapply(split(sample, rep(1:alpha_repeat, each = alpha_repeat)), as.numeric)
    }
    return(sample)
  }
  
  # read the parameters
  if (sensitivity_analysis[["unmeasured_confounding"]] == ""){
    measured_covariates = dplyr::select(X)
  } else {
    measured_covariates = dplyr::select(X, -sensitivity_analysis[["unmeasured_confounding"]])
  }
  
  alpha_repeat = sensitivity_analysis[["alpha_repeat"]]
  alpha = sensitivity_analysis[["alpha"]]
  alpha_name = names(alpha)
  names(alpha_name) = names(alpha)
  
  
  # separate the alphas into 2 parts: grided or non-grided
  no_grid_alpha_names = alpha_name[sapply(alpha_name, function(x) !("grid" %in% names(alpha[[x]])))]
  grid_alpha_names = alpha_name[sapply(alpha_name, function(x) ("grid" %in% names(alpha[[x]])))]
  
  # simulate the non-grided alphas
  no_grid_alpha_simulation = map(no_grid_alpha_names, ~alpha_generate(alpha_repeat = alpha_repeat, alpha[[.x]], context))
  no_grid_alpha_simulation = as_tibble(no_grid_alpha_simulation)
  
  
  if (length(grid_alpha_names) != 0){
    # sample grided alphas
    grid_alpha_simulation = map(grid_alpha_names, ~alpha_generate(alpha_repeat = alpha_repeat, alpha[[.x]], context))
    grid_alpha_simulation = expand.grid(grid_alpha_simulation) # cross join
    
    # combine all alphas
    alpha_simulation = expand_grid(grid_alpha_simulation, no_grid_alpha_simulation)[, alpha_name]
  }
  else {
    alpha_simulation = no_grid_alpha_simulation
  }
  return(list(measured_covariates = measured_covariates, alpha_repeat = alpha_repeat, alpha_simulation = alpha_simulation))
}



