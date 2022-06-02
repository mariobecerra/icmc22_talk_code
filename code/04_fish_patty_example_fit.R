library(opdesmixr)
library(tidyverse)
library(mlogit)
library(here)

# detach("package:opdesmixr", unload=TRUE)






# Read fish patty data from CSV from Peter's paper.
# The data on the paper is wrong, 
# the response of the sixth observation should be 1.16 instead of 1.61.
# In this CSV it has already been corrected.
data_fishp_orig = read_csv(here("data/fish_patty_experiment.csv"))

# # Data also available in mixexp package:
# data("fishp", package = "mixexp")
# head(fishp)

to_pv_binary = function(x){
  temp = as.integer(as.factor(x))
  out = ifelse(temp == 2, 1, -1)
  return(out)
}


data_2 = data_fishp_orig %>% 
  # set_names(c("pv_1", "pv_2", "pv_3", "comp_1", "comp_2", "comp_3", "y")) %>% 
  mutate(
    z1 = to_pv_binary(z1),
    z2 = to_pv_binary(z2),
    z3 = to_pv_binary(z3)
  )


cornell_formula = y ~ -1 +
  x1 +
  x2 +
  x3 +
  x1:x2 +
  x1:x3 +
  x2:x3 +
  x1:x2:x3 +
  x1:z1 +
  x2:z1 +
  x3:z1 +
  x1:x2:z1 +
  x1:x3:z1 +
  x2:x3:z1 +
  x1:x2:x3:z1 +
  x1:z2 +
  x2:z2 +
  x3:z2 +
  x1:x2:z2 +
  x1:x3:z2 +
  x2:x3:z2 +
  x1:x2:x3:z2 +
  x1:z3 +
  x2:z3 +
  x3:z3 +
  x1:x2:z3 +
  x1:x3:z3 +
  x2:x3:z3 +
  x1:x2:x3:z3 +
  x1:z1:z2 +
  x2:z1:z2 +
  x3:z1:z2 +
  x1:x2:z1:z2 +
  x1:x3:z1:z2 +
  x2:x3:z1:z2 +
  x1:x2:x3:z1:z2 +
  x1:z1:z3 +
  x2:z1:z3 +
  x3:z1:z3 +
  x1:x2:z1:z3 +
  x1:x3:z1:z3 +
  x2:x3:z1:z3 +
  x1:x2:x3:z1:z3 +
  x1:z2:z3 +
  x2:z2:z3 +
  x3:z2:z3 +
  x1:x2:z2:z3 +
  x1:x3:z2:z3 +
  x2:x3:z2:z3 +
  x1:x2:x3:z2:z3 +
  x1:z1:z2:z3 +
  x2:z1:z2:z3 +
  x3:z1:z2:z3 +
  x1:x2:z1:z2:z3 +
  x1:x3:z1:z2:z3 +
  x2:x3:z1:z2:z3 +
  x1:x2:x3:z1:z2:z3


mod_01 = lm(
  cornell_formula,
  data = data_2)

summary(mod_01)








beta_vec_0 = c(
  2.87, 1.08, 2.01, -1.14, -1.00, 0.20, 3.18, 
  0.49, 0.18, 0.25, -0.81, -0.54, -0.14, 0.07, 
  0.71, 0.25, 0.40, -0.59, -0.01, 0.07, -1.41, 
  -0.09, -0.08, 0.01, 0.10, -0.03, -0.19, 0.11, 
  0.07, -0.03, 0.00, -0.06, -0.06, 0.23, 1.74, 
  -0.05, -0.05, 0.17, 0.14, -0.27, -0.25, -0.71, 
  0.10, -0.03, -0.05, -0.19, -0.43, 0.12, 1.77, 
  0.04, -0.04, -0.04, -0.09, -0.12, 0.27, -1.33)

names_beta_vec_0 = c("x1", "x2", "x3", "x1:x2", "x1:x3", "x2:x3", "x1:x2:x3", "x1:z1", "x2:z1", "x3:z1", "x1:x2:z1", "x1:x3:z1", "x2:x3:z1", "x1:x2:x3:z1", "x1:z2", "x2:z2", "x3:z2", "x1:x2:z2", "x1:x3:z2", "x2:x3:z2", "x1:x2:x3:z2", "x1:z3", "x2:z3", "x3:z3", "x1:x2:z3", "x1:x3:z3", "x2:x3:z3", "x1:x2:x3:z3", "x1:z1:z2", "x2:z1:z2", "x3:z1:z2", "x1:x2:z1:z2", "x1:x3:z1:z2", "x2:x3:z1:z2", "x1:x2:x3:z1:z2", "x1:z1:z3", "x2:z1:z3", "x3:z1:z3", "x1:x2:z1:z3", "x1:x3:z1:z3", "x2:x3:z1:z3", "x1:x2:x3:z1:z3", "x1:z2:z3", "x2:z2:z3", "x3:z2:z3", "x1:x2:z2:z3", "x1:x3:z2:z3", "x2:x3:z2:z3", "x1:x2:x3:z2:z3", "x1:z1:z2:z3", "x2:z1:z2:z3", "x3:z1:z2:z3", "x1:x2:z1:z2:z3", "x1:x3:z1:z2:z3", "x2:x3:z1:z2:z3", "x1:x2:x3:z1:z2:z3")

names(beta_vec_0) = names_beta_vec_0





parameter_estimates = as.data.frame(beta_vec_0) %>% 
  rownames_to_column() %>% 
  full_join(
    as.data.frame(coefficients(mod_01)) %>% 
      rownames_to_column()
  ) %>% 
  set_names(c("variable", "cornell", "me"))


parameter_estimates %>% 
  mutate(dif = cornell - me) %>% 
  filter(abs(dif) > 0.02)




mod_mat_cornell = model.matrix(cornell_formula, data = data_2)

mod_02 = lm.fit(x = mod_mat_cornell, y = data_2$y)

round(mod_02$coefficients, 2)


mod_mat_cornell_2 = mod_mat_cornell %>% 
  as.data.frame() %>% 
  select(parameter_estimates$variable) %>% 
  as.matrix()


fitted_values = tibble(
  cornell_fitted = as.numeric(mod_mat_cornell_2 %*% parameter_estimates$cornell),
  me_fitted = as.numeric(mod_mat_cornell_2 %*% parameter_estimates$me),
  y = data_2$y
) %>% 
  mutate(
    residuals_cornell = cornell_fitted - y,
    residuals_me = me_fitted - y
  )


















cornell_formula_02 = y ~ -1 +
  x1 + x2 + x3 +
  x1:x2 + x1:x3 + x2:x3 +
  x1:z1 + x2:z1 + x3:z1 +
  x1:z2 + x2:z2 + x3:z2 +
  x1:z3 + x2:z3 + x3:z3 +
  z1:z2 + z1:z3 + z2:z3 +
  I(z1^2) + I(z2^2) + I(z3^2)




mod_03 = lm(
  cornell_formula_02,
  data = data_2)

summary(mod_03)






cornell_formula_03 = y ~ -1 +
  x1 + x2 + x3 +
  x1:x2 + x1:x3 + x2:x3 +
  x1:z1 + x2:z1 + x3:z1 +
  x1:z2 + x2:z2 + x3:z2 +
  x1:z3 + x2:z3 + x3:z3 +
  z1:z2 + z1:z3 + z2:z3




mod_04 = lm(
  cornell_formula_03,
  data = data_2)

summary(mod_04)



sum(fitted_values$residuals_cornell^2)
sum(fitted_values$residuals_me^2)
sum((predict(mod_03, data = data_2) - data_2$y)^2)
sum((predict(mod_04, data = data_2) - data_2$y)^2)














beta_vec_2 = as.numeric(mod_04$coefficients)

beta_vec_2_prime = c(beta_vec_2[1] - beta_vec_2[3], 
  beta_vec_2[2] - beta_vec_2[3],
  beta_vec_2[4:length(beta_vec_2)]
  )


# get_utility = function(beta, x_vec){
#   return(sum(beta * x_vec))
# }









get_utility = function(beta, x_vec){
  # Example:
  # get_utility(beta_vec_2_prime, c(1, 0, 0, -1, -1, -1))
  
  x = x_vec[1:3]
  z = x_vec[4:6]
  
  x_mod = c(
  x[1],
  x[2],
  x[1]*x[2], 
  x[1]*x[3], 
  x[2]*x[3], 
  x[1]*z[1], 
  x[2]*z[1], 
  x[3]*z[1], 
  x[1]*z[2], 
  x[2]*z[2], 
  x[3]*z[2], 
  x[1]*z[3], 
  x[2]*z[3], 
  x[3]*z[3], 
  z[1]*z[2], 
  z[1]*z[3], 
  z[2]*z[3]
  )
  
  return(sum(beta * x_mod))
}


get_utility_cornell_beta_2_prime = function(x_vec){
  return(get_utility(beta_vec_2_prime, x_vec))
}


get_utility_cornell_beta_2_prime(c(1, 0, 0, -1, -1, -1))



set.seed(2022)
points_utility = matrix(c(1, 0, 0, -1, -1, -1,
                          1, 0, 0, -1, -1, 1,
                          1, 0, 0, -1, 1, -1,
                          1, 0, 0, -1, 1, 1,
                          1, 0, 0, 1, -1, -1,
                          1, 0, 0, 1, -1, 1,
                          1, 0, 0, 1, 1, -1,
                          1, 0, 0, 1, 1, 1,
                          0, 1, 0, -1, -1, -1,
                          0, 1, 0, -1, -1, 1,
                          0, 1, 0, -1, 1, -1,
                          0, 1, 0, -1, 1, 1,
                          0, 1, 0, 1, -1, -1,
                          0, 1, 0, 1, -1, 1,
                          0, 1, 0, 1, 1, -1,
                          0, 1, 0, 1, 1, 1,
                          0, 0, 1, -1, -1, -1,
                          0, 0, 1, -1, -1, 1,
                          0, 0, 1, -1, 1, -1,
                          0, 0, 1, -1, 1, 1,
                          0, 0, 1, 1, -1, -1,
                          0, 0, 1, 1, -1, 1,
                          0, 0, 1, 1, 1, -1,
                          0, 0, 1, 1, 1, 1), ncol = 6, byrow = T) %>% 
  as.data.frame() %>% 
  bind_rows(
    as.data.frame(opdesmixr::gaussian_create_random_initial_design(100000, 3, 3))
  ) %>% 
  mutate(utility = NA_real_)


for(i in 1:nrow(points_utility)){
  if(i %% 500 == 1) print(i)
  points_utility$utility[i] = get_utility_cornell_beta_2_prime(as.numeric(points_utility[i, 1:6]))
}

points_utility %>% 
  filter(utility == min(utility) |
         utility == max(utility))




















simulate_mixture_choice_data = function(
  design_array, beta_vector, order, n_pv = 0, append_model_matrix = T, seed = NULL){
  
  dim_des_array = dim(design_array)
  
  q = dim_des_array[1] - n_pv
  J = dim_des_array[2]
  S = dim_des_array[3]
  
  if(!is.null(seed)) set.seed(seed)
  
  # Only works for pairwise comparisons:
  model_tibble = map_df(1:S, function(s){
    Xs = opdesmixr::mnl_get_Xs(design_array, s = s, order = order, n_pv = n_pv)
    
    utilities = as.numeric(Xs %*% beta_vector)
    
    exp_utilities = exp(utilities)
    probs = exp_utilities/sum(exp_utilities)
    
    # stopifnot(sum(probs) - 1)
    if(abs(sum(probs) - 1) > 1e-8) stop("sum of probs numerically different to 1")
    
    # # Simulate Bernoulli trial with probability probs[1]
    # choose_first = rbernoulli(n = 1, p = probs[1])
    # # Vector of choices
    # choices = ifelse(choose_first, c(1, 0), c(0, 1))
    # Same as the following (but rnultinom generalizes to more classes):
    choice = as.numeric(rmultinom(n = 1, size = 1, prob = probs))
    
    if(n_pv == 0){
      out = as.data.frame(t(design_array[,,s])) %>%
        set_names(paste0("comp_", 1:q)) %>%
        as_tibble() %>%
        mutate(
          choice_set = s,
          utilities = utilities,
          probs = probs,
          choice = choice)
    } else{
      out = as.data.frame(t(design_array[,,s])) %>%
        set_names(c(paste0("comp_", 1:q), paste0("pv_", 1:n_pv))) %>%
        as_tibble() %>%
        mutate(
          choice_set = s,
          utilities = utilities,
          probs = probs,
          choice = choice)
    }
    
    
    if(append_model_matrix){
      out = out %>%
        bind_cols(
          as.data.frame(Xs) %>%
            set_names(paste0("model_mat_col", 1:ncol(.)))
        )
    }
    
    return(out)
  })
  
  return(model_tibble)
  
}




######################################################################
######################################################################
## Peter
######################################################################
######################################################################


q = 3
n_pv = 3
J = 2
S = 112


beta_vec_3_prime = c(beta_vec_2_prime, 0, 0, 0)


# 60 seconds in 4 cores
design_array_i_opt_1 = opdesmixr::mnl_mixture_coord_exch(
  q = q, J = J, S = S, n_pv = 3,
  beta = beta_vec_3_prime, seed = 2021, order = 2, opt_crit = "I",
  transform_beta = F,
  n_random_starts = 4, n_cores = 4, max_it = 4)




simulated_data_i_opt_1_1 = simulate_mixture_choice_data(
  design_array_i_opt_1$X, n_pv = n_pv, beta = beta_vec_3_prime, order = 2, append_model_matrix = F)


sim_data_mlogit_i_opt_1_1 <- dfidx(
  simulated_data_i_opt_1_1 %>%
    select(comp_1, comp_2, comp_3, pv_1, pv_2, pv_3, choice_set, choice),
  idx = c("choice_set"))


cornell_formula_05 = choice ~ -1 + 
  comp_1 + comp_2 + 
  comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 + 
  comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1 + comp_1:pv_2 + 
  comp_2:pv_2 + comp_3:pv_2 + comp_1:pv_3 + comp_2:pv_3 + 
  comp_3:pv_3 + pv_1:pv_2 + 
  pv_1:pv_3 + pv_2:pv_3 + 
  I(pv_1^2) + I(pv_2^2) + I(pv_3^2)


mod_1 = mlogit(cornell_formula_05, 
               data = sim_data_mlogit_i_opt_1_1)

summary(mod_1)

MASS::ginv(mnl_get_information_matrix(X = design_array_i_opt_1$X, beta = beta_vec_3_prime, 
                                      order = 2, transform_beta = F, n_pv = 1))








# 2.5 minutes
(t1 = Sys.time())
n_params = length(beta_vec_3_prime)
coefs_test_1 = as.data.frame(matrix(rep(NA_real_, 300*n_params), ncol = n_params))

for(i in 1:nrow(coefs_test_1)){
  if(i %% 10 == 1) print(i)
  
  
  sim_data_i = simulate_mixture_choice_data(
    design_array_i_opt_1$X, n_pv = 3, beta = beta_vec_3_prime, 
    order = 2, append_model_matrix = F) %>% 
    select(comp_1, comp_2, comp_3, pv_1, pv_2, pv_3, choice_set, choice)
  
  
  sim_data_mlogit_i <- dfidx(sim_data_i, idx = c("choice_set"))
  
  model_mlogit_i = mlogit(cornell_formula_05,
                          data = sim_data_mlogit_i)
  
  
  coefs_mlogit <- as.numeric(model_mlogit_i$coefficients)
  
  coefs_test_1[i,] = coefs_mlogit
  
}
(t2 = Sys.time())
t2 - t1



# Boxplots of simulations with mlogit
# Real values in blue
# Boxplots are mostly centered in real values
coefs_test_1 %>%
  set_names(paste0("beta_mlogit_", 1:n_params)) %>%
  pivot_longer(cols = 1:n_params) %>%
  ggplot() +
  geom_boxplot(aes(name, value)) +
  geom_point(
    data = tibble(
      par = paste0("beta_mlogit_", 1:n_params),
      real_value = beta_vec_3_prime[c(1:2, 18:20, 3:17)]),
    aes(x = par, y = real_value),
    color = "blue",
    size = 3,
    inherit.aes = F
  )





