library(opdesmixr)
library(tidyverse)
library(mlogit)
library(here)

# detach("package:opdesmixr", unload=TRUE)





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
## 3 ingredients and 1 process variable
######################################################################
######################################################################


q = 3
J = 2
S = 140

beta_vec_1 = c(7.562, 0.907, 5.109, 14.573, 17.1806, 19.2705, 19.2705, 19.2705, 0)

# 40 seconds in 4 cores
design_array_i_opt_1 = opdesmixr::mnl_mixture_coord_exch(
  q = q, J = J, S = S, n_pv = 1,
  beta = beta_vec_1, seed = 2021, order = 2, opt_crit = "I",
  transform_beta = F,
  n_random_starts = 4, n_cores = 4, max_it = 4)




simulated_data_i_opt_1_1 = simulate_mixture_choice_data(
  design_array_i_opt_1$X, n_pv = 1, beta = beta_vec_1, order = 2, append_model_matrix = F)


sim_data_mlogit_i_opt_1_1 <- dfidx(
  simulated_data_i_opt_1_1 %>%
    select(comp_1, comp_2, comp_3, pv_1, choice_set, choice),
  idx = c("choice_set"))


mod_1 = mlogit(choice ~ -1 + comp_1 + comp_2 + comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 + comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1 + I(pv_1^2), data = sim_data_mlogit_i_opt_1_1)

summary(mod_1)

MASS::ginv(mnl_get_information_matrix(X = design_array_i_opt_1$X, beta = beta_vec_1, order = 2, transform_beta = F, n_pv = 1))










n_params = length(beta_vec_1)
coefs_test_1 = as.data.frame(matrix(rep(NA_real_, 200*n_params), ncol = n_params))

for(i in 1:nrow(coefs_test_1)){
  if(i %% 10 == 1) print(i)
  
  
  sim_data_i = simulate_mixture_choice_data(
    design_array_i_opt_1$X, n_pv = 1, beta = beta_vec_1, order = 2, append_model_matrix = F) %>% 
    select(comp_1, comp_2, comp_3, pv_1, choice_set, choice)
  
  
  sim_data_mlogit_i <- dfidx(sim_data_i, idx = c("choice_set"))
  
  model_mlogit_i = mlogit(choice ~ -1 +
                            comp_1 + comp_2 +
                            comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 +
                            comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1 +
                            I(pv_1^2),
                          data = sim_data_mlogit_i)
  
  
  coefs_mlogit <- as.numeric(model_mlogit_i$coefficients)
  
  coefs_test_1[i,] = coefs_mlogit
  
}




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
      real_value = beta_vec_1[c(1:2, 9, 3:8)]),
    aes(x = par, y = real_value),
    color = "blue",
    size = 3,
    inherit.aes = F
  )





