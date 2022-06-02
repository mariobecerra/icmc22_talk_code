library(tidyverse)
library(mlogit)
library(readxl)
library(here)








cocktail_data_kul_orig_01 = read_excel(here("data/cocktail_workshop_data_kuleuven.xlsx"), sheet = 2)

cocktail_data_kul_01 = cocktail_data_kul_orig_01 %>%
  mutate(choice_set = paste0(CONSUMER, "_", PAIR)) %>%
  select(choice_set, comp_1 = MANGO, comp_2 = LEMON, comp_3 = CASSIS, pv_1 = TEMP, choice = CHOSEN)


cocktail_data_mlogit_01 <- dfidx(cocktail_data_kul_01, idx = c("choice_set"))


model_cocktail_01 = mlogit(
  choice ~ -1 +
    comp_1 + comp_2 +
    comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 +
    comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1,
  data = cocktail_data_mlogit_01)

summary(model_cocktail_01)










cocktail_data_kul_02 = read_excel(here("data/cocktail_workshop_data_kuleuven.xlsx"), sheet = 3) %>%
  mutate(choice_set = paste0(CONSUMER, "_", PAIR)) %>%
  select(choice_set, comp_1 = MANGO, comp_2 = LEMON, comp_3 = CASSIS, pv_1 = TEMP, choice = CHOSEN)


cocktail_data_mlogit_02 <- dfidx(cocktail_data_kul_02, idx = c("choice_set"))



model_cocktail_02 = mlogit(
  choice ~ -1 +
    comp_1 + comp_2 +
    comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 +
    comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1,
  data = cocktail_data_mlogit_02)

summary(model_cocktail_02)












cocktail_data_kul_03 = read_excel(here("data/cocktail_workshop_data_kuleuven.xlsx"), sheet = 4) %>%
  mutate(choice_set = paste0(CONSUMER, "_", PAIR)) %>%
  select(choice_set, comp_1 = MANGO, comp_2 = LEMON, comp_3 = CASSIS, pv_1 = TEMP, choice = CHOSEN)


cocktail_data_mlogit_03 <- dfidx(cocktail_data_kul_03, idx = c("choice_set"))




model_cocktail_03 = mlogit(
  choice ~ -1 +
    comp_1 + comp_2 +
    comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 +
    comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1 +
    I(pv_1^2),
  data = cocktail_data_mlogit_03)

summary(model_cocktail_03)




model_cocktail_04 = mlogit(
  choice ~ -1 +
    comp_1 + comp_2 +
    comp_1:comp_2 + comp_1:comp_3 + comp_2:comp_3 +
    comp_1:pv_1 + comp_2:pv_1 + comp_3:pv_1,
  data = cocktail_data_mlogit_03)

summary(model_cocktail_04)














coefs_all = as.data.frame(model_cocktail_01$coefficients) %>%
  rownames_to_column() %>%
  full_join(
    as.data.frame(model_cocktail_02$coefficients) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(model_cocktail_03$coefficients) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(model_cocktail_04$coefficients) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(sqrt(diag(vcov(model_cocktail_01)))) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(sqrt(diag(vcov(model_cocktail_02)))) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(sqrt(diag(vcov(model_cocktail_03)))) %>%
      rownames_to_column()
  ) %>%
  full_join(
    as.data.frame(sqrt(diag(vcov(model_cocktail_04)))) %>%
      rownames_to_column()
  ) %>%
  set_names(c("coef", paste0("coef_value_model_0", 1:4), paste0("sd_model_0", 1:4))) %>% 
  mutate(coef = paste0(1:nrow(.), ") ", coef))





coefs_all



coefs_all_2 = coefs_all %>%
  select(coef, grep("01", names(.))) %>%
  mutate(model = "01") %>%
  set_names(c("coef", "value", "sd", "model")) %>%
  bind_rows(
    coefs_all %>%
      select(coef, grep("02", names(.))) %>%
      mutate(model = "02") %>%
      set_names(c("coef", "value", "sd", "model"))
  ) %>%
  bind_rows(
    coefs_all %>%
      select(coef, grep("03", names(.))) %>%
      mutate(model = "03") %>%
      set_names(c("coef", "value", "sd", "model"))
  ) %>% 
  bind_rows(
    coefs_all %>%
      select(coef, grep("04", names(.))) %>%
      mutate(model = "04") %>%
      set_names(c("coef", "value", "sd", "model"))
  )


coefs_all_2 %>%
  mutate(ymin = value - 1.96*sd,
         ymax = value + 1.96*sd) %>%
  ggplot(aes(x = coef, y = value, ymin = ymin, ymax = ymax, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(position = position_dodge(width = 0.3))



coefs_all_2 %>%
  mutate(ymin = value - 1.96*sd,
         ymax = value + 1.96*sd) %>%
  ggplot(aes(x = coef, y = value, ymin = ymin, ymax = ymax, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(position = position_dodge(width = 0.3)) +
  ylim(-20, 40)



# coefs_all_2 %>%
#   mutate(ymin = value - 1.96*value,
#          ymax = value + 1.96*value) %>%
#   ggplot(aes(x = coef, y = value, ymin = ymin, ymax = ymax, color = model)) +
#   geom_errorbar(position = "dodge")
# 
# 
# 
# coefs_all_2 %>%
#   mutate(ymin = value - 1.96*value,
#          ymax = value + 1.96*value) %>%
#   ggplot(aes(x = coef, y = value, ymin = ymin, ymax = ymax, color = model)) +
#   geom_crossbar(position = "dodge")
# 
# 
# 
# 
# 
# 
# Simpler graphs with only the point estimates
coefs_all %>%
  select(starts_with("coef")) %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  ggplot(aes(x = coef, y = value, color = name)) +
  geom_point(position = position_dodge(width = 0.3))










preds_orig_mod_03 = predict(model_cocktail_03, newdata = cocktail_data_mlogit_01) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  set_names(c("alt_1", "alt_2")) %>% 
  mutate(
    pred = ifelse(alt_1 >= alt_2, 1, 2),
    choice_set = unique(cocktail_data_mlogit_01$idx$choice_set)) %>% 
  inner_join(
    cocktail_data_kul_01 %>% 
      select(choice, choice_set) %>% 
      group_by(choice_set) %>% 
      summarize(choice2 = paste(choice, collapse = " ")) %>% 
      mutate(choice = ifelse(choice2 == "0 1", 2, 1))
  )


sum(preds_orig_mod_03$choice == preds_orig_mod_03$pred)

# 73% hit rate
sum(preds_orig_mod_03$choice == preds_orig_mod_03$pred)/nrow(preds_orig_mod_03)




