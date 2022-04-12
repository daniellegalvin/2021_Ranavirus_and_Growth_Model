
#simulating prior data
# N=1000 #Number of simulations

# prior_sim <- tibble(a = rnorm(N, 0.5, 0.5),
#                     b_sp = rnorm(N, 1, 0.5),
#                     b_sp_interaxn = rnorm(N, 1, 0.5),
#                     b = rnorm(N, 5, 0.5),
#                     b10 = rnorm(N, 1, 0.5),
#                     b100 = rnorm(N, 0.5, 0.5),
#                     b1000 = rnorm(N, 0.25, 0.5),
#                     sigma = rexp(N, 0.05),
#                     sim = 1:N) 
# 
# prior_and_x_1 <- prior_sim %>% 
#   mutate(lambda_int = exp(a + b*1 + b10*0 + b100*0 + b1000*0 +),
#          lambda_trt10 = exp(a + b10*1 + b100*0 + b1000*0),
#          lambda_trt100 = exp(a + b10*0 + b100*1 + b1000*0),
#          lambda_trt1000 = exp(a + b10*0 + b100*0 + b1000*1)) %>% 
#   pivot_longer (cols = c(lambda_int, lambda_trt10,lambda_trt100,lambda_trt1000), values_to = "lambda") %>% 
#   mutate(y = rpois(nrow(.), lambda=lambda)) 
# 
# prior_and_x_1 %>% 
#   ggplot(aes(x = name, y = lambda, group = sim)) + 
#   # geom_line() +
#   geom_point(aes(y = lambda)) +
#   labs(x = "treatment group", y = "days alive")+
#   scale_y_log10()



#running a predictive model in brms
d_new <- real_data_alive %>%
  mutate(trt_cat = as.character(treatment_group))

get_prior(days_survived ~ 1 + trt_cat*species,
family = poisson(link="log"), data = d_new)

prior_test <- brm(days_survived ~ 1 + trt_cat*species,
                  family = poisson(link="log"),
                  data = d_new,
                  prior = c(prior(normal(-0.05,0.02), class = "b", coef = speciesr_pipiens),
                            prior(normal(-0.2,0.1), class = "b"),
                            prior(normal(3,0.75), class = "Intercept")),
                  sample_prior = "only",
                  iter = 1000, chains = 1)

prior_test
plot(conditional_effects(prior_test)) # T shows your data points in the graph



# Posteriors #, this is not complete, I copied and pasted from my blue sucker data.
# could be useful eventually when you want to simulate posterior predictions

posteriors <- as_draws_df(prior_test) %>% as_tibble() %>% expand_grid(length_s = length_s) %>%
  left_join(d %>% distinct(length_s, weight_s)) %>% 
  mutate(mu = exp(b_Intercept + b_weight_s*0 + b_length_s*length_s + 
                    `b_length_s:weight_s`*length_s*0),
         mu_prior = exp(prior_Intercept + prior_b_weight_s*0 + prior_b_length_s*length_s + 
                          `prior_b_length_s:weight_s`*length_s*0))

ggplot(posteriors_bsr_brm %>% filter(.draw <=1000), aes(x=length_s, y=mu_prior)) + 
  geom_point() +
  geom_line(aes(group  = .draw), alpha = 0.1)
