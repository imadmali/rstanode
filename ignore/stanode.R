

clean_f(f)
trans_f(clean_f(f), 2, 2)
write_to_stan(trans_f(clean_f(f), 2, 2))

# ode_model <- stan_model(stanc_ret = stanc_builder("sho.stan"))
# del <- sampling(ode_model, data = sim_data, chains = 1, iter = 1, algorithm = "Fixed_param")
# del <- extract(del)
