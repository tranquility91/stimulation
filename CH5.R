set.seed(0)
n = 10000
U = runif(n)
theta_sm = exp(U)
theta_av = 0.5 * (exp(U) + exp(1 - U))

var_sm = var(theta_sm)
var_av = var(theta_av)

# 百分比變異數減少
reduction_pct = (var_sm - var_av) / var_sm * 100
print(reduction_pct)


#2
z975 = qnorm(0.975)
target_len = 0.001

Xbar = 0
M2 = 0
n = 0

repeat {
    u = runif(1)
    x1 = exp(u^2)
    x2 = exp((1 - u)^2)
    x = (x1 + x2) / 2
    
    n = n + 1
    delta = x - Xbar
    Xbar = Xbar + delta / n
    M2 = M2 + delta * (x - Xbar)
    
    if (n > 1) {
        S2 = M2 / (n - 1)
        CI_len = 2 * z975 * sqrt(S2) / sqrt(n)
        if (CI_len <= target_len) break
    }
}

print(Xbar)   
print(n)       
print(CI_len)  


#2-1
set.seed(0)
n = 1e5
mu = 4
sigma = 3
a = 10

X = rnorm(n, mean = mu, sd = sigma)
Y = as.numeric(X <= a)

EX = mean(X)     
EY = mean(Y)   

print(EX)
print(EY)


# Sample mean estimator
theta_sm = mean(Y)

# Control variate estimator using X
c = -cov(Y, X) / var(X)
theta_cv = mean(Y + c * (X - mu))

# Confidence intervals
z = qnorm(0.975)
se_sm = sd(Y) / sqrt(n)
se_cv = sd(Y + c * (X - mu)) / sqrt(n)
ci_sm = c(theta_sm - z * se_sm, theta_sm + z * se_sm)
ci_cv = c(theta_cv - z * se_cv, theta_cv + z * se_cv)

print(ci_sm)
print(ci_cv)




#2-2
set.seed(0)
n = 1e5
mu = 4
sigma = 3
a = 10

X = rnorm(n, mean = mu, sd = sigma)
X_prime = 8 - X  

X1 = as.numeric(X <= a)
X2 = as.numeric(X_prime <= a)

cov_X1X2 = cov(X1, X2)
var_X1 = var(X1)
var_X2 = var(X2)
alpha = (var_X2 - cov_X1X2) / (var_X1 + var_X2 - 2 * cov_X1X2)

theta_mix = mean(alpha * X1 + (1 - alpha) * X2)


W = alpha * X1 + (1 - alpha) * X2
se = sd(W) / sqrt(n)
z = qnorm(0.975)
ci = c(theta_mix - z * se, theta_mix + z * se)


theta_true = pnorm((a - mu) / sigma)


print(round(theta_true, 6))
print(round(alpha, 6))
print(round(theta_mix, 6))
print(paste("95% CI: [", round(ci[1], 6), ",", round(ci[2], 6), "]"))














# set.seed(0)
# n = 1e5
# mu = 4
# sigma = 3
# a = 10
# 
# X = rnorm(n, mean = mu, sd = sigma)
# Y = 8 - X
# EX = mean(X)     
# EY = mean(Y)   
# 
# print(EX)
# print(EY)
# cov_XY = cov(X, Y)
# var_X = var(X)
# var_Y = var(Y)
# 
# alpha = (var_Y - cov_XY) / (var_X + var_Y - 2 * cov_XY)
# 
# theta_wrong = mean(alpha * X + (1 - alpha) * Y)
# 
# W = alpha * X + (1 - alpha) * Y
# se_wrong = sd(W) / sqrt(n)
# z = qnorm(0.975)
# ci_wrong = c(theta_wrong - z * se_wrong, theta_wrong + z * se_wrong)
# 
# print(alpha)
# print(theta_wrong)
# print(ci_wrong)
# 
# 
# 
# set.seed(0)
# n = 1e5
# mu = 4
# sigma = 3
# a = 10
# 
# # Step 1: Generate X
# X = rnorm(n, mean = mu, sd = sigma)
# Y = 8 - X
# 
# # Step 2: Compute alpha using variance and covariance
# cov_XY = cov(X, Y)
# var_X = var(X)
# var_Y = var(Y)
# alpha = (var_Y - cov_XY) / (var_X + var_Y - 2 * cov_XY)
# 
# # Step 3: Wrong estimator (form unchanged)
# theta_wrong = mean(alpha * X + (1 - alpha) * Y)
# 
# # Step 4: True θ
# theta_true = mean(X <= a)
# 
# # Step 5: Confidence interval
# W = alpha * X + (1 - alpha) * Y
# se = sd(W) / sqrt(n)
# z = qnorm(0.975)
# ci_wrong = c(theta_wrong - z * se, theta_wrong + z * se)
# 
# # Output
# cat("True θ = P(X ≤ 10):", theta_true, "\n")
# cat("Wrong estimator mean:", theta_wrong, "\n")
# cat("Bias:", theta_wrong - theta_true, "\n")
# cat("95% CI:", ci_wrong, "\n")
# cat("Alpha used:", alpha, "\n")




#5-3-1
N = 1e6

# Standard MC
U1 = runif(N)
U2 = runif(N)
theta_sm = as.numeric(U1^2 + U2^2 <= 1)
var_sm_sim = var(theta_sm)

# Conditioning
U = runif(N)
theta_c = sqrt(1 - U^2)
var_c_sim = var(theta_c)



reduction_ratio = (var_sm_sim - var_c_sim) / var_sm_sim


print(var_sm_sim)
print(var_c_sim)
print(reduction_ratio)







#5-3-2
z975 = qnorm(0.975)
target_len = 0.001

# Frame
simulate_until_ci = function(gen_func) {
  n = 0
  mean_x = 0
  M2 = 0
  repeat {
    x = gen_func()
    n = n + 1
    delta = x - mean_x
    mean_x = mean_x + delta / n
    M2 = M2 + delta * (x - mean_x)
    
    if (n > 1) {
      S = sqrt(M2 / (n - 1))
      if (S == 0) next
      ci_len = 2 * z975 * S / sqrt(n)
      if (ci_len < target_len) break
    }
  }
  return(list(n = n, pi_est = 4 * mean_x, var_pi = 16 * M2 / (n - 1)))
}

# 方法一：Antithetic Variates
antithetic_func = function() {
  u1 = runif(1)
  u2 = runif(1)
  v1 = 1 - u1
  v2 = 1 - u2
  i1 = as.numeric(u1^2 + u2^2 <= 1)
  i2 = as.numeric(v1^2 + v2^2 <= 1)
  return((i1 + i2) / 2)
}

# 方法二：Optimal Control Variate - u1 + u2
simulate_until_ci_control_1 = function() {
  n = 0
  mean_x = 0
  mean_y = 0
  Cxy = 0
  M2_y = 0
  mean_cv = 0
  M2_cv = 0
  
  repeat {
    u1 = runif(1)
    u2 = runif(1)
    x = as.numeric(u1^2 + u2^2 <= 1)
    y = u1 + u2
    
    n = n + 1
    delta_x = x - mean_x
    mean_x = mean_x + delta_x / n
    delta_y = y - mean_y
    mean_y = mean_y + delta_y / n
    
    Cxy = Cxy + delta_x * (y - mean_y)
    M2_y = M2_y + delta_y * (y - mean_y)
    
    c = if (n > 1 && M2_y > 0) -Cxy / M2_y else 0
    cv = x + c * (y - 1)
    
    delta_cv = cv - mean_cv
    mean_cv = mean_cv + delta_cv / n
    M2_cv = M2_cv + delta_cv * (cv - mean_cv)
    
    if (n > 1) {
      S = sqrt(M2_cv / (n - 1))
      if (S == 0) next
      ci_len = 2 * z975 * S / sqrt(n)
      if (ci_len < target_len) break
    }
  }
  
  return(list(n = n, pi_est = 4 * mean_cv, var_pi = 16 * M2_cv / (n - 1)))
}

# 方法二：Optimal Control Variate - u1^2 + u2^2
simulate_until_ci_control_2 = function() {
  n = 0
  mean_x = 0
  mean_y = 0
  Cxy = 0
  M2_y = 0
  mean_cv = 0
  M2_cv = 0
  
  repeat {
    u1 = runif(1)
    u2 = runif(1)
    x = as.numeric(u1^2 + u2^2 <= 1)
    y = u1^2 + u2^2
    
    n = n + 1
    delta_x = x - mean_x
    mean_x = mean_x + delta_x / n
    delta_y = y - mean_y
    mean_y = mean_y + delta_y / n
    
    Cxy = Cxy + delta_x * (y - mean_y)
    M2_y = M2_y + delta_y * (y - mean_y)
    
    c = if (n > 1 && M2_y > 0) -Cxy / M2_y else 0
    cv = x + c * (y - mean_y)
    
    delta_cv = cv - mean_cv
    mean_cv = mean_cv + delta_cv / n
    M2_cv = M2_cv + delta_cv * (cv - mean_cv)
    
    if (n > 1) {
      S = sqrt(M2_cv / (n - 1))
      if (S == 0) next
      ci_len = 2 * z975 * S / sqrt(n)
      if (ci_len < target_len) break
    }
  }
  
  return(list(n = n, pi_est = 4 * mean_cv, var_pi = 16 * M2_cv / (n - 1)))
}

# 方法三：Conditioning
conditioning_func = function() {
  u = runif(1)
  return(sqrt(1 - u^2))
}

# 方法四：Conditioning + Antithetic
simulate_until_ci_cond_anti = function() {
  n = 0
  mean_x = 0
  M2 = 0
  
  repeat {
    u = runif(1)
    v = 1 - u
    x = (sqrt(1 - u^2) + sqrt(1 - v^2)) / 2
    
    n = n + 1
    delta = x - mean_x
    mean_x = mean_x + delta / n
    M2 = M2 + delta * (x - mean_x)
    
    if (n > 1) {
      S = sqrt(M2 / (n - 1))
      if (S == 0) next
      ci_len = 2 * z975 * S / sqrt(n)
      if (ci_len < target_len) break
    }
  }
  
  return(list(n = n, pi_est = 4 * mean_x, var_pi = 16 * M2 / (n - 1)))
}

# 執行所有模擬
res_anti = simulate_until_ci(antithetic_func)
res_ctrl_1 = simulate_until_ci_control_1()
res_ctrl_2 = simulate_until_ci_control_2()
res_cond = simulate_until_ci(conditioning_func)
res_cond_anti = simulate_until_ci_cond_anti()

# 輸出（含變異數）
print(res_anti$n)
print(res_anti$pi_est)
print(res_anti$var_pi)

print(res_ctrl_1$n)
print(res_ctrl_1$pi_est)
print(res_ctrl_1$var_pi)

print(res_ctrl_2$n)
print(res_ctrl_2$pi_est)
print(res_ctrl_2$var_pi)

print(res_cond$n)
print(res_cond$pi_est)
print(res_cond$var_pi)

print(res_cond_anti$n)
print(res_cond_anti$pi_est)
print(res_cond_anti$var_pi)




















#5-4
n_list = c(5, 10, 100, 500, 1000, 5000)
true_pi = pi


h = function(u) {
    sqrt(1 - u^2)
}

# MC
estimate_conditioning = function(n) {
    U = runif(n)
    theta = mean(h(U))
    return(4 * theta)
}

# 2
estimate_stratified = function(n) {
    U = runif(n)
    U_strat = (U + (0:(n - 1))) / n
    theta = mean(h(U_strat))
    return(4 * theta)
}

# 3
estimate_stratified_antithetic = function(n) {
    U = runif(n)
    U1 = (U + (0:(n - 1))) / n
    U2 = ((1 - U) + (0:(n - 1))) / n
    theta = mean((h(U1) + h(U2)) / 2)
    return(4 * theta)
}


results = data.frame(
    n = n_list,
    conditioning = NA,
    stratified = NA,
    strat_anti = NA
)


for (i in seq_along(n_list)) {
    n = n_list[i]
    results$conditioning[i] = estimate_conditioning(n)
    results$stratified[i] = estimate_stratified(n)
    results$strat_anti[i] = estimate_stratified_antithetic(n)
}

print(results)





