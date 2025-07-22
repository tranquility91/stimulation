#1-1
n_max = 300000
X = rnorm(n_max)
z975 = qnorm(0.975)


Xbar = numeric(n_max)
S2 = numeric(n_max)

# 初始化
Xbar[1] = X[1]
S2[1] = NA
M2 = 0

for (n in 2:n_max) {
    delta = X[n] - Xbar[n - 1]
    Xbar[n] = Xbar[n - 1] + delta / n
    M2 = M2 + delta * (X[n] - Xbar[n])
    S2[n] = M2 / (n - 1)
}

# 信賴區間
SE = sqrt(S2 / (1:n_max))
lower = Xbar - z975 * SE
upper = Xbar + z975 * SE

n_plot = 100000
par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))
plot(1:n_plot, Xbar[1:n_plot], type = "l", col = "blue", main = "X̄n vs n", xlab = "n", ylab = "X̄n")
plot(1:n_plot, S2[1:n_plot], type = "l", col = "darkgreen", main = "S²n vs n", xlab = "n", ylab = "S²n")
plot(1:n_plot, Xbar[1:n_plot], type = "l", col = "black", ylim = c(-0.1, 0.1),
     main = "X̄n ± z₀.₉₇₅·Sn/√n", xlab = "n", ylab = "Value")
lines(1:n_plot, lower[1:n_plot], col = "red", lty = 2)
lines(1:n_plot, upper[1:n_plot], col = "red", lty = 2)




# 1-2
sequential_stop = function(d, max_iter = 1e7) {
    
    Xbar = 0
    M2 = 0
    for (n in 1:max_iter) {
        x = rnorm(1)
        delta = x - Xbar
        Xbar = Xbar + delta / n
        M2 = M2 + delta * (x - Xbar)
        if (n > 1) {
            S2 = M2 / (n - 1)
            SE = sqrt(S2) / sqrt(n)
            if (SE < d) return(n)
        }
    }
    return(NA) 
}

n_001 = sequential_stop(0.01)
n_0001 = sequential_stop(0.001)

print(n_001)
print(n_0001)


#2-1
stop_when_ci_small = function(generator_fn, ci_width = 0.001, alpha = 0.05, max_iter = 1e7) {
    z_alpha = qnorm(1 - alpha / 2)
    Xbar = 0
    M2 = 0
    
    for (n in 1:max_iter) {
        x = generator_fn()
        delta = x - Xbar
        Xbar = Xbar + delta / n
        M2 = M2 + delta * (x - Xbar)
        
        if (n > 1) {
            S2 = M2 / (n - 1)
            CI_len = 2 * z_alpha * sqrt(S2) / sqrt(n)
            if (CI_len <= ci_width) {
                return(list(mean = Xbar, n = n, ci_len = CI_len))
            }
        }
    }
    return(NULL)
}


integral_result = stop_when_ci_small(
    generator_fn = function() {
        u = runif(1)
        exp(u^2)
    },
    ci_width = 0.001
)
print(integral_result$mean) 
print(integral_result$n) 
print(integral_result$ci_len) 

#normal way
f = function(x) exp(x^2)
result = integrate(f, lower = 0, upper = 1)
print(result$value) 



##2-2
stop_when_ci_small_flex = function(generator_fn, ci_width = 0.1, alpha = 0.05, max_iter = 1e7) {
  z_alpha = qnorm(1 - alpha / 2)
  Xbar = 0
  M2 = 0
  
  for (n in 1:max_iter) {
    x = generator_fn()
    delta = x - Xbar
    Xbar = Xbar + delta / n
    M2 = M2 + delta * (x - Xbar)
    
    if (n > 1) {
      S2 = M2 / (n - 1)
      if (S2 > 0) {  
        CI_len = 2 * z_alpha * sqrt(S2) / sqrt(n)
        if (CI_len <= ci_width) {
          return(list(mean = Xbar, n = n, ci_len = CI_len))
        }
      }
    }
  }
  return(NULL)
}

pi_result = stop_when_ci_small_flex(
  generator_fn = function() {
    u1 = runif(1)
    u2 = runif(1)
    x = 2 * u1 - 1
    y = 2 * u2 - 1
    inside = ifelse(x^2 + y^2 <= 1, 1, 0)
    4 * inside
  },
  ci_width = 0.1
)

print(pi_result$mean)
print(pi_result$n)
print(pi_result$ci_len)
# #2-2
# stop_when_ci_small_flex = function(generator_fn, ci_width = 0.1, alpha = 0.05, max_iter = 1e7, min_n = 30) {
#   z_alpha = qnorm(1 - alpha / 2)
#   Xbar = 0
#   M2 = 0
#   
#   for (n in 1:max_iter) {
#     x = generator_fn()
#     delta = x - Xbar
#     Xbar = Xbar + delta / n
#     M2 = M2 + delta * (x - Xbar)
#     
#     if (n >= min_n) { 
#       S2 = M2 / (n - 1)
#       CI_len = 2 * z_alpha * sqrt(S2) / sqrt(n)
#       if (CI_len <= ci_width) {
#         return(list(mean = Xbar, n = n, ci_len = CI_len))
#       }
#     }
#   }
#   return(NULL)
# }
# 
# 
# pi_result = stop_when_ci_small_flex(
#   generator_fn = function() {
#     u1 = runif(1)
#     u2 = runif(1)
#     x = 2 * u1 - 1
#     y = 2 * u2 - 1
#     inside = ifelse(x^2 + y^2 <= 1, 1, 0)
#     4 * inside
#   },
#   ci_width = 0.1,
#   min_n = 30  
# )
# 
# print(pi_result$mean)
# print(pi_result$n)
# print(pi_result$ci_len)



#cauchy
# --- 平均值的 bootstrap ---
n = 30
original_sample = rnorm(n, mean = 5, sd = 2)
original_mean = mean(original_sample)

B = 1000
bootstrap_means = numeric(B)

for (b in 1:B) {
    resample = sample(original_sample, size = n, replace = TRUE)
    bootstrap_means[b] = mean(resample)
}

hist(bootstrap_means, breaks = 40, probability = TRUE,
     main = "Bootstrap Distribution of Sample Mean",
     xlab = "Sample Mean", col = "skyblue")
abline(v = original_mean, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Original Sample Mean"), col = c("red"), lty = 2)

ci_mean = quantile(bootstrap_means, c(0.025, 0.975))
print(paste("Bootstrap 95% CI for mean:", ci_mean[1], "to", ci_mean[2]))
print(paste("Original sample mean:", original_mean))


# --- 中位數的 bootstrap（用 Cauchy 分布）---
original_sample = rcauchy(n)
original_median = median(original_sample)

bootstrap_medians = numeric(B)

for (b in 1:B) {
    resample = sample(original_sample, size = n, replace = TRUE)
    bootstrap_medians[b] = median(resample)
}

hist(bootstrap_medians, breaks = 40, probability = TRUE,
     main = "Bootstrap Distribution of Sample Median",
     xlab = "Sample Median", col = "lightgreen")
abline(v = original_median, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Original Sample Median"), col = c("blue"), lty = 2)

ci_median = quantile(bootstrap_medians, c(0.025, 0.975))
print(paste("Bootstrap 95% CI for median:", ci_median[1], "to", ci_median[2]))
print(paste("Original sample median:", original_median))


# --- Bootstrap MSE of sample variance ---
X = rcauchy(n)
theta_hat = var(X)
theta_boot = numeric(B)

for (b in 1:B) {
    X_star = sample(X, size = n, replace = TRUE)
    theta_boot[b] = var(X_star)
}

bootstrap_mse = mean((theta_boot - theta_hat)^2)
print(paste("原樣本估計的 θ =", theta_hat))
print(paste("Bootstrap MSE(θ̂) ≈", bootstrap_mse))

hist(theta_boot, breaks = 40, col = "lightblue",
     main = "Bootstrap Distribution of θ̂",
     xlab = "Bootstrap Estimates of θ")
abline(v = theta_hat, col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Original θ̂", col = "red", lty = 2)


# --- 理論公式 vs bootstrap 模擬的 MSE ---
x = rnorm(50, mean = 10, sd = 2)
n = length(x)
xbar = mean(x)

bootstrap_mse_formula = sum((x - xbar)^2) / n^2

B = 100000
boot_means = replicate(B, mean(sample(x, size = n, replace = TRUE)))
bootstrap_mse_empirical = mean((boot_means - xbar)^2)

print(paste("Bootstrap MSE (理論公式) =", bootstrap_mse_formula))
print(paste("Bootstrap MSE (模擬近似) =", bootstrap_mse_empirical))


##3

x = c(1, 2, 6)
n = length(x)

samples <- as.matrix(expand.grid(x, x, x))


boot_mean   = rowMeans(samples)                  
boot_median = apply(samples, 1, median)         
boot_var = apply(samples, 1, function(v) {        
  m = mean(v)
  sum((v - m)^2) / (n - 1)
})

orig_mean   = mean(x)
orig_median = median(x)
orig_var    = var(x)


mse_mean    = mean((boot_mean   - orig_mean)^2)           
var_mean    = mean((boot_mean   - mean(boot_mean))^2)      
var_median  = mean((boot_median - mean(boot_median))^2)    
mse_median  = mean((boot_median - orig_median)^2)    

var_var     = mean((boot_var    - mean(boot_var))^2)       


print(mse_mean)
print(var_mean)
print(var_median)
print(var_var)




#27種(錯的)
x = c(1, 2, 6)
n = length(x)


samples = expand.grid(x, x, x)

means = apply(samples, 1, mean)
medians = apply(samples, 1, median)
vars = apply(samples, 1, var)

orig_mean = mean(x)
orig_median = median(x)
orig_var = var(x)

mse_mean = mean((means - orig_mean)^2)

var_mean = var(means)
var_median = var(medians)
var_var = var(vars)

print(mse_mean)
print(var_mean)
print(var_median)
print(var_var)









# 1、2 理論值
s2 = var(x)
theory_var_mean = s2 / n

print(theory_var_mean)



##MSE 
set.seed(42)
x = c(1, 2, 6)
n = length(x)
B = 10000

# 原樣本統計量
theta_hat = mean(x)

# Bootstrap 模擬
boot_means = numeric(B)
for (b in 1:B) {
    x_star = sample(x, size = n, replace = TRUE)
    boot_means[b] = mean(x_star)
}

# Bootstrap MSE
mse_b = mean((boot_means - theta_hat)^2)

# 信賴區間
ci = quantile(boot_means, c(0.025, 0.975))

print(mse_b)
print(ci[1],ci[2])



x = c(5.2, 6.1, 4.8, 5.9, 5.5)
n = length(x)

mu_hat = mean(x)
sigma_hat = sd(x)

B = 10000
boot_means = numeric(B)

for (b in 1:B) {
    x_star = rnorm(n, mean = mu_hat, sd = sigma_hat)
    boot_means[b] = mean(x_star)
}

# 統計分析
ci = quantile(boot_means, c(0.025, 0.975))
print(paste("95% Parametric Bootstrap CI for mean =", ci[1], "to", ci[2]))


##4
set.seed(100)
X = rnorm(100, mean = 5, sd = 2)  
n = length(X)
theta_hat = mean(X)       


##4-1
B = 10000
boot_means_np = replicate(B, mean(sample(X, size = n, replace = TRUE)))

# Bias
bias_np = mean(boot_means_np) - theta_hat
print(bias_np)


##4-2
var_np = var(boot_means_np)
print(var_np)





##4-3
mu_hat = mean(X)
sigma_hat = sd(X)

boot_means_p = replicate(B, mean(rnorm(n, mean = mu_hat, sd = sigma_hat)))


##4-4
ci_boot = quantile(boot_means_np, c(0.025, 0.975))
print(ci_boot[1])
print(ci_boot[2])


##4-5
z975 = qnorm(0.975)
ci_exact = c(theta_hat - z975 * sd(X)/sqrt(n),
             theta_hat + z975 * sd(X)/sqrt(n))
print(ci_exact[1])
print(ci_exact[2])










# set.seed(100)
# X = rnorm(100, mean = 5, sd = 2)  # 原始樣本
# n = length(X)
# B = 10000                         # 重抽樣次數
# 
# # Step 1: 用樣本估參數
# mu_hat = mean(X)
# sigma_hat = sd(X)
# 
# # Step 2: Parametric Bootstrap
# boot_means_p = replicate(B, {
#     X_star = rnorm(n, mean = mu_hat, sd = sigma_hat)  # 從 N(μ̂, σ̂) 抽樣
#     mean(X_star)                                      # 算樣本平均
# })
# 
# # Step 3: 結果視覺化與描述
# hist(boot_means_p, breaks = 40, col = "lightblue", probability = TRUE,
#      xlab = "Bootstrap Sample Mean (Parametric)")
# 
# abline(v = mu_hat, col = "red", lwd = 2, lty = 2)
# # 比較理論分布
# curve(dnorm(x, mean = mu_hat, sd = sigma_hat / sqrt(n)), 
#       col = "darkgreen", lwd = 2, add = TRUE)














