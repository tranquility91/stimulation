#7
X = c(10, 11, 10.5, 11.5, 14, 8, 13, 6, 15, 10, 11.5, 10.5, 12, 8, 16, 5)
s = sd(X)
n_required = ceiling((s / 0.1)^2)
extra_needed = n_required - length(X)

print(paste("Total n required:", n_required))
print(paste("Additional needed:", extra_needed))





#8
B = 1000

N = replicate(B, {
    s = 0
    count = 0
    while (s <= 1) {
        s = s + runif(1)
        count = count + 1
    }
    count
})


est_e = mean(N)

m = length(N)
e_bar = est_e
var_e = sum((N - e_bar)^2) / m / m  
se_e = sqrt(var_e)


z975 = qnorm(0.975)
ci_lower = e_bar - z975 * se_e
ci_upper = e_bar + z975 * se_e

print(est_e)
print(var_e)
print(ci_lower)
print(ci_upper)


#13
X = c(56, 101, 78, 67, 93, 87, 64, 72, 80, 69)
n = length(X)
mu_hat = mean(X)
B = 10000
a = -5
b = 5

boot_means = replicate(B, mean(sample(X, size = n, replace = TRUE)))

diffs = boot_means - mu_hat
p_hat = mean(diffs > a & diffs < b)

print(p_hat)





#14
x = c(1, 3)
samples = expand.grid(x, x)  # 所有可能的有放回抽樣（2^2 = 4 種）
boot_vars = apply(samples, 1, function(row) var(c(row[1], row[2])))

# 自己算母體變異數（分母 = 4）
mean_var = mean(boot_vars)
var_S2_true = mean((boot_vars - mean_var)^2)

# 輸出
print("所有可能的 S² 值：")
print(boot_vars)
print("平均 S²：")
print(mean_var)
print("母體變異數 Var(S²)：")
print(var_S2_true)


