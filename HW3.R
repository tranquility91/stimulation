#6
# Inverse
generate_truncated_exp_inverse = function(n) {
    u = runif(n)
    x = -log(1 - u * (1 - exp(-0.05)))
    return(x)
}

set.seed(2025)
samples6_inv = generate_truncated_exp_inverse(1000)


f6 = function(x) {
    exp(-x) / (1 - exp(-0.05))
}

hist(samples6_inv, probability = TRUE, breaks = 60)
curve(f6(x), from = 0.00001, to = 0.0499, col = "red", lwd = 2, add = TRUE)



E_sim_inv = mean(samples6_inv)

E_theory = integrate(function(x) x * exp(-x) / (1 - exp(-0.05)), lower = 0, upper = 0.05)$value

print(E_theory)
print(E_sim_inv)


#10
simulate_claim_prob = function(n_sim = 10000) {
    exceed_count = 0
    for (i in 1:n_sim) {
        claims = rbinom(1000, 1, 0.05)
        amounts = rexp(1000, rate = 1/800) * claims
        if (sum(amounts) > 50000) {
            exceed_count = exceed_count + 1
        }
    }
    return(exceed_count / n_sim)
}

simulate_claim_prob()


# #15
# generate_gamma2_inverse_numeric <- function(n, step = 0.001, xmax = 15) {
#     u <- runif(n)
#     x_grid <- seq(0, xmax, by = step)
#     cdf <- cumsum(x_grid * exp(-x_grid)) * step / gamma(2)
#     samples <- numeric(n)
#     
#     for (i in 1:n) {
#         samples[i] <- x_grid[which(cdf >= u[i])[1]]
#     }
#     return(samples)
# }
# samples15 <- generate_gamma2_inverse_numeric(10000)
# f15 <- function(x) {
#     ifelse(x >= 0, x * exp(-x), 0)
# }
# 
# # 畫圖
# hist(samples15, probability = TRUE, breaks = 60,
#      main = "Q15: Gamma(2,1) via Composition", xlab = "x")
# curve(f15(x), col = "red", lwd = 2, add = TRUE)
# generate_gamma2_composition <- function(n) {
#     x <- -log(runif(n)) + -log(runif(n))
#     return(x)
# }
# 
# generate_gamma2_ar_rigorous <- function(n) {
#     samples <- numeric(n)
#     i <- 1
#     c <- 4 / exp(1)  # ≈ 1.4715
#     while (i <= n) {
#         y <- rexp(1, rate = 0.5)  # proposal: Exp(0.5)
#         u <- runif(1)
#         if (u <= (2 * y * exp(-0.5 * y)) / c) {
#             samples[i] <- y
#             i <- i + 1
#         }
#     }
#     return(samples)
# }
# 
# # 模擬
# samples15_comp <- generate_gamma2_composition(10000)
# samples15_ar <- generate_gamma2_ar_rigorous(10000)
# 
# # 理論密度
# f15 <- function(x) {
#     ifelse(x >= 0, x * exp(-x), 0)
# }
# 
# # 畫圖
# hist(samples15_comp, probability = TRUE, breaks = 60,
#      main = "Q15: Gamma(2,1) via Composition", xlab = "x")
# curve(f15(x), col = "red", lwd = 2, add = TRUE)
# 
# hist(samples15_ar, probability = TRUE, breaks = 60,
#      main = "Q15: Gamma(2,1) via AR", xlab = "x")
# curve(f15(x), col = "red", lwd = 2, add = TRUE)
# 
# 
# #19
# generate_F_inverse <- function(n) {
#     u <- runif(n)
#     x <- (-1 + sqrt(1 + 8 * u)) / 2
#     return(x)
# }
# 
# 
# 
# generate_F_ar <- function(n) {
#     samples <- numeric(n)
#     i <- 1
#     total_trials <- 0
#     c <- 1.5  # 因為最大密度為 (1 + 2*1)/2 = 1.5
#     
#     while (i <= n) {
#         y <- runif(1)
#         u <- runif(1)
#         total_trials <- total_trials + 1
#         if (u <= ((1 + 2 * y) / 2) / c) {
#             samples[i] <- y
#             i <- i + 1
#         }
#     }
#     cat("Average trials per sample (AR method):", total_trials / n, "\n")
#     return(samples)
# }
# 
# 
# 
# generate_F_composition <- function(n) {
#     u1 <- runif(n)
#     u2 <- runif(n)
#     x <- ifelse(u1 < 0.5, u2, sqrt(u2))
#     return(x)
# }
# 
# # 模擬
# samples19_inv <- generate_F_inverse(10000)
# samples19_ar <- generate_F_ar(10000)
# samples19_comp <- generate_F_composition(10000)
# 
# # 理論密度
# f19 <- function(x) {
#     ifelse(x >= 0 & x <= 1, (1 + 2 * x) / 2, 0)
# }
# 
# # 畫圖
# hist(samples19_inv, probability = TRUE, breaks = 50,
#      main = "Q19: Inverse Method", xlab = "x")
# curve(f19(x), col = "red", lwd = 2, add = TRUE)
# 
# hist(samples19_ar, probability = TRUE, breaks = 50,
#      main = "Q19: Acceptance-Rejection", xlab = "x")
# curve(f19(x), col = "red", lwd = 2, add = TRUE)
# 
# hist(samples19_comp, probability = TRUE, breaks = 50,
#      main = "Q19: Composition Method", xlab = "x")
# curve(f19(x), col = "red", lwd = 2, add = TRUE)







