
generate_x <- function(n) {
    U <- runif(1)          # 1. 產生一個 Uniform(0,1) 的亂數 U
    X <- U^(1/n)           # 2. 計算 X = U^(1/n)
    return(X)
}

# 產生 10000 筆樣本，n = 3 為例
n <- 3
samples <- replicate(10000, generate_x(n))

# 畫出直方圖，與理論 PDF 比較：f(x) = n * x^(n-1)
hist(samples, probability = TRUE, breaks = 50, 
     main = paste("Sampling from F(x) = x^", n, sep = ""), xlab = "x")
curve(n * x^(n - 1), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "Theoretical PDF"), col = c("black", "red"), lwd = 2)



#gamma範例
gamma_sample <- function(n, beta) {
    U <- runif(n)              
    X <- -beta * log(prod(U))  
    return(X)
}

samples <- replicate(10000, gamma_sample(n = 5, beta = 2))


hist(samples, probability = TRUE, breaks = 50, main = "Gamma(5,2) Sampling", xlab = "x")
curve(dgamma(x, shape = 5, scale = 2), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Sample", "Theoretical Gamma(5,2)"), col = c("black", "red"), lwd = 2)





#1-1
generate_x = function() {
    U = runif(1)          
    X = log(U*(exp(1)-1))         
    return(X)
}

samples = replicate(10000, generate_x())

hist(samples, probability = TRUE, breaks = 50, 
     main = "Sampling from f(x) = e^x / (e - 1)", xlab = "x")

# 理論PDF
curve(exp(x) / (exp(1) - 1), col = "red", lwd = 2, add = TRUE)



#1-2
generate_x = function(a, b) {
    U = runif(1)          
    X = (log(1-U)/-a)^(1/b)         
    return(X)
}

samples = replicate(10000, generate_x(3,2))

hist(samples, probability = TRUE, breaks = 50, 
     main = "Weibull(α = 3, β = 2)", xlab = "x")

# PDF:f(x) = αβx^(β-1)exp(−αx^β)
curve({
    3 * 2 * x^(2 - 1) * exp(-3 * x^2)
}, add = TRUE, col = "red", lwd = 2)


#1-3
gamma_sample <- function(n, beta) {
    U = runif(n)              
    X = -beta * log(prod(U))  
    return(X)
}


samples = replicate(10000, gamma_sample(n = 10, beta = 5))


samples_rgamma = rgamma(10000, shape = 10, scale = 5)


ks_result = ks.test(samples, samples_rgamma)

print(ks_result)


hist(samples, probability = TRUE, breaks = 50, col = rgb(0, 0, 1, 0.4),
     main = "Gamma(10,5) from Exponential sum vs rgamma()", xlab = "x")
hist(samples_rgamma, probability = TRUE, breaks = 50, col = rgb(1, 0, 0, 0.4), add = TRUE)
legend("topright", legend = c("Exp sum", "rgamma"), fill = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)))


#ar_method for beta distribution(2,4)

f = function(x) {
    20 * x * (1 - x)^3
}

g = function(x) {
    1
}

c = 135 / 64


accept_reject_beta = function(n_samples) {
    samples = numeric(n_samples)
    i = 1
    total_trials = 0
    
    while (i <= n_samples) {
        Y = runif(1)
        U = runif(1)
        
        if (U <= f(Y) / (c * g(Y))) {
            samples[i] = Y
            i = i + 1
        }
        
        total_trials = total_trials + 1
    }
    
    print(n_samples / total_trials)
    return(samples)
}

samples = accept_reject_beta(10000)

hist(samples, probability = TRUE, breaks = 50,
     main = "Sampling from f(x) = 20x(1-x)^3 using Acceptance-Rejection",
     xlab = "x")
curve(f(x), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "Target Density f(x)"), col = c("black", "red"), lwd = 2)




#AR_method for gamma(1.5,1)

f = function(x) {
    2 * sqrt(x) * exp(-x) / sqrt(pi)
}

g = function(x) {
    (2 / 3) * exp(-2 * x / 3)
}


c = sqrt(27) * exp(-0.5) / sqrt(2 * pi) 

accept_reject_gamma = function(n_samples) {
    samples = numeric(n_samples)
    i = 1
    total_trials = 0
    
    while (i <= n_samples) {
        U1 = runif(1)
        U2 = runif(1)
        Y = -1.5 * log(U2)  # Y ~ Exp(1.5)
        
        if (U1 <= f(Y) / (c * g(Y))) {
            samples[i] = Y
            i = i + 1
        }
        
        total_trials = total_trials + 1
    }
    
    print(n_samples / total_trials)
    return(samples)
}


samples = accept_reject_gamma(10000)


hist(samples, probability = TRUE, breaks = 50,
     main = "Sampling from Gamma(1.5,1) using Acceptance-Rejection",
     xlab = "x")
curve(dgamma(x, shape = 1.5, scale = 1), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "Gamma(1.5,1) PDF"),
       col = c("black", "red"), lwd = 2)


#AR_method for Normal
generate_normal_via_rejection <- function(n_samples) {
    samples <- numeric(n_samples)
    i <- 1
    total_trials <- 0
    
    while (i <= n_samples) {
        Y1 <- rexp(1)  
        Y2 <- rexp(1)  
        
        if (Y2 >= (Y1 - 1)^2 / 2) {
            sign <- sample(c(-1, 1), 1) 
            samples[i] <- sign * Y1
            i <- i + 1
        }
        
        total_trials <- total_trials + 1
    }
    
    
    return(samples)
}

samples = generate_normal_via_rejection(10000)

hist(samples, probability = TRUE, breaks = 50,
     main = "Sampling from N(0,1) via Acceptance-Rejection",
     xlab = "x")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "N(0,1) PDF"), col = c("black", "red"), lwd = 2)


#AR_截斷_gamma
generate_truncated_gamma <- function(n_samples) {
    samples <- numeric(n_samples)
    i <- 1
    total_trials <- 0
    
    while (i <= n_samples) {
        U1 <- runif(1)
        U2 <- runif(1)
        Y <- -2 * log(U2) + 5  
        
        if (U1 <= (Y / 5) * exp(-(Y - 5) / 2)) {
            samples[i] <- Y
            i <- i + 1
        }
        
        total_trials <- total_trials + 1
    }
    
    cat("Acceptance rate:", n_samples / total_trials, "\n")
    return(samples)
}


samples <- generate_truncated_gamma(10000)

hist(samples, probability = TRUE, breaks = 50,
     main = "Truncated Gamma(2,1) on x > 5 using Acceptance-Rejection",
     xlab = "x")

curve(x * exp(-x) / (6 * exp(-5)), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "Target Density f(x)"), col = c("black", "red"), lwd = 2)


#2-1
generate_normal_5_4 <- function(n_samples) {
    samples <- numeric(n_samples)
    i <- 1
    total_trials <- 0
    
    while (i <= n_samples) {
        Y1 <- rexp(1)  # Exp(1)
        Y2 <- rexp(1)  # Exp(1)
        
        if (Y2 >= (Y1 - 1)^2 / 2) {
            sign <- sample(c(-1, 1), 1)  
            Z <- sign * Y1               
            X <- 5 + 2 * Z               
            samples[i] <- X
            i <- i + 1
        }
        
        total_trials <- total_trials + 1
    }
    
    return(samples)
}


samples <- generate_normal_5_4(10000)


hist(samples, probability = TRUE, breaks = 50,
     main = "Sampling from N(5,4) using Acceptance-Rejection",
     xlab = "x")
curve(dnorm(x, mean = 5, sd = 2), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "N(5,4) PDF"), col = c("black", "red"), lwd = 2)


generate_normal_5_4_direct <- function(n_samples) {
    samples <- numeric(n_samples)
    i <- 1
    total_trials <- 0
    
    while (i <= n_samples) {
        U1 <- runif(1)
        U2 <- runif(1)
        
        # Sample from shifted Exponential(1/2): mean = 2, shift = 5
        Y <- -2 * log(U2) + 5
        
        # Acceptance condition
        if (U1 <= exp(- (Y - 7)^2 / 8)) {
            # Add random sign to get full N(5,4)
            sign <- sample(c(-1, 1), 1)
            X <- 5 + sign * (Y - 5)
            samples[i] <- X
            i <- i + 1
        }
        
        total_trials <- total_trials + 1
    }
    
    cat("Acceptance rate:", n_samples / total_trials, "\n")
    return(samples)
}

samples <- generate_normal_5_4_direct(10000)

hist(samples, probability = TRUE, breaks = 50,
     main = "Sampling from N(5,4) using shifted-Exp rejection",
     xlab = "x")
curve(dnorm(x, mean = 5, sd = 2), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Sample", "N(5,4) PDF"), col = c("black", "red"), lwd = 2)



# 先產生兩組樣本
samples1 <- generate_normal_5_4(10000)
samples2 <- generate_normal_5_4_direct(10000)

# KS 檢定：比較兩組樣本是否來自相同分布
ks_result <- ks.test(samples1, samples2)

# 輸出結果
print(ks_result)




#2-2
# (1) 使用接受拒絕法產生 Gamma(2,1) 條件 X >= 5
set.seed(2025)
generate_trunc_gamma_ar <- function(n_samples) {
    samples <- numeric(n_samples)
    total_trials <- 0
    
    for (i in 1:n_samples) {
        accepted <- FALSE
        while (!accepted) {
            U1 <- runif(1)
            U2 <- runif(1)
            Y <- -2 * log(U2) + 5
            
            if (U1 <= (Y / 5) * exp(-(Y - 5)/2)) {
                samples[i] <- Y
                accepted <- TRUE
            }
            total_trials <- total_trials + 1
        }
    }
    cat("(1) Acceptance-Rejection average iterations:", total_trials / n_samples, "\n")
    return(samples)
}

# (2) 使用 inverse transform 抽 gamma 再 reject 掉 < 5 的
gamma_sample <- function(n, beta = 1) {
    U <- runif(n)
    X <- -beta * log(prod(U))
    return(X)
}

# 加上 rejection（條件 X >= 5）
generate_trunc_gamma_inverse <- function(n_samples) {
    samples <- numeric(n_samples)
    total_trials <- 0
    i <- 1
    
    while (i <= n_samples) {
        X <- gamma_sample(n = 2, beta = 1)  # Gamma(2,1)
        total_trials <- total_trials + 1
        
        if (X >= 5) {
            samples[i] <- X
            i <- i + 1
        }
    }
    
    cat("(2) Inverse Transform (page 5) average iterations:", total_trials / n_samples, "\n")
    return(samples)
}

samples_ar <- generate_trunc_gamma_ar(100000)
samples_inv <- generate_trunc_gamma_inverse(100000)

# (3) 比較兩種方法產生的樣本是否統計相同（KS test）
ks_result <- ks.test(samples_ar, samples_inv)
print(ks_result)

# 畫圖比較
hist(samples_ar, probability = TRUE, breaks = 50, col = rgb(0, 0, 1, 0.4),
     main = "Truncated Gamma(2,1) ≥ 5 by Two Methods", xlab = "x", ylim = c(0, 0.2))
hist(samples_inv, probability = TRUE, breaks = 50, col = rgb(1, 0, 0, 0.4), add = TRUE)
legend("topright", legend = c("Accept-Reject", "Inverse Transform"),
       fill = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)))




#3-1
#(a)
generate_a = function(n_samples) {
    samples = numeric(n_samples)
    
    for (i in 1:n_samples) {
        U1 = runif(1)
        U2 = runif(1)
        
        if (U1 < 1/3) {
            samples[i] = U2
        } else if (U1 < 2/3) {
            samples[i] = U2^(1/3)
        } else {
            samples[i] = U2^(1/5)
        }
    }
    
    return(samples)
}

f_a = function(x) {
    (1/3) + x^2 + (5/3) * x^4
}

samples_a = generate_a(100000)


hist(samples_a, probability = TRUE, breaks = 50)
curve(f_a(x), from = 0, to = 1, col = "red", lwd = 2, add = TRUE)

#3(b)
generate_b_AR = function(n_samples) {
    samples = numeric(n_samples)
    i = 1
    total_trials = 0
    p1 = (1 - exp(-2) + 2) / 3
    
    while (i <= n_samples) {
        total_trials = total_trials + 1
        if (runif(1) <= p1) {
            repeat {
                x_candidate = runif(1)
                f_target = (2 * exp(-2 * x_candidate) + 2) / 3
                f_proposal = 1
                c = 4 / 3
                accept_prob = f_target / (c * f_proposal)
                if (runif(1) <= accept_prob) {
                    samples[i] = x_candidate
                    i = i + 1
                    break
                }
                total_trials = total_trials + 1
            }
        } else {
            repeat {
                x_candidate = rexp(1, rate = 2)
                if (x_candidate >= 1 && runif(1) <= 1/3) {
                    samples[i] = x_candidate
                    i = i + 1
                    break
                }
                total_trials = total_trials + 1
            }
        }
    }
    
    return(samples)
}




f_b = function(x) {
    ifelse(x < 1, (2 * exp(-2 * x) + 2) / 3, 2 * exp(-2 * x) / 3)
}

samples_b = generate_b_AR(100000)

hist(samples_b, probability = TRUE, breaks = 50)
curve(f_b(x), from = 0, to = 4, col = "red", lwd = 2, add = TRUE)




# generate_b_AR_inverse = function(n_samples) {
#     samples = numeric(n_samples)
#     i = 1
#     total_trials = 0
#     p1 = (1 - exp(-2) + 2) / 3
# 
#     while (i <= n_samples) {
#         total_trials = total_trials + 1
#         if (runif(1) <= p1) {
#             repeat {
#                 x_candidate = runif(1)
#                 f_target = (2 * exp(-2 * x_candidate) + 2) / 3
#                 f_proposal = 1
#                 c = 4 / 3
#                 accept_prob = f_target / (c * f_proposal)
#                 if (runif(1) <= accept_prob) {
#                     samples[i] = x_candidate
#                     i = i + 1
#                     break
#                 }
#                 total_trials = total_trials + 1
#             }
#         } else {
#             U = runif(1)
#             x = -0.5 * log(U) + 1
#             samples[i] = x
#             i = i + 1
#         }
#     }
# 
#     return(samples)
# }
# 
# samples = generate_b_AR_inverse(100000)
# 
# hist(samples, probability = TRUE, breaks = 50)
# curve(f_b(x), from = 0, to = 4, col = "red", lwd = 2, add = TRUE)




#3-2
generate_gamma_1_1 = function(n_samples) {
    U = runif(n_samples)
    -log(U)
}

generate_standard_normal_ar = function(n_samples) {
    samples = numeric(n_samples)
    i = 1
    total_trials = 0
    
    while (i <= n_samples) {
        Y1 = -log(runif(1)) 
        Y2 = -log(runif(1))  
        
        if (Y2 >= (Y1 - 1)^2 / 2) {
            sign = sample(c(-1, 1), 1)
            samples[i] = sign * Y1
            i = i + 1
        }
        
        total_trials = total_trials + 1
    }
    
    return(samples)
}

generate_t2_composition = function(n_samples) {
    V = generate_gamma_1_1(n_samples)  
    Y = 1 / V                           
    Z = generate_standard_normal_ar(n_samples)
    X = sqrt(Y) * Z                
    return(X)
}


set.seed(40)
samples_t2 = generate_t2_composition(10000)

hist(samples_t2, probability = TRUE, breaks = 60)
curve(dt(x, df = 2), col = "red", lwd = 2, add = TRUE)



