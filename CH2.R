#2-1
set.seed(20250311)
n=10000
f1 = function(x) exp(x)
int_1 = integrate(f1, 0, 1)$value
U = runif(n, 0, 1)
I_hat = mean(exp(U)) 


#2-2
f2 = function(x) exp(x + x^2)
int_2 = integrate(f2, -2, 2)$value

x_mc = runif(n, -2, 2)
mc_2 = 4*mean(exp(x_mc + x_mc^2))


#2-3
f3 = function(x) exp(-x^2)
int_3 = integrate(f3, -Inf, Inf)$value


X = rnorm(n, mean = 0, sd = 1)

I_hat = sqrt(2*pi) * mean(exp(-X^2 / 2))

#2-4
compute_N = function() {
  total = 0
  count = 0
  while (total <= 1) {
    total = total + runif(1)
    count = count + 1
  }
  return(count)
}



N_100 = replicate(100, compute_N())
E_N_100 = mean(N_100)


N_1000 = replicate(1000, compute_N())
E_N_1000 = mean(N_1000)


N_10000 = replicate(10000, compute_N())
E_N_10000 = mean(N_10000)


N_10000 = replicate(100000, compute_N())
E_N_100000 = mean(N_10000)


print(E_N_100)
print(E_N_1000)
print(E_N_10000)
print(E_N_100000)
print(exp(1))



#3-1
set.seed(20250311) 

X_values = c(1, 3, 5, 7, 9)


p = c(0.1, 0.2, 0.2, 0.2, 0.3)


cdf = cumsum(p)

generate_X = function(n) {
    U = runif(n)  
    X = sapply(U, function(u) {
        return(X_values[min(which(u < cdf))])
    })
    return(X)
}

samples = generate_X(10000)
hist(samples, breaks = seq(0, 10, by = 2))


#3-2
set.seed(20250311) 
X_values = seq(1, 19, by = 2)  
p_values = rep(1 / length(X_values), length(X_values))


cdf_values = cumsum(p_values)


U = runif(10000)


samples_inverse_transform = X_values[sapply(U, function(u) which.max(cdf_values >= u))]


hist(samples_inverse_transform, breaks = seq(0, 20, by = 2))


n = 10000
samples2 = X_values[floor(runif(n, min = 1, max = length(X_values) + 1))]


hist(samples2, breaks = seq(0, 20, by = 2))




#geo
set.seed(42)

simulate_geometric = function(p) {
    n = 0
    while (TRUE) {
        n = n + 1  
        U = runif(1)  
        if (U < p) {  
            return(n)
        }
    }
}

p = 0.3
n_samples = 10000
X1 = replicate(n_samples, simulate_geometric(p))

hist(X1, breaks = 50, probability = TRUE)  #這是最直觀的幾何分布 但若P很小要算很久



#高效方法
generate_geometric_inverse <- function(p, n) {
    U = runif(n) 
    X = floor(log(U) / log(1 - p)) + 1 
    return(X)
}

p = 0.3
n_samples = 100000
X2 = generate_geometric_inverse(p, n_samples)


hist(X2, breaks = 50, probability = TRUE)


#ber
set.seed(42)

n = 1000   
p = 0.04  

U = runif(n)  
X = as.integer(U < p)  

print(X)

#直接跳過中間0(用幾何
set.seed(42)

n = 100 
p = 0.04  
X = rep(0, n)

m = 0 
while (m < n) {
    U = runif(1)  
    k = floor(log(U) / log(1 - p)) + 1 
    
    if (m + k <= n) {
        X[m + k] = 1  
        m = m + k 
    } else {
        break
    }
}

print(X) 


#Inverse for bin
set.seed(42)

n = 100000 
p = 0.03  

U = runif(1)  
X = qbinom(U, size = n, prob = p)  
print(X)


#n大p小時possion

set.seed(42)
n = 100000  
p = 0.03  
X = rpois(1, lambda = n * p) 
print(X)





#4-1
set.seed(42)

n_samples = 10000  
p = 0.4  

simulate_geometric = function(p) {
    n = 0 
    while (TRUE) {
        n = n + 1 
        U = runif(1)
        if (U < p) { 
            return(n)
        }
    }
}
X1 = replicate(n_samples, simulate_geometric(p))


#inverse method(有反函數的)
geo_inverse = function(p, n) {
    U = runif(n) 
    X = floor(log(U) / log(1 - p)) + 1 
    return(X)
}
X2 = geo_inverse(p, n_samples)


#exp逼近
geo_exp = function(p, n) {
    lambda = -log(1 - p) 
    X = ceiling(rexp(n, rate = lambda)) 
    return(X)
}
X3 = geo_exp(p, n_samples)



par(mfrow = c(1, 3))
hist(X1, breaks = 50, probability = TRUE)

hist(X2, breaks = 50, probability = TRUE)
hist(X3, breaks = 50, probability = TRUE)




#4-2
set.seed(42)

n = 100   
p = 0.3  
mu = n * p 
sigma = sqrt(n * p * (1 - p)) 

P_Y_eq_25_norm = pnorm(25.5, mean = mu, sd = sigma) - pnorm(24.5, mean = mu, sd = sigma)
P_25_to_30_norm = pnorm(30.5, mean = mu, sd = sigma) - pnorm(24.5, mean = mu, sd = sigma)

samples = rbinom(10000, size = n, prob = p) 
P_Y_eq_25_sim = mean(samples == 25)  
P_25_to_30_sim = mean(samples >= 25 & samples <= 30) 


print(P_Y_eq_25_norm)
print(P_25_to_30_norm)
print(P_Y_eq_25_sim )
print(P_25_to_30_sim )


#a_r_method
set.seed(42)


P = c(0.11, 0.12, 0.09, 0.08, 0.12, 0.10, 0.09, 0.09, 0.10, 0.10)

# 最大比值 c
c = max(P) / (1 / 10) 


generate_discrete_AR = function(n) {
    samples = numeric(n)
    count = 0
    
    while (count < n) {
        U1 = runif(1)
        Y = floor(10 * U1) + 1  
        
        U2 = runif(1)  
        
        if (U2 <= P[Y] / 0.12) {
            count = count + 1
            samples[count] = Y
        }
    }
    
    return(samples)
}

samples = generate_discrete_AR(10000)


hist(samples, breaks = seq(0.5, 10.5, by = 1), probability = TRUE, col = "lightblue",
     main = "Simulated Discrete Distribution", xlab = "X")


x_vals = 1:10
lines(x_vals, P, col = "red", type = "o", lwd = 2)
legend("topright", legend = c("Simulated", "Theoretical"), col = c("blue", "red"), lwd = 2)



#5-1

set.seed(42)

P = c(0.17, 0.23, 0.15, 0.10, 0.35)


c = max(P/(1/5))

generate_discrete_AR = function(n) {
    samples = numeric(n)
    count = 0
    
    while (count < n) {
        U1 = runif(1)
        Y = floor(5 * U1) + 1 
        
        U2 = runif(1)
        
        if (U2 <= P[Y] / 0.35) {
            count = count + 1
            samples[count] = Y
        }
    }
    
    return(samples)
}

samples = generate_discrete_AR(10000)


hist(samples, breaks = seq(0.5, 5.5, by = 1), probability = TRUE, col = "lightblue")

#theo
x_vals = 1:5
lines(x_vals, P, col = "red", type = "o", lwd = 2)


#5-2
set.seed(42)


P = c(0.11, 0.09, 0.11, 0.09, 0.11, 0.09, 0.11, 0.09, 0.11, 0.09)
values = 5:14  


c = max(P/(1/10))


generate_discrete_AR = function(n) {
    samples = numeric(n)
    count = 0
    
    while (count < n) {
        U1 = runif(1)
        Y = floor(10 * U1) + 5  
        
        U2 = runif(1) 
        
        index = which(values == Y) 
        
        if (U2 <= P[index] / 0.11) {
            count = count + 1
            samples[count] = Y
        }
    }
    
    return(samples)
}


samples = generate_discrete_AR(10000)


hist(samples, breaks = seq(4.5, 14.5, by = 1), probability = TRUE, col = "lightblue")

x_vals = 5:14
lines(x_vals, P, col = "red", type = "o", lwd = 2)


#5-3
set.seed(2025)

lambda = 20
k = 15
n_samples = 10000

#inverse method
P_trunc = dpois(0:k, lambda)
P_trunc = P_trunc / sum(P_trunc)
F_trunc = cumsum(P_trunc) 

generate_poisson_inverse_trunc = function(n) {
    U = runif(n)
    X = sapply(U, function(u) which.max(F_trunc >= u) - 1)
    return(X)
}

samples1 = generate_poisson_inverse_trunc(n_samples)

#AR method_Poisson(20)
generate_poisson_rejection_p20 = function(n, lambda, k) {
    samples = numeric(n)
    count = 0
    total_trials = 0  
    
    aux_lambda = 20  
    
    c = max(dpois(0:k, lambda) / dpois(0:k, aux_lambda))
    
    while (count < n) {
        Y = rpois(1, aux_lambda) 
        total_trials = total_trials + 1  
        
        U = runif(1)  
        if (Y <= k && U < dpois(Y, lambda) / (c * dpois(Y, aux_lambda))) {  
            count = count + 1
            samples[count] = Y
        }
    }
    return(samples)
}

samples2 = generate_poisson_rejection_p20(n_samples, lambda, k)

#AR method_Poisson(15)
generate_poisson_rejection_p15 = function(n, lambda, k) {
    samples = numeric(n)
    count = 0
    total_trials = 0  
    
    aux_lambda = 15  
    
    c = max(dpois(0:k, lambda) / dpois(0:k, aux_lambda))
    
    while (count < n) {
        Y = rpois(1, aux_lambda) 
        total_trials = total_trials + 1  
        
        U = runif(1)  
        if (Y <= k && U < dpois(Y, lambda) / (c * dpois(Y, aux_lambda))) {  
            count = count + 1
            samples[count] = Y
        }
    }
    return(samples)
}

samples3 = generate_poisson_rejection_p15(n_samples, lambda, k)

#AR NB(20)
generate_poisson_rejection_nb20 = function(n, lambda, k) {
    samples = numeric(n)
    count = 0
    total_trials = 0  
    
    r = 20  
    p = r / (r + lambda)
    
    c = max(dpois(0:k, lambda) / dnbinom(0:k, size = r, prob = p))
    
    while (count < n) {
        Y = rnbinom(1, size = r, prob = p)  
        total_trials = total_trials + 1  
        
        U = runif(1)  
        if (Y <= k && U < dpois(Y, lambda) / (c * dnbinom(Y, size = r, prob = p))) {  
            count = count + 1
            samples[count] = Y
        }
    }
    return(samples)
}

samples4 = generate_poisson_rejection_nb20(n_samples, lambda, k)

E_X1 = mean(samples1)
Var_X1 = var(samples1)

E_X2 = mean(samples2)
Var_X2 = var(samples2)

E_X3 = mean(samples3)
Var_X3 = var(samples3)

E_X4 = mean(samples4)
Var_X4 = var(samples4)


true_E = sum((0:k) * P_trunc)
true_Var = sum((0:k)^2 * P_trunc) - true_E^2  

print(E_X1)
print(Var_X1)
print(E_X2)
print(Var_X2)
print(E_X3)
print(Var_X3)
print(E_X4)
print(Var_X4)
print(true_E)
print(true_Var)



x_max = k
breaks_range = seq(-0.5, x_max + 0.5, by = 1)


par(mfrow = c(2, 3))
barplot(P_trunc, names.arg = 0:k, col = "gray")

hist(samples1, breaks = breaks_range, probability = TRUE, col = "lightblue", main = "Inverse Transform Poisson")
hist(samples2, breaks = breaks_range, probability = TRUE, col = "lightgreen", main = "Poisson(20) AR")
hist(samples3, breaks = breaks_range, probability = TRUE, col = "red", main = "Poisson(15) AR")
hist(samples4, breaks = breaks_range, probability = TRUE, col = "purple", main = "Negative Binomial(20) AR")



generate_truncnorm_rejection = function(n, lambda, k) {
    samples = numeric(n)
    count = 0
    while (count < n) {
        Y = rnorm(1, mean=lambda, sd=sqrt(lambda))  # Proposal from Normal
        Y = round(Y)  # Round to nearest integer
        if (Y <= k && Y >= 0) {  # Accept only if within range
            count = count + 1
            samples[count] = Y
        }
    }
    return(samples)
}

samples_norm = generate_truncnorm_rejection(n_samples, lambda, k)

mean(samples_norm)
var(samples_norm)



#composition method
generate_composition_sample = function(n) {
    samples = numeric(n)
    
    for (i in 1:n) {
        U1 = runif(1)
        U2 = runif(1)
        
        if (U1 <= 0.5) {
            samples[i] = floor(10 * U2) + 1  
        } else {
            samples[i] = floor(5 * U2) + 6   
        }
    }
    
    return(samples)
}


samples = generate_composition_sample(10000)


table(samples) / length(samples)


#6-1
generate_composition_sample = function(n) {
    samples = numeric(n)
    
    for (i in 1:n) {
        U1 = runif(1)
        U2 = runif(1)
    
        if (U1 <= 0.55) {
            samples[i] = 5 + 2 * floor(5 * U2)
        } else {
            samples[i] = 6 + 2 * floor(5 * U2)
        }
    }
    
    return(samples)
}

n_samples = 10000
samples = generate_composition_sample(n_samples)


hist(samples, breaks=seq(4.5, 14.5, by=1), probability=TRUE, col="lightblue")

x_vals = c(5,7,9,11,13,6,8,10,12,14)
p_vals = c(rep(0.11, 5), rep(0.09, 5))
points(x_vals, p_vals, col="red", pch=19)  #theoretical point


#6-2
set.seed(42)

# ---------- (a) Pr(X = j) = (1/2)^j ----------
generate_pmf_a <- function(n) {
    samples <- numeric(n)
    for (i in 1:n) {
        U <- runif(1)
        p <- 1/2
        samples[i] <- ceiling(log(1 - U) / log(1 - p))  # Geo(p) + 1
    }
    return(samples)
}

# ---------- (b) Pr(X = j) = (1/2)*(2/3)^j ----------
generate_pmf_b <- function(n) {
    samples <- numeric(n)
    for (i in 1:n) {
        U <- runif(1)
        p <- 1 - 2/3  # = 1/3
        samples[i] <- ceiling(log(1 - U) / log(1 - p))  # Geo(p) + 1
    }
    return(samples)
}

# ---------- (c) Composition: (a)+(b)/2 ----------
generate_pmf_c <- function(n) {
    samples <- numeric(n)
    for (i in 1:n) {
        U1 <- runif(1)
        U2 <- runif(1)
        
        if (U1 <= 0.5) {
            # Use (a)
            p <- 1/2
        } else {
            # Use (b)
            p <- 1 - 2/3  # = 1/3
        }
        
        samples[i] <- ceiling(log(1 - U2) / log(1 - p))  # Unified style
    }
    return(samples)
}


n_samples = 10000

samples_a = generate_pmf_a(n_samples)
samples_b = generate_pmf_b(n_samples)
samples_c = generate_pmf_c(n_samples)

E_a = mean(samples_a)
Var_a = var(samples_a)
E_b = mean(samples_b)
Var_b= var(samples_b)
E_c = mean(samples_c)
Var_c = var(samples_c)

print(E_a)
print(Var_a)
print(E_b)
print(Var_b)
print(E_c)
print(Var_c)

