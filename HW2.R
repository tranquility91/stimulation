#(3)
#用inverse transform method，CDF是[0.3,0.5,0.85,1.0]
#根據 U~ uniform(0,1)落在哪個區間選擇對應X，嘗試10萬次
set.seed(2025)
generate_discrete_X = function(n) {
    U = runif(n)
    X = numeric(n)
    X[U <= 0.3] = 1
    X[U > 0.3 & U <= 0.5] = 2
    X[U > 0.5 & U <= 0.85] = 3
    X[U > 0.85] = 4
    return(X)
}

samples3 = generate_discrete_X(100000)
prob = table(samples3) / length(samples3) 
print(prob)



#(4)
#我猜期望值=1，變異數=0.995(因為有covariance這項)
simulate_hits = function(n_runs = 100000) {
    hit_counts = numeric(n_runs)
    
    for (i in 1:n_runs) {
        deck = sample(1:100)
        hits = sum(deck == 1:100)  
        hit_counts[i] = hits
    }
    
    return(hit_counts)
}

samples4 = simulate_hits()
mean(samples4)   
var(samples4)    

#(7)
simulate_dice = function() {
    observed = integer(11)
    count = 0
    
    while (any(observed == 0)) {
        roll = sample(1:6, 2, replace = TRUE)
        total = sum(roll)
        if (total >= 2 && total <= 12) {
            observed[total - 1] = 1
        }
        count = count + 1
    }
    
    return(count)
}
n_simulations = 100000
results = replicate(n_simulations, simulate_dice())
print(mean(results))
print(var(results))

