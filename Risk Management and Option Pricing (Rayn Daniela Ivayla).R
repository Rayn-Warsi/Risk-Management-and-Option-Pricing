rm(list=ls())
mu <- 0.1
r <- 0.03 # compounded yearly
sigma <- 0.2
# all values stated above are early
#stock price currently at 100
# call option price 110

amount_of_days <- 184 #183 + 1 as r is indexed
S_0 <- 100 
strike_price <- 110
monte_carlo_iterations <- 10000

generate_stock_prices <- function() {
    S <- rep(0,amount_of_days)
  S[1] <- S_0
  
  for (t in 2:amount_of_days) {
    delta_t <- 1/365
    S[t] <- S[t-1]* exp((mu - 0.5*sigma^2)*delta_t + sigma*sqrt(delta_t)*rnorm(1))
  }
  return(S)
}

#a)

# monte carlo loop
in_the_money <- rep(F, monte_carlo_iterations)

for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  
  if (S[amount_of_days]>strike_price) {
    in_the_money[i] = T
  }
  
}
plot(S)
mean(in_the_money)

#b)
#alice
alice_profit <- rep(0,monte_carlo_iterations)
alice_price <- 3450

# Alice monte carlo loop 
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  balance <- alice_price
  new_balance <- balance*exp(r*(184/365)) 
  pay <- max(100*(S[184]-strike_price),0)
  final_balance <- new_balance - pay
  alice_profit[i] <- final_balance
}
probability_alice_profit <- mean(alice_profit > 0)

#Bradley part
bradley_profit <- rep(0,monte_carlo_iterations)
bradley_price <- 1650

#Bradley monte carlo loop
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  bradley_balance <- bradley_price 
  hedged <- FALSE
  for (t in 1:amount_of_days) { 
    if (S[t]>strike_price && hedged == FALSE) {
      bradley_balance <- bradley_balance - (S[t]*100) #buy 100 shares (bradley balance can be negative)
      hedged <- TRUE
    } else if (S[t] <= strike_price && hedged == TRUE) {
      bradley_balance <- bradley_balance + (S[t]*100) #sell 100 shares 
      hedged <- FALSE
    }
    bradley_balance <- bradley_balance * exp(r*(1/365))
  }
  #bradley profit
  if(S[amount_of_days]>=strike_price) {
    bradley_balance<- bradley_balance +strike_price*100
  }
  bradley_profit[i] <- bradley_balance
}
mean(bradley_profit>0) #since it is still distributed Bernoulii

#Claire part
claire_profit <- rep(0,monte_carlo_iterations)
claire_price <- 400

calculate_delta <- function(S_t, t, strike_price=110, r=0.03, sigma=0.2, tau=184/365) {
  t=t/365
  d1 <- (log(S_t / strike_price) + (r + 0.5 * sigma^2) * (tau-t)) / (sigma * sqrt(tau-t))
  return(pnorm(d1))
}

#Claire Monte Carlo 
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  claire_balance <- claire_price 
  shares <- 0
  
  for (t in 1:amount_of_days) {
    desired_shares <- calculate_delta(S[t], t) * 100
    if(desired_shares> shares) {
      diff <- desired_shares - shares
      claire_balance <- claire_balance - S[t]*diff #buy diff 
      shares <- shares+diff
    } else if (desired_shares< shares) {
      diff <- shares - desired_shares
      claire_balance<- claire_balance + S[t]*diff #sell diff
      shares <- shares-diff
      }
    claire_balance <- claire_balance * exp(r*(1/365))
  }
  # claire profit
  if(S[amount_of_days]>=strike_price) {
    stopifnot(shares==100)
    claire_balance<- claire_balance + strike_price*100
  }
  claire_profit[i] <- claire_balance
}


mean(claire_profit>0) #since it is still distributed Bernoulii

hist(alice_profit, probability= TRUE)
hist(bradley_profit, probability= TRUE)
hist(claire_profit, probability= TRUE)

#d)
#Utility function
utility <- function(x, a=0.1) {
  if(a == 0) {
    return(x)
  }
  return(1/a * (1-exp(-a*x)))
}

mean_expectation <- function(pnl,P, a = 0.1) {
  utilities <- utility(exp(r*amount_of_days/365)*P+pnl, a)
  return(mean(utilities))
}


#Alice Monte Carlo loop for profit and loss
alice_pnl <- rep(0, amount_of_days)
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  balance <- alice_price
  new_balance <- balance*exp(r*(184/365)) 
  pay <- max(100*(S[184]-strike_price),0)
  final_balance <- new_balance - pay
  alice_profit[i] <- final_balance
  alice_pnl[i] <- -1*pay
  }

#Bradley Monte Carlo loop for profit and loss
bradley_pnl <- rep(0, amount_of_days)
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  bradley_balance <- bradley_price 
  hedged <- FALSE
  bradley_pnl_sim <- 0 #the profit and loss accumulated in 1 simulation 
  for (t in 1:amount_of_days) { 
    if (S[t]>strike_price && hedged == FALSE) {
      bradley_balance <- bradley_balance - (S[t]*100) #buy 100 shares (bradley balance can be negative)
      hedged <- TRUE
      bradley_pnl_sim <- bradley_pnl_sim - (S[t]*100)
    } else if (S[t] <= strike_price && hedged == TRUE) {
      bradley_balance <- bradley_balance + (S[t]*100) #sell 100 shares 
      hedged <- FALSE
      bradley_pnl_sim <- bradley_pnl_sim + (S[t]*100)
    }
    bradley_balance <- bradley_balance * exp(r*(1/365))
  }
  if (S[amount_of_days]>strike_price) {
     bradley_pnl_sim <- bradley_pnl_sim + (100*strike_price)
  }
  bradley_pnl[i] <- bradley_pnl_sim
}

#Claire Monte Carlo for profit and loss
claire_pnl <- rep(0, amount_of_days)
for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  claire_balance <- claire_price 
  shares <- 0

claire_pnl_sim <- 0
  for (t in 1:amount_of_days) {
    desired_shares <- calculate_delta(S[t], t) * 100
    if(desired_shares> shares) {
      diff <- desired_shares - shares
      claire_balance <- claire_balance - S[t]*diff #buy diff 
      shares <- shares+diff
      claire_pnl_sim <- claire_pnl_sim - (S[t]*diff)
    } else if (desired_shares< shares) {
      diff <- shares - desired_shares
      claire_balance<- claire_balance + S[t]*diff #sell diff
      shares <- shares-diff
      claire_pnl_sim <- claire_pnl_sim + (S[t]*diff)
      }
    claire_balance <- claire_balance * exp(r*(1/365))
  } 
if (S[amount_of_days]>strike_price) {
  claire_pnl_sim <- claire_pnl_sim + (100*strike_price)
}
claire_pnl[i] <- claire_pnl_sim
}

alice_indiff <- uniroot(mean_expectation, interval = c(100, 10000), pnl = alice_pnl)$root
bradley_indiff <- uniroot(mean_expectation, interval = c(100, 10000), pnl = bradley_pnl)$root
claire_indiff <- uniroot(mean_expectation, interval = c(100, 10000), pnl = claire_pnl)$root

hist(alice_pnl)
hist(bradley_pnl)
hist(claire_pnl)

#e)
#P(A|B)=P(A and B)/P(B)
#P(A|B)=(P(B|A)*P(A))/P(B)
#Defining events
#A is falling below 90 before maturity
#B is falling in the money at maturity


down_and_out_and_in_the_money <- rep(F, monte_carlo_iterations)
in_the_money <- rep(F, monte_carlo_iterations)

for (i in 1:monte_carlo_iterations) {
  S <- generate_stock_prices()
  down_and_out_flag <- F
  in_the_money_flag <- F
  
  if (S[amount_of_days]>strike_price) {
    in_the_money_flag <- T
  }
  
  for (t in 1:183) {
    if (S[t] < 90) {
      down_and_out_flag <- T
    }
  }
  
  if (down_and_out_flag == TRUE && in_the_money_flag == TRUE) {
    down_and_out_and_in_the_money[i] <- T
  }
  if(in_the_money_flag == TRUE) {
    in_the_money[i] = TRUE
  }
  
}

P_A_and_B<-mean(down_and_out_and_in_the_money)
P_B <- mean(in_the_money)

#Using P(A|B)=P(A and B)/P(B)
P_A_given_B <- P_A_and_B/P_B