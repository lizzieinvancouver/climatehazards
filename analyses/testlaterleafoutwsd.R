## Started 22 June 2023 ##
## By Lizzie ##

## Testing if higher SD has to mean earlier leafout ##
## Because it's a threshold, maybe you always get there sooner? ##
## Copied some code from biaglag.R from J Auerbach ##

n_sim <- 1e4
thermal_sum <- 2000
lag <- 70

## Testing if higher SD means earlier budburst ....

resultsSDlow <- data.frame(bday = numeric(1e4),
                      A = numeric(1e4),
                      B = numeric(1e4),
                      X = numeric(1e4))

for(sim in 1:n_sim) {
  temp <- rnorm(120, 100 + 10 * (1:120), 50) / 20 #simulated temperature
  bday <- resultsSDlow$bday[sim] <- which.max(cumsum(temp) > thermal_sum)  #bloom date
  resultsSDlow$A[sim] <- mean(temp) #average temperature
  resultsSDlow$B[sim] <- mean(temp[(bday - lag):bday]) #average temperature over window
  resultsSDlow$X[sim] <- mean(temp[1:bday]) #average temperature until bloom

}

resultsSDhigh <- data.frame(bday = numeric(1e4),
                      A = numeric(1e4),
                      B = numeric(1e4),
                      X = numeric(1e4))

for(sim in 1:n_sim) {
  temp <- rnorm(120, 100 + 10 * (1:120), 100) / 20 #simulated temperature
  bday <- resultsSDhigh$bday[sim] <- which.max(cumsum(temp) > thermal_sum)  #bloom date
  resultsSDhigh$A[sim] <- mean(temp) #average temperature
  resultsSDhigh$B[sim] <- mean(temp[(bday - lag):bday]) #average temperature over window
  resultsSDhigh$X[sim] <- mean(temp[1:bday]) #average temperature until bloom

}

resultsSDhigher <- data.frame(bday = numeric(1e4),
                      A = numeric(1e4),
                      B = numeric(1e4),
                      X = numeric(1e4))

for(sim in 1:n_sim) {
  temp <- rnorm(120, 100 + 10 * (1:120), 150) / 20 #simulated temperature
  bday <- resultsSDhigher$bday[sim] <- which.max(cumsum(temp) > thermal_sum)  #bloom date
  resultsSDhigher$A[sim] <- mean(temp) #average temperature
  resultsSDhigher$B[sim] <- mean(temp[(bday - lag):bday]) #average temperature over window
  resultsSDhigher$X[sim] <- mean(temp[1:bday]) #average temperature until bloom

}

par(mfrow=c(1,3))
hist(resultsSDlow$bday, main="SD at 50")
hist(resultsSDhigh$bday,  main="SD at 100")
hist(resultsSDhigher$bday,  main="SD at 150")

mean(resultsSDlow$bday)
mean(resultsSDhigh$bday)
mean(resultsSDhigher$bday)
