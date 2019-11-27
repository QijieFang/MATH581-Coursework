source("ws2.R")

index_beta <- which(ws2>=0)

index_gamma <- which(ws2<0)

beta <- 1/mean(ws2[index_beta])

gamma <- -1/mean(ws2[index_gamma])

m <- length(index_beta)

n <- length(ws2)

CI_beta_lower <- beta - 1.96 * (beta ^ 2 / m) ^ (-1/2)

CI_beta_upper <- beta + 1.96 * (beta ^ 2 / m) ^ (-1/2)

CI_gamma_lower <- gamma - 1.96 * (gamma ^ 2 / (n - m)) ^ (-1/2)

CI_gamma_upper <- gamma + 1.96 * (gamma ^ 2 / (n - m)) ^ (-1/2)

f_beta <- 1 / 2 * beta * exp(-beta * ws2[index_beta])

f_gamma <- 1 / 2 * gamma * exp(beta * ws2[index_gamma])

plot(range(ws2),range(c(0, max(f_beta,f_gamma))), type = "n")

points(ws2[index_beta], f_beta)

points(ws2[index_gamma], f_gamma)

W <- c(rexp(m,beta),-rexp(n-m,gamma))

qqplot(ws2, W)

lines(c(-10,10),c(-10,10), col = "red")