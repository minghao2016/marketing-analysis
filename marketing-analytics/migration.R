P0 <- matrix(0, nrow = 4, ncol = 4)
P0[1,1] <- 1

P <- matrix(c(.5, .5, 0, 0, .2, 0, .8, 0, .1, 0, 0, .9, 0, 0, 0, 1), nrow = 4, byrow = TRUE)
G <- matrix(c(100, 0, 0, 0), nrow = 4)

P0 %*% G
P %*% G
P %*% P %*% G






