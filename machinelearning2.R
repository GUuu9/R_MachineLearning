
#LNOP11 - 20p

set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2) 
df1 <- data.frame(x1,x2,y)
View(df1)

y <- matrix(df1$y)
x <- cbind(1, as.matrix(df1[,1:2]))
#   행렬곱 : %*%
w <- solve(t(x) %*% x) %*% t(x) %*% y
w
lm(y ~ x1 + x2, data = df1)

y_hat <- x %*% w
r <- y - y_hat #잔차
sum(r) #잔차의 합
lm(y ~ x1 + x2, data = df1)$residuals # 잔차 / r과 동일한 계산




#set.seed(123)
#x <- runif(1000, -5, 5)
#y <- x + rnorm(1000) + 3

x <- c(1, 2.5, 3)
y <- c(1, 1.5, 3)

plot(x,y,pch=16,cex=1.5, col="red",
     xlab = "x",ylab="y")
reg1 <- lm(y ~ x)

# 비용함수
cost <- function(X, y, w) {
  sum( (X %*% w - y)^2 ) / (2*length(y))
}

# 벡터, 행렬로 표현
X <- cbind(1,matrix(x))
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.07
num_iters <- 1000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(0,0), nrow=2)
w
# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% w - y)
  delta <- t(X) %*% error
  w <- w - alpha * delta / length(y)
  cost_history[i] <- cost(X, y, w)
  w_history[[i]] <- w
  print(c(as.vector(w),cost(X, y, w)))
  Sys.sleep(0.02)
}


set.seed(123)
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

plot(x,y,pch=16,cex=1.5, col=rgb(0.2,0.4,0.6,0.4),
     xlab = "x",ylab="y")
reg1 <- lm(y ~ x)
abline(reg1,col="red",lwd=1.5)

# 비용함수
cost <- function(X, y, w) {
  sum( (X %*% w - y)^2 ) / (2*length(y))
}

# 벡터, 행렬로 표현
X <- cbind(1,matrix(x))
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.01
num_iters <- 1000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(0,0), nrow=2)

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% w - y)
  delta <- t(X) %*% error
  w <- w - alpha * delta / length(y)
  cost_history[i] <- cost(X, y, w)
  w_history[[i]] <- w
  print(c(as.vector(w),cost(X, y, w)))
  Sys.sleep(0.02)
}

# 경사하강법 시각화
plot(x,y,pch=16,cex=1.5, col=rgb(0.2,0.4,0.6,0.4),
     xlab = "x",ylab="y")
for (i in c(1,3,6,10,14, seq(20, num_iters, by=10))) {
  abline(coef=w_history[[i]],
         col=rgb(0.8,0,0,0.3))
}
abline(coef=w, col='blue', lwd=3)

# cost 변화
plot(cost_history, type='l', col='blue', lwd=2, 
     ylab='C(w)', xlab='Iterations')


set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(6:14, 25, replace = T)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 1.5*x2 + rnorm(25,0,1),2)
df1 <- data.frame(x1,x2,y)
View(df1)
reg2 <- lm(y ~ x1 + x2)

# 비용함수
cost <- function(X, y, w) {
  sum( (X %*% w - y)^2 ) / (2*length(y))
}

# 벡터, 행렬로 표현
X <- cbind(1,x1,x2)
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.004
num_iters <- 20000

# cost, w 저장
cost_history <- double(num_iters)
w_history <- list(num_iters)

# 초기화
w <- matrix(c(2,3,4), nrow=3)

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% w - y)
  delta <- t(X) %*% error
  w <- w - alpha * delta / length(y)
}
print(c(as.vector(w),cost(X, y, w)))
print(reg2)

# 로지스틱 회귀분석
set.seed(11)
x <- round(c(runif(20,0,6), runif(20,5,9)), 2)
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40

x <- df$x
y <- df$y
plot(x,y,pch=16,cex=2)
abline(h=0.5, col="gray")

f <- function(x) 1/(1+exp(-x)) # 로지스틱 함수
X <- cbind(1,matrix(x))
y <- matrix(y)
w1 <- 0
w0 <- 0
w <- matrix(c(w0,w1))
alpha <- 0.2

for(i in 1:100000){
  gradient <- 1/length(y) * t(X) %*% (matrix(apply(X %*% w,1,f))-y)
  w <- w - alpha*gradient
}
w

curve(1/(1+exp(-(w[1]+w[2]*x))),
      min(x),max(x),
      ylab = "y", add=T, lwd=2, col="red")

x_new <- 4.5
abline(v=x_new,lty=3,col="blue")
1/(1+exp(-(w[1]+w[2]*x_new)))
abline(h=1/(1+exp(-(w[1]+w[2]*x_new))),lty=3,col="blue")

# glm 함수 이용
lreg <- glm(y ~ x, family = "binomial")
w0 <- unname(lreg$coefficients[1])
w1 <- unname(lreg$coefficients[2])
w0
w1


#HW

library(dplyr)
library(magrittr)

state <- as.data.frame(state.x77)


x1 <- state$Population
x2 <- state$Income
x3 <- state$Illiteracy
x4 <- state$`Life Exp`
x5 <- state$Frost
y <- state$Murder
df <- data.frame(x1,x2,x3,x4,x5,y)
View(df)
reg2 <- lm(y ~ x1 + x2 + x3 + x4 + x5)

# 비용함수
cost <- function(X, y, w) {
  sum( (X %*% w - y)^2 ) / (2*length(y))
}

X <- cbind(1,x1,x2,x3,x4,x5)
y <- as.matrix(y)

# 학습률, 반복횟수
alpha <- 0.0000000406099
num_iters <- 20000

cost_history <- double(num_iters)
w_history <- list(num_iters)

w <- matrix(c(2,3,4,5,6,7), nrow=6)

for (i in 1:num_iters) {
  error <- (X %*% w - y)
  delta <- t(X) %*% error
  w <- w - alpha * delta / length(y)
}
print(c(as.vector(w),cost(X, y, w)))
print(reg2)
