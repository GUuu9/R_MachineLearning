f <- function(x1, x2) -x1^2-x2^2+2*x1-x1*x2-1
library(Deriv)
Deriv(f,"x1")
Deriv(f,"x2")
A <- matrix(c(2,1,
            1,2),byrow = T, ncol = 2)
b <- c(2,0)
solve(A,b)
Deriv(f,"x1",nderiv = 2) #편미분
Deriv(f,"x2",nderiv = 2)
Deriv(Deriv(f,"x1"),"x2")

f <- function(x1, x2, x3) x1^4+2*x2^2+3*x3^2-4*x1-4*x2*x3
Deriv(f,"x1", nderiv = 2)
Deriv(f,"x2", nderiv = 2)
Deriv(f,"x3", nderiv = 2)
A <- matrix(c(12,0,0,
              0,4,-4,
              0,-4,6), byrow = T, ncol = 3)


# 경사 하강법
f <- function(x) x^2-10*x+26
x <- 7
alpha <- 0.01 # 알파값에 따라 값을 구하는 정도가 달라짐

library(Deriv)
gradient <- Deriv(f,"x") # 1차 평미분

for(i in 1:100){
  x <- x - alpha*gradient(x)
  print(c(x, f(x)))
  Sys.sleep(0.1)
}

# 변수가 2개 이상인 경우
f <- function(x1, x2) x1^2+x2^2-2*x1+x1*x2+1
x <- c(5,5)
alpha <- 0.1

f1.prime <- Deriv(f,"x1") # 1차 편미분 x1기준
f2.prime <- Deriv(f,"x2")


for(i in 1:100){
  gradient <- c(f1.prime(x[1],x[2]),
                f2.prime(x[1],x[2]))
  x <- x - alpha*gradient
  print(c(x, f(x[1],x[2])))
  Sys.sleep(0.1)
}

#숙제
f <- function(x1, x2, x3) (x1-4)^2+x3^2*x1+(x2+1)^2+6
x <- c(4, -1, 0)
alpha <- 0.1 # 계속 수정하며 찾을 것

f1.prime <- Deriv(f,"x1") # 1차 편미분 x1기준
f2.prime <- Deriv(f,"x2")
f3.prime <- Deriv(f,"x3")

for(i in 1:100){
  gradient <- c(f1.prime(x[1],x[2],x[3]),
                f2.prime(x[1],x[2],x[3]),
                f3.prime(x[1],x[2],x[3]))
  x <- x - alpha*gradient
  print(c(x, f(x[1],x[2],x[3])))
  Sys.sleep(0.1)
}


# 11장 15p
x <- 1:10
y <- c(3,3,3,6,6,9,9,9,10,11)
plot(x,y)
cov(x,y) # 공분산

x_bar <- mean(x)
y_bar <- mean(y)

w1 <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
w0 <- y_bar-w1*x_bar

abline(w0, w1,col='red',lwd=3)

w0+w1*11 # 마지막 값은 구하려는 값
w1 == cov(x,y)/var(x)

lm(y ~ x)


x <- c(36.5, 28, 42.9, 52, 51.5, 53.8, 25.4, 37.2, 50.9, 29.2)
y <- c(14, 9, 15, 20, 21, 25, 9, 13, 20, 10)

plot(x,y)
cov(x, y)

x_bar <- mean(x)
y_bar <- mean(y)

w1 <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
w0 <- y_bar-w1*x_bar

abline(w0, w1,col='red',lwd=3)

#reg1 <- lm(y ~ x)
#abline(reg1, col="red)
#w0 <- reg1$coefficients[1]
#w1 <- reg1$coefficients[2]

(17 - w0)/w1
w1 == cov(x,y)/var(x)


reg1$residuals
sum(reg1$residuals)
