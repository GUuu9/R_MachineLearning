#AND
perceptron <- function(x1, x2, w1, w2, b) { if(w1*x1 + w2*x2 + b <= 0) {
  return(0) } else {
    return(1) }
}
AND <- function(x1, x2) { return(perceptron(x1, x2, 0.5, 0.5, -0.7))
}
AND(0,0); AND(0,1); AND(1,0); AND(1,1)

#NAND
perceptron <- function(x1, x2, w1, w2, b) { if(w1*x1 + w2*x2 + b <= 0) {
  return(0) } else {
    return(1) }
}
NAND <- function(x1, x2) { return(perceptron(x1, x2, -0.5, -0.5, 0.7))
}
NAND(0,0); NAND(0,1); NAND(1,0); NAND(1,1)

#OR
perceptron <- function(x1, x2, w1, w2, b) { if(w1*x1 + w2*x2 + b <= 0) {
  return(0) } else {
    return(1) }
}
OR <- function(x1, x2) { return(perceptron(x1, x2, 0.5, 0.5, -0.2))
}
OR(0,0); OR(0,1); OR(1,0); OR(1,1)

#NOR
perceptron <- function(x1, x2, w1, w2, b) { if(w1*x1 + w2*x2 + b <= 0) {
  return(0) } else {
    return(1) }
}
NOR <- function(x1, x2) { return(perceptron(x1, x2, -0.5, -0.5, 0.2))
}
NOR(0,0); NOR(0,1); NOR(1,0); NOR(1,1)


## 13 - 25p hint
f1 <- f2 <- function(z) 1/(1+exp(-z))
w.1 <- matrix(c(0.3,0.7,
                0.5,0.4), byrow = T, ncol = 2)


w.2 <- matrix(c(1.2,2.6), byrow = T, ncol = 2)
b.1 <- matrix(c(0.1,
                0.5), byrow = T, ncol = 1)

b.2 <- matrix(3.9)

x <- a.0 <- matrix(c(7,
                     8), byrow = T, ncol = 1)


a.1 <- f1(b.1+w.1 %*% a.0)
y.hat <- a.2 <- f2(b.2+w.2 %*% a.1) 
y.hat

## 13 - 24p HW

f1 <- function(z) 1/(1+exp(-z))
f2 <- function(z) tanh(z)
f3 <- function(z) max(0.1*z ,z)

w.1 <- w.2 <- matrix(c(0.6,0.6,0.6,0.6,
                       0.6,0.6,0.6,0.6,
                       0.6,0.6,0.6,0.6,
                       0.6,0.6,0.6,0.6), byrow = T, ncol = 4)

w.3 <- matrix(c(2.7,2.7,2.7,2.7), byrow = T, ncol = 4)

b.1 <- b.2 <- matrix(c(1.2,
                       1.2,
                       1.2,
                       1.2), byrow = T, ncol = 1)

b.3 <- matrix(1.2)


x <- a.0 <- matrix(c(1.2,
                     5.9,
                     2.3,
                     0.2), byrow = T, ncol = 1)


a.1 <- f1(b.1+w.1 %*% a.0)
a.2 <- f2(b.2+w.2 %*% a.1)
y.hat <- a.3 <- f3(b.3+w.3 %*% a.2) 
y.hat
a.1
a.2

# Forward propagation computation 역전파 알고리즘
x <- matrix(c(7,8))
y <- 0.4

f <- function(z) 1/(1+exp(-z))

a.0 <- x
b.1 <- matrix(c(0.1, 0.5))
b.2 <- matrix(c(0.2))
W.1 <- matrix(c(0.3, 0.07,
                0.5, 0.4), byrow = T, ncol=2)
W.2 <- matrix(c(0.2, 0.6), byrow = T, ncol=2)

df <- data.frame(t(rep(NA,10)))
names(df) <- c("b1.1", "b2.1",
               "w11.1", "w12.1","w21.1","w22.1",
               "b2.1",
               "w11.2","w12.2",
               "y.hat")

for(i in 1:500) {
  a.1 <- f(b.1 + W.1 %*% a.0)
  a.2 <- as.vector(f(b.2 + W.2 %*% a.1))
  y.hat <- a.2
  df[i,] <- c(b.1[1,],b.1[2,],
              W.1[1,1],W.1[1,2],W.1[2,1],W.1[2,2],
              b.2[1,],
              W.2[1,1],W.2[1,2],
              y.hat)
  
  # Back propagation computation
  alpha <- 0.3
  delta.2 <- a.2 - y
  
  gr.2 <- delta.2*a.2*(1-a.2) * rbind(1,a.1)
  
  b.2 <- b.2 - alpha*gr.2[1]      # b.2 update
  W.2 <- W.2 - alpha*gr.2[-1]     # W.2 update
  
  delta.1 <- delta.2 * a.2 * (1-a.2) * W.2 
  
  gr.1 <- (t(delta.1)*a.1*(1-a.1)) %*% cbind(1,t(a.0))
  
  b.1 <- b.1 - alpha*gr.1[,1]      # b.1 update
  W.1 <- W.1 - alpha*gr.1[,-1]     # W.1 update
}
a.1 <- f(b.1 + W.1 %*% a.0)
y.hat <- a.2 <- as.vector(f(b.2 + W.2 %*% a.1))
df <- rbind.data.frame(df, c(b.1[1,],b.1[2,], 
                             W.1[1,1],W.1[1,2],W.1[2,1],W.1[2,2],
                             b.2[1,],
                             W.2[1,1],W.2[1,2],
                             y.hat))
View(df)
