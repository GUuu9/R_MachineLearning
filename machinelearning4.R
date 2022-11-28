library(lpSolve)
library(magrittr)
library(dplyr)
###1

x1 <- c(5.22, 9.76, 2.13, 1.38, 7.80, 2.59, 3.80, 3.84, 3.63, 6.97, 6.80, 6.09, 6.43, 2.44, 1.07, 5.80, 5.93, 7.85, 5.97, 4.27)
x2 <- c(2.74, 19.88, 6.16, 12.83, 11.60, 3.79, 17.25, 16.81, 16.39, 4.89, 6.46, 16.19, 3.78, 15.38, 9.48, 5.51, 3.53, 13.76, 10.95, 4.81)
y <- c(25.43, 52.99, 26.00, 30.52, 41.76, 21.29, 38.91, 38.06, 38.72, 33.56, 32.88, 42.04, 30.06, 36.19, 26.97, 31.06, 27.66, 44.62, 37.22, 27.19)


plot(x1+sqrt(x2),y, pch = 16, col = 'blue')

dif <- NULL
for (i in 1:20){
  dif[i] <- y[i] - x1[i] - sqrt(x2[i])
}
dif #차이값
mean(dif)

df1 <- data.frame(x1,sqrt(x2),y)

y <- matrix(df1$y)
x <- cbind(1, as.matrix(df1[,1:2]))
#   행렬곱 : %*%
w <- solve(t(x) %*% x) %*% t(x) %*% y
w

dif <- NULL
for (i in 1:20){
  dif[i] <- y[i] - w[2] * x1[i] - w[3] * sqrt(x2[i]) - w[1]
}
dif #차이값
mean(dif)

plot(w[1]+x1*w[2]+sqrt(x2)*w[3],y, pch = 16, col = 'blue')


###2
# 목적식 fit[i]에 sin(i)의 제곱값 + cos(i)값의 합을 저장해둔다 200개
obj <- c(1:200)
for (i in 1:200)
  obj[i] <- sin(i)^2 + cos(i)

#이제 목적식에 곱할 x값들을 탐색한다.
# 1번 제약식
cons1 <- rep(0, 200 * 198)
for(i in 1:198)
{
  j <- (i - 1) * 201
  cons1[j+1] <- cons1[j+2] <- cons1[j+3] <- 1
}  

# 2번 제약식
x <- c(1:200)
primeNum <- c(2:200)
for (i in 2:200)
  primeNum <- setdiff(primeNum, x[x %% i == 0 & x %/% i != 1])

# 소수의 위치 표시
cons2 <- rep(0, 200)
for(i in primeNum)
  cons2[i] <- 1

# 3번 제약식
cons3 <- rep(0, 200 * 200)
for(i in 1:200)
{
  j <- (i - 1) * 201
  cons3[j+1] <- 1
}  

# 1번 제약조건 198개 소수 여부 1개 3번 제약조건 200개 => 399개
const <- matrix(c(cons1, cons2, cons3), 
            byrow=TRUE, nrow=399)

direction  <- c(rep("<=", 199), rep(">=", 200))

rhs <- c(rep(2.758, 198), 1.56, rep(0, 200))

sol <-  lp("max", obj, const, direction, rhs)

sol$solution


###3
library(TSP)
nw.tsp <- matrix(c(0	,208	,59	,16	,119	,170	,208	,47	,147	,117	,102	,108	,160	,79	,292	,138	,183	,237	,156,
                   0	,0	,170	,209	,109	,80	,1	,213	,116	,43	,115	,150	,72	,148	,77	,118	,38	,29	,60,
                   0	,0	,0	,50	,70	,120	,159	,99	,97	,151	,49	,117	,110	,84	,244	,88	,134	,187	,110,
                   0	,0	,0	,0	,106	,156	,197	,55	,136	,167	,88	,116	,149	,77	,282	,127	,173	,226	,146,
                   0	,0	,0	,0	,0	,170	,130	,158	,25	,66	,16	,171	,49	,139	,178	,143	,105	,117	,150,
                   0	,0	,0	,0	,0	,0	,83	,136	,165	,122	,142	,72	,109	,100	,170	,69	,59	,112	,30,
                   0	,0	,0	,0	,0	,0	,0	,213	,118	,43	,126	,149	,74	,147	,77	,123	,38	,33	,60,
                   0	,0	,0	,0	,0	,0	,0	,0	,186	,210	,138	,63	,199	,52	,302	,94	,187	,240	,158,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,75	,53	,199	,57	,166	,180	,171	,109	,125	,142,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,84	,189	,31	,197	,108	,143	,64	,54	,99,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,151	,56	,119	,199	,123	,89	,143	,134,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,176	,53	,237	,29	,123	,179	,96,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,143	,136	,127	,53	,82	,86,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,255	,31	,139	,197	,106,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,214	,122	,60	,148,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,114	,168	,81,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,66	,33,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,89,
                   0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0	,0
                   
),
byrow=T, ncol=19)
nw.tsp <- nw.tsp + t(nw.tsp)
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(1:19)                                                     
tsp <- TSP(nw.tsp) 
n_of_cities(tsp) 
labels(tsp)
tour <- solve_TSP(tsp) 
names(tour)
tour # 길이


###4
x1 <- c(9,12,17,25,7,23,27,16,15,3,5,20,13,8,18,28,11,24,6,30,10,21,29,2,14)
x2 <- c(7,12,11,7,14,14,7,13,10,10,10,8,12,7,9,13,14,8,10,6,11,9,13,7,9)
y <- c(26.42,39.44,46.57,52.09,34.85,60.54,56.56,48.81,40.36,23.46,24.68,45.53,40.63,26.02,45.08,66.78,40.16,52.47,28.26,60.02,34.42,48.83,68.50,14.41,37.98)

df <- data.frame(x1,x2,y)

X <- cbind(1, df$x1, df$x2) %>% as.matrix(df[,1:2])
y <- df$y %>% as.matrix(df[,1:2])

# 학습률, 반복횟수
alpha <- 0.7
num_iters <- 5000

h <- NULL

#w와 h값 초기화
w <- matrix(c(0,0,0), nrow=3)
h <- matrix(c(0,0,0), nrow=3)
# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% w - y)
  grad <- t(X) %*% error
  h <- h + grad^2
  w <- w - alpha * (1/sqrt(h)) * grad / length(y)
}

w

# 계산된 w값으로 y값 확인
yy <- NULL
for (i in 1:25){
  yy[i] <- w[1] + w[2] * x1[i] + w[3] * x2[i]
}
yy <- yy %>% as.matrix(yy)


###5
###LINDO 로 계산함.
max 20x1+50x2
st
1.2x1+4x2<=240
0.5x1+x2<=81
x2<=40
x1>=0
x2>=0
end
gin x1
gin x2

결과는 다음과 같다

LP OPTIMUM FOUND AT STEP      3
OBJECTIVE VALUE =   3525.00000

SET       X2 TO <=    28 AT    1, BND=   3520.     TWIN=  3517.         11

NEW INTEGER SOLUTION OF    3520.00000     AT BRANCH      1 PIVOT      11
BOUND ON OPTIMUM:  3520.000
DELETE       X2 AT LEVEL     1
ENUMERATION COMPLETE. BRANCHES=     1 PIVOTS=      11

LAST INTEGER SOLUTION IS THE BEST FOUND
RE-INSTALLING BEST SOLUTION...

OBJECTIVE FUNCTION VALUE

1)      3520.000

VARIABLE        VALUE          REDUCED COST
X1       106.000000        -20.000000
X2        28.000000        -50.000000

NO. ITERATIONS=      11
BRANCHES=    1 DETERM.=  1.000E    0



###6
# Forward propagation computation 역전파 알고리즘
x <- matrix(c(2,9)) ## x1, x2값들
y <- 0.82           ## Y 값

f1 <- function(z) 1/(1+exp(-z))
f2 <- function(z) tanh(z)

a.0 <- x            ## 입력층
b.1 <- matrix(c(0.1, 0.5))
b.2 <- matrix(c(3.9))
W.1 <- matrix(c(3, 0.7,
                0.5, 2), byrow = T, ncol=2)
W.2 <- matrix(c(1.2, -7), byrow = T, ncol=2)

df <- data.frame(t(rep(NA,10))) # 역행렬 구성
names(df) <- c("b1.1", "b2.1",
               "w11.1", "w12.1","w21.1","w22.1",
               "b2.1",
               "w11.2","w12.2",
               "y.hat")

for(i in 1:31) { #31회 갱신된 후 y.hat의 출력값을 계산한다.
  a.1 <- f2(b.1 + W.1 %*% a.0)
  a.2 <- as.vector(f1(b.2 + W.2 %*% a.1))
  y.hat <- a.2
  df[i,] <- c(b.1[1,],b.1[2,],
              W.1[1,1],W.1[1,2],W.1[2,1],W.1[2,2],
              b.2[1,],
              W.2[1,1],W.2[1,2],
              y.hat)
  
  # Back propagation computation
  alpha <- 0.37                  # 학습률은 0.37이다
  delta.2 <- a.2 - y
  
  gr.2 <- delta.2*a.2*(1-a.2) * rbind(1,a.1)
  
  b.2 <- b.2 - alpha*gr.2[1]      # b.2 update
  W.2 <- W.2 - alpha*gr.2[-1]     # W.2 update
  
  delta.1 <- delta.2 * a.2 * (1-a.2) * W.2 
  
  gr.1 <- (t(delta.1)*a.1*(1-a.1)) %*% cbind(1,t(a.0))
  
  b.1 <- b.1 - alpha*gr.1[,1]      # b.1 update
  W.1 <- W.1 - alpha*gr.1[,-1]     # W.1 update
}
a.1 <- f2(b.1 + W.1 %*% a.0)
y.hat <- a.2 <- as.vector(f1(b.2 + W.2 %*% a.1))
df <- rbind.data.frame(df, c(b.1[1,],b.1[2,], 
                             W.1[1,1],W.1[1,2],W.1[2,1],W.1[2,2],
                             b.2[1,],
                             W.2[1,1],W.2[1,2],
                             y.hat))
View(df)

# 역전파 알고리즘 수정과정 직접 계산 1화
delta12 <- 0.1301085 - y

aa11 <- f1(0.1 + 3 * 2 + 0.7 * 9)
aa12 <- f1(0.5 + 0.5 * 2 + 2 * 9)

delta12;aa11; aa12;

bb12 <- delta1.2 * 0.1301085 * ( 1 - 0.1301085) * 1
ww112 <- delta1.2 * 0.1301085 * ( 1 - 0.1301085) * aa11
ww122 <- delta1.2 * 0.1301085 * ( 1 - 0.1301085) * aa12
bb12;ww112;ww122;

bb12 <- 3.9 - alpha * bb12
ww112<- 1.2 - alpha * ww112
ww122<- -7 - alpha * ww122
# 수정된 역전파 값
bb12; ww112; ww122;

#이후의 값들은 활성화 함수 값을계산할 경우 0에 한없이 가까운 값이기 때문에
#값이 변하지 않는다.
daa11 <- delta1.2 * 0.1301085 * ( 1 - 0.1301085) * ww112
daa21 <- delta1.2 * 0.1301085 * ( 1 - 0.1301085) * ww122
daa11;daa21

dww211 <- daa21 * aa12 * (1 - aa12) * 7
dww211
