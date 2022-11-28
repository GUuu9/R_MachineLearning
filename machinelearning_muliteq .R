install.packages("Deriv")
library("Deriv")
f <- function(X1,X2,X3) X1^4+2*X2^2+3*X3^2-4*X1-4*X2*X3
Deriv(Deriv(f,"X1"), "X2")

f <- function(X1, X2) X1^2+X2^2

vec <- c(Deriv(f,"X1"),Deriv(f,"X2"))

hm <- c(Deriv(Deriv(f, "x1"), "X1"), Deriv(Deriv(f,"X1"), "x2"),
        Deriv(Deriv(f, "x2"), "X1"), Deriv(Deriv(f,"X2"), "x2"))

vec
hm
