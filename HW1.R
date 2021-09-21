### 2.2
# a
A = matrix( c(-1,3,4,2), nrow= 2 , ncol= 2 , byrow=T  )
5*A
# b
B = matrix( c(4,-3,1,-2,-2,0), nrow = 3, byrow = T)
B
B%*%A
# c
t(A)
t(B)
t(A)%*%t(B)
# d
C = matrix( c(5, -4, 2), nrow = 3, byrow = F)
t(C)%*% B

### 2.3
# b
A = matrix( c(2,1,1,3), nrow = 2, byrow = T)
B = matrix( c(1,4,2,5,0,3), nrow = 2, byrow = T)
C = matrix( c(1,4,3,2), nrow = 2, byrow = T)
t(C)
solve(t(C))
solve(C)
t(solve(C))
# c
A %*% B
t(A %*% B )
t(B) %*% t(A)

### 2.7
A = matrix( c(9,-2,-2,6), nrow = 2, byrow = T)
# a
eigen(A)
# c
solve(A)
# d
inverse_A = solve(A)
eigen(inverse_A)$values
eigen(inverse_A)$vectors

### 2.20
A = matrix( c(2,1,1,3), nrow = 2, byrow = T)
A_sqrt = sqrt(A)  # Or: A ^ (1/2)
A_sqrt_inv = solve(sqrt(A))
A_sqrt %*% A_sqrt_inv
A_sqrt_inv %*% A_sqrt

### 2.21
A = matrix( c(1,1,2,-2,2,2), nrow = 3, byrow = T)
A
#a
A_trans = t(A)
B = A_trans %*% A
eigen(B)
#b
C = A %*% A_trans
eigen(C)
format(eigen(C)$values, scientific = F,digits = 0)

### 2.24
Sigma = matrix( c(4,0,0,0,9,0,0,0,1), nrow = 3, byrow = T)
Sigma
#a
Sigma_inv = solve(Sigma)
Sigma_inv
Sigma %*% Sigma_inv
#b
eigen(Sigma)
#c
eigen(Sigma_inv)

###2.25
#a
Sigma = matrix( c(25,-2,4,-2,4,1,4,1,9), nrow = 3, byrow = T)
Sigma
sqrt(diag(Sigma))
V_sqrt = matrix ( c(5,0,0,0,2,0,0,0,3), nrow = 3, byrow = T)
V_sqrt
Row = solve(V_sqrt) %*% Sigma %*% solve(V_sqrt)
Row
#b
V_sqrt %*% Row %*% V_sqrt

###2.41
#a
# By the variance-covariance matrix Sigma_x we can tell that all variables in vector X are independence with each other.
Mean_X = matrix( c(3,2,-2,0), nrow = 4, byrow = F)
A = matrix( c(1,-1,0,0,1,1,-2,0,1,1,1,-3), nrow = 3, byrow = T)
A %*% Mean_X
#b
## for covariance matrix for (AX)
Sigma_x = matrix( c(3,0,0,0,0,3,0,0,0,0,3,0,0,0,0,3), nrow = 4, byrow = T)
Cov_AX = A %*% Sigma_x %*% t(A)
## for variances of (AX)
diag(Cov_AX)
## According to the Cov_AX, all numbers are zero except for the diagonal, which means
## these three variables (AX1, AX2, AX3) are independent with each other.
