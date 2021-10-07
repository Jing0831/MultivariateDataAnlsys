a=6
a=9 #update...
b=7
a+b #Ctrl + Enter = Run
#---------------------------------------------------------------
a=6
a
c(8,2,3,4,5) #Conbine function --> vector

Myvector = c(7,8,4,3,5,1)
Myvector

c(7,8,1,3,2,1,4,5,6)
myMatrix = matrix( c(7,8,1,3,2,1,4,5,6), nrow= 3 , ncol= 3 , byrow=FALSE  )
# 3 by 3 matrix, byrow=F --> vertically,put the numbers by column!
myMatrix

A = matrix( c(7,8,1,3,2,1,4,5,6), nrow= 3 , ncol= 3 , byrow=FALSE  )
A

B = matrix( c(1,2,2,3,2,2,1,6,7), nrow= 3 , ncol= 3 , byrow=T  )
B
# Matrix doesn't have to be square!

B[2,1]# Using [] to locate/access the element
B[3,3]
B[3,3] = -8 #Using [] to change/resign the element
#############STOP HERE:9/2/2021 ############################################

A*B  # NOT MATRIX MULTIPLICATION --> Multiply elements in each position
A%*%B  # MATRIX MULTIPICATION

#Find Inverse

solve(A)

solve(A)%*%A   # Yields Identity matrix I 

#Transpose
t(A)

t(t(Myvector))  #Transpose of Transpose yields same item
# q = t(t(Myvector)), q will saved as a matrix

#Determinant 

det(A)

det (matrix ( c(2,1,1,2), nrow=2, ncol=2))

#TRACE OF MATRIX

matrix.trace(A)
# 'stack overflow' gives you every answers >.< ''
# package: matrixcalc  for .trace()
matrix.rank(A) # each row is independent
# EXAMPLE
C <- A%*%B
C


# MULTIPLY BY SCALAR
B
2*B
B/2
# COVARAINCE MATRIX
cov(A)
S <- cov(A)
S
# how to trans Covariance Matrix into Correlation Matrix
s = matrix(c(504,360,180,360,360,0,180,0,720),nrow = 3, ncol = 3, byrow=TRUE)
v = matrix(c(sqrt(504),0,0,0,sqrt(360),0,0,0,sqrt(720)),nrow = 3, ncol = 3, byrow = T)
v_inv = solve(v)
row = v_inv %*% s %*% v_inv

# CORRELATION MATRIX FROM COVARAINCE MATRIX

cov2cor(S) # same result as above, cov to cor

E<-eigen(s)

E$values # the value is from big to small
E$values[1]


