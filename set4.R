#Problem Set 4


#maximize X S.T. Y=(K)^b1(L)^b1 ; Kbar=Kx+ky ; Lbar=lk+ly


# Constrained Maximization

## X=(kx^a1 *lx^a2)
#max X
maxfnc = function(inputs){
  kx = inputs[1]
  lx = inputs[2]
  ret = ((kx)^a1 * (lx)^a2)
  return(signf*ret)
}

#constraints

fcnConst = function(inputs){
  kx = inputs[1]
  lx = inputs[2]
  ky = inputs[3]
  ly = inputs[4]

  const1 = (ybar - (ky)^b1 * (ly)^b2) #y constraint
  const2 = (kbar - kx - ky) ##kbar constraint
  const3 = (lbar - lx - ly) #lbar constraint
  return(c(const1,const2,const3))
  
}


#params

kbar = 10
lbar = 12
a1 = .2
a2 = .8
b1 = .25
b2 = .75


library("alabama")
stval = c(1,1,1,1)
signf = -1 # because 'auglag' is a minimization algorithm
solmax1 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
ybar = 1;ybar
solinput1 = solmax1$par
Opt1 = -1*solmax1$value;Opt1
#maxopt = -1*solmax$output;maxopt


## Ybar is fixed at 2

ybar = 2;ybar
solmax2 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
solinput2 = solmax2$par
Opt2 = -1*solmax2$value;Opt2


# ybar is 4

ybar = 4;ybar
solmax4 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
solinput4 = solmax4$par
Opt4 = -1*solmax4$value;Opt4


#ybar is 6

ybar = 6;ybar
solmax6 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
solinput6 = solmax6$par
Opt6 = -1*solmax6$value;Opt6

#ybar is 8

ybar = 8;ybar
solmax8 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
solinput8 = solmax8$par
Opt8 = -1*solmax8$value;Opt8

#ybar is 10

ybar = 10;ybar
solmax10 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
solinput10 = solmax10$par
Opt10 = -1*solmax10$value;Opt10


matrix(c(solinput1,
         solinput2,
         solinput4,
         solinput6,
         solinput8,
         solinput10), nrow = 6)

matrix(c(Opt1,Opt2,Opt4,Opt6,Opt8,Opt10,1,2,4,6,8,10),ncol = 2)


#maximize X S.T. Y=(K)^b1(L)^b1 ; Kbar=Kx+ky ; Lbar=lk+ly


# Constrained Maximization

## X=(kx^a1 *lx^a2)
#max X
maxfnc = function(inputs){
+   kx = inputs[1]
+   lx = inputs[2]
+   ret = ((kx)^a1 * (lx)^a2)
+   return(signf*ret)
+ }

#constraints

fcnConst = function(inputs){
+   kx = inputs[1]
+   lx = inputs[2]
+   ky = inputs[3]
+   ly = inputs[4]
+ 
+   const1 = (ybar - (ky)^b1 * (ly)^b2) #y constraint
+   const2 = (kbar - kx - ky) ##kbar constraint
+   const3 = (lbar - lx - ly) #lbar constraint
+   return(c(const1,const2,const3))
+   
+ }


#params

kbar = 10
lbar = 12
a1 = .2
a2 = .8
b1 = .25
b2 = .75


library("alabama")
stval = c(1,1,1,1)
signf = -1 # because 'auglag' is a minimization algorithm
solmax1 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.101533 
par:  1.15118 1.80573 7.87747 9.2674 
fval =   -1.65 
 
Outer iteration:  3 
Max(abs(heq)):  0.0003141186 
par:  1.04998 1.62327 8.94971 10.3769 
fval =   -1.488 
 
Outer iteration:  4 
Max(abs(heq)):  4.303465e-05 
par:  1.05017 1.62324 8.94987 10.3768 
fval =   -1.488 
 
Outer iteration:  5 
Max(abs(heq)):  5.061128e-05 
par:  1.05013 1.62324 8.94982 10.3767 
fval =   -1.488 
 
Outer iteration:  6 
Max(abs(heq)):  3.626764e-05 
par:  1.05014 1.62324 8.94983 10.3767 
fval =   -1.488 
 
Outer iteration:  7 
Max(abs(heq)):  2.116772e-06 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  8 
Max(abs(heq)):  8.934603e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  9 
Max(abs(heq)):  6.915591e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  10 
Max(abs(heq)):  5.230394e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  11 
Max(abs(heq)):  7.649704e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
ybar = 1;ybar
[1] 1
solinput1 = solmax1$par
Opt1 = -1*solmax1$value;Opt1
[1] 1.487847
#maxopt = -1*solmax$output;maxopt


## Ybar is fixed at 2

ybar = 2;ybar
[1] 2
solmax2 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.100418 
par:  8.08558 10.1894 0.938483 0.886977 
fval =   -9.729 
 
Outer iteration:  3 
Max(abs(heq)):  0.0001687814 
par:  7.91924 10.026 2.08074 1.97383 
fval =   -9.564 
 
Outer iteration:  4 
Max(abs(heq)):  0.0001702989 
par:  7.91946 10.0264 2.08071 1.97375 
fval =   -9.564 
 
Outer iteration:  5 
Max(abs(heq)):  1.089572e-05 
par:  7.91955 10.0261 2.08046 1.97387 
fval =   -9.564 
 
Outer iteration:  6 
Max(abs(heq)):  1.048133e-05 
par:  7.91954 10.0261 2.08045 1.97387 
fval =   -9.564 
 
Outer iteration:  7 
Max(abs(heq)):  6.840095e-06 
par:  7.91954 10.0261 2.08046 1.97387 
fval =   -9.564 
 
Outer iteration:  8 
Max(abs(heq)):  4.318332e-06 
par:  7.91954 10.0261 2.08046 1.97387 
fval =   -9.564 
 
Outer iteration:  9 
Max(abs(heq)):  4.529474e-07 
par:  7.91954 10.0261 2.08046 1.97388 
fval =   -9.564 
 
Outer iteration:  10 
Max(abs(heq)):  1.905478e-07 
par:  7.91954 10.0261 2.08046 1.97388 
fval =   -9.564 
 
solinput2 = solmax2$par
Opt2 = -1*solmax2$value;Opt2
[1] 9.56415


# ybar is 4

ybar = 4;ybar
[1] 4
solmax4 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.100725 
par:  6.12737 8.17576 2.89783 2.89976 
fval =   -7.718 
 
Outer iteration:  3 
Max(abs(heq)):  0.000116812 
par:  5.99709 8.00075 4.00285 3.99921 
fval =   -7.553 
 
Outer iteration:  4 
Max(abs(heq)):  0.0001057623 
par:  5.99714 8.00078 4.00286 3.99919 
fval =   -7.553 
 
Outer iteration:  5 
Max(abs(heq)):  3.136984e-05 
par:  5.99718 8.0009 4.00282 3.99907 
fval =   -7.553 
 
Outer iteration:  6 
Max(abs(heq)):  5.78757e-06 
par:  5.99719 8.00093 4.00281 3.99907 
fval =   -7.553 
 
Outer iteration:  7 
Max(abs(heq)):  5.165196e-06 
par:  5.99719 8.00093 4.00281 3.99907 
fval =   -7.553 
 
Outer iteration:  8 
Max(abs(heq)):  1.556623e-06 
par:  5.99719 8.00094 4.00281 3.99906 
fval =   -7.553 
 
Outer iteration:  9 
Max(abs(heq)):  1.300396e-06 
par:  5.99719 8.00094 4.00281 3.99906 
fval =   -7.553 
 
Outer iteration:  10 
Max(abs(heq)):  1.814417e-06 
par:  5.99719 8.00094 4.00281 3.99906 
fval =   -7.553 
 
Outer iteration:  11 
Max(abs(heq)):  1.260291e-07 
par:  6.00002 7.99999 3.99998 4.00001 
fval =   -7.553 
 
solinput4 = solmax4$par
Opt4 = -1*solmax4$value;Opt4
[1] 7.5527


#ybar is 6

ybar = 6;ybar
[1] 6
solmax6 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.100986 
par:  4.32938 6.1064 4.69695 4.96828 
fval =   -5.701 
 
Outer iteration:  3 
Max(abs(heq)):  0.0001372096 
par:  4.2207 5.9245 5.77916 6.07556 
fval =   -5.536 
 
Outer iteration:  4 
Max(abs(heq)):  2.605241e-05 
par:  4.22086 5.92454 5.77917 6.07549 
fval =   -5.536 
 
Outer iteration:  5 
Max(abs(heq)):  1.64055e-05 
par:  4.22085 5.92452 5.77915 6.07546 
fval =   -5.536 
 
Outer iteration:  6 
Max(abs(heq)):  1.383463e-05 
par:  4.22085 5.92453 5.77915 6.07548 
fval =   -5.536 
 
Outer iteration:  7 
Max(abs(heq)):  1.573611e-06 
par:  4.22085 5.92453 5.77915 6.07547 
fval =   -5.536 
 
Outer iteration:  8 
Max(abs(heq)):  1.776812e-06 
par:  4.22085 5.92453 5.77915 6.07547 
fval =   -5.536 
 
Outer iteration:  9 
Max(abs(heq)):  1.220811e-06 
par:  4.22085 5.92453 5.77915 6.07548 
fval =   -5.536 
 
Outer iteration:  10 
Max(abs(heq)):  9.584292e-07 
par:  4.22085 5.92452 5.77915 6.07548 
fval =   -5.536 
 
solinput6 = solmax6$par
Opt6 = -1*solmax6$value;Opt6
[1] 5.536085

#ybar is 8

ybar = 8;ybar
[1] 8
solmax8 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.10123 
par:  2.67538 3.98262 6.35211 7.09125 
fval =   -3.678 
 
Outer iteration:  3 
Max(abs(heq)):  0.0002176321 
par:  2.57694 3.79791 7.42285 8.20224 
fval =   -3.514 
 
Outer iteration:  4 
Max(abs(heq)):  1.373425e-05 
par:  2.57705 3.79785 7.42294 8.20216 
fval =   -3.514 
 
Outer iteration:  5 
Max(abs(heq)):  7.850546e-06 
par:  2.57706 3.79784 7.42294 8.20215 
fval =   -3.514 
 
Outer iteration:  6 
Max(abs(heq)):  5.400045e-06 
par:  2.57706 3.79785 7.42294 8.20216 
fval =   -3.514 
 
Outer iteration:  7 
Max(abs(heq)):  1.495087e-06 
par:  2.57706 3.79785 7.42294 8.20215 
fval =   -3.514 
 
Outer iteration:  8 
Max(abs(heq)):  9.097155e-07 
par:  2.57706 3.79785 7.42294 8.20215 
fval =   -3.514 
 
Outer iteration:  9 
Max(abs(heq)):  4.225851e-07 
par:  2.57706 3.79785 7.42294 8.20215 
fval =   -3.514 
 
Outer iteration:  10 
Max(abs(heq)):  4.778989e-07 
par:  2.57706 3.79785 7.42294 8.20215 
fval =   -3.514 
 
solinput8 = solmax8$par
Opt8 = -1*solmax8$value;Opt8
[1] 3.514428

#ybar is 10

ybar = 10;ybar
[1] 10
solmax10 = auglag(par = stval, fn = maxfnc, heq = fcnConst)
Max(abs(heq)):  10 
Outer iteration:  1 
Max(abs(heq)):  10 
par:  1 1 1 1 
fval =   -1 
 
Outer iteration:  2 
Max(abs(heq)):  1.101533 
par:  1.15118 1.80573 7.87747 9.2674 
fval =   -1.65 
 
Outer iteration:  3 
Max(abs(heq)):  0.0003141186 
par:  1.04998 1.62327 8.94971 10.3769 
fval =   -1.488 
 
Outer iteration:  4 
Max(abs(heq)):  4.303465e-05 
par:  1.05017 1.62324 8.94987 10.3768 
fval =   -1.488 
 
Outer iteration:  5 
Max(abs(heq)):  5.061128e-05 
par:  1.05013 1.62324 8.94982 10.3767 
fval =   -1.488 
 
Outer iteration:  6 
Max(abs(heq)):  3.626764e-05 
par:  1.05014 1.62324 8.94983 10.3767 
fval =   -1.488 
 
Outer iteration:  7 
Max(abs(heq)):  2.116772e-06 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  8 
Max(abs(heq)):  8.934603e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  9 
Max(abs(heq)):  6.915591e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  10 
Max(abs(heq)):  5.230394e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
Outer iteration:  11 
Max(abs(heq)):  7.649704e-07 
par:  1.05015 1.62325 8.94985 10.3768 
fval =   -1.488 
 
solinput10 = solmax10$par
Opt10 = -1*solmax10$value;Opt10
[1] 1.487847


matrix(c(solinput1,
+          solinput2,
+          solinput4,
+          solinput6,
+          solinput8,
+          solinput10), nrow = 6)
          [,1]     [,2]     [,3]      [,4]
[1,]  1.050044 2.080460 4.220851  7.422943
[2,]  1.623289 1.973878 5.924523  8.202154
[3,]  8.949956 6.000023 5.779149  1.050044
[4,] 10.376711 7.999992 6.075477  1.623289
[5,]  7.919540 3.999977 2.577057  8.949956
[6,] 10.026122 4.000008 3.797846 10.376711

matrix(c(Opt1,Opt2,Opt4,Opt6,Opt8,Opt10,1,2,4,6,8,10),ncol = 2)
         [,1] [,2]
[1,] 1.487847    1
[2,] 9.564150    2
[3,] 7.552700    4
[4,] 5.536085    6
[5,] 3.514428    8
[6,] 1.487847   10
