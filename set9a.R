# Country H --------------------------------------------------------------

fcnU = function(input){
+ XDH = input[1]
+ YDH = input[2]
+ ret = (XDH^v)*(YDH^Omega)
+ return(signf*ret) # signf=-1
+ }
# B con
fcnBC = function(input){
+ # the constraint 
+ XDH = input[1]
+ YDH = input[2]
+ Homeppf = M-(5*XDH)-YDH
+ return(c(Homeppf)) 
+ }
# 
fcnNonNeg = function(input){
+ # the constraint 
+   h = input
+   return(h)
+ }
# parameters 
v = 1/3 
Omega  = 2/3 
PX =1
M=500
a=5
library("alabama") # load "alabama" optimization package
stval = c(3,4)
signf = -1 # 
solU = auglag(par = stval,  fn = fcnU, heq = fcnBC, hin = fcnNonNeg)
Min(hin):  3 Max(abs(heq)):  481 
Outer iteration:  1 
Min(hin):  3 Max(abs(heq)):  481 
par:  3 4 
fval =   -3.634 
 
Outer iteration:  2 
Min(hin):  43.79387 Max(abs(heq)):  46.2561 
par:  43.7939 234.775 
fval =   -134.1 
 
Outer iteration:  3 
Min(hin):  33.57543 Max(abs(heq)):  0.3972225 
par:  33.5754 331.726 
fval =   -154.6 
 
Outer iteration:  4 
Min(hin):  33.42958 Max(abs(heq)):  0.03746197 
par:  33.4296 332.89 
fval =   -154.7 
 
Outer iteration:  5 
Min(hin):  33.41237 Max(abs(heq)):  0.002178417 
par:  33.4124 332.94 
fval =   -154.7 
 
Outer iteration:  6 
Min(hin):  33.41162 Max(abs(heq)):  0.001502953 
par:  33.4116 332.94 
fval =   -154.7 
 
Outer iteration:  7 
Min(hin):  33.41173 Max(abs(heq)):  0.0009051663 
par:  33.4117 332.94 
fval =   -154.7 
 
Outer iteration:  8 
Min(hin):  33.41169 Max(abs(heq)):  1.109779e-05 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  9 
Min(hin):  33.41169 Max(abs(heq)):  2.320301e-06 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  10 
Min(hin):  33.4117 Max(abs(heq)):  1.364279e-05 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  11 
Min(hin):  33.41169 Max(abs(heq)):  9.718397e-06 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  12 
Min(hin):  33.41169 Max(abs(heq)):  2.984822e-06 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  13 
Min(hin):  33.41169 Max(abs(heq)):  1.147425e-06 
par:  33.4117 332.942 
fval =   -154.7 
 
Outer iteration:  14 
Min(hin):  33.33988 Max(abs(heq)):  5.994747e-08 
par:  33.3399 333.301 
fval =   -154.7 
 
solinput = solU$par
XDHAH = solinput[1];XDHAH
[1] 33.33988
YDHAH = solinput[2];YDHAH
[1] 333.3006
UoptAH = -1*solU$value;UoptAH
[1] 154.7196




# Country F ---------------------------------------------------------------
-
+ fcnU = function(input){
+ XDF = input[1]
+ YDF = input[2]
+   ret = (XDF^v)*(YDF^Omega)
+   return(signf*ret) # signf=-1
+ }

# Budget const
fcnBC = function(input){
+ # the constraint 
+ XDF = input[1]
+ YDF = input[2]
+ Foreingppf = M-XDF-(a*YDF)
+   return(c(Foreingppf)) 
+ }
# 
fcnNonNeg = function(input){
+   # the constraint 
+   h = input
+   return(h)
+ }
# parameters 
v = 1/3 
Omega  = 2/3 
PX =1
M=500
a=5
library("alabama") # load "alabama" optimization package
stval = c(3,4)
signf = -1 # because 'auglag' is a minimization algorithm
solU = auglag(par = stval,  fn = fcnU, heq = fcnBC, hin = fcnNonNeg)
Min(hin):  3 Max(abs(heq)):  477 
Outer iteration:  1 
Min(hin):  3 Max(abs(heq)):  477 
par:  3 4 
fval =   -3.634 
 
Outer iteration:  2 
Min(hin):  60.84687 Max(abs(heq)):  46.81209 
par:  148.954 60.8469 
fval =   -82.01 
 
Outer iteration:  3 
Min(hin):  66.67732 Max(abs(heq)):  0.02489783 
par:  166.589 66.6773 
fval =   -90.48 
 
Outer iteration:  4 
Min(hin):  66.68208 Max(abs(heq)):  1.061609e-05 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  5 
Min(hin):  66.68207 Max(abs(heq)):  6.615201e-05 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  6 
Min(hin):  66.68206 Max(abs(heq)):  3.746688e-05 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  7 
Min(hin):  66.68206 Max(abs(heq)):  4.823757e-06 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  8 
Min(hin):  66.68205 Max(abs(heq)):  3.58349e-06 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  9 
Min(hin):  66.68205 Max(abs(heq)):  1.08305e-06 
par:  166.59 66.6821 
fval =   -90.48 
 
Outer iteration:  10 
Min(hin):  66.68205 Max(abs(heq)):  4.284857e-07 
par:  166.59 66.6821 
fval =   -90.48 
 
solinput = solU$par
XDFAF = solinput[1];XDFAF
[1] 166.5897
YDFAF = solinput[2];YDFAF
[1] 66.68205
UoptF = -1*solU$value;UoptF
[1] 90.48058






# World market equilibrium
# Non linear programming 
fcnU = function(input){
+ XDH = input[1]
+ YDH = input[2]
+ XDF = input[3]
+ YDF = input[4]
+ XSH = input[5]
+ YSH = input[6]
+ XSF = input[7]
+ YSF = input[8]
+ PY  = input[9]
+ ret = (XDH^v)*(YDH^Omega)+(XDF^v)*(YDF^Omega)
+ return(signf*ret) # signf=-1
+ }
# Budget const
fcnBC = function(input){
+ # constraint 
+ XDH = input[1]
+ YDH = input[2]
+ XDF = input[3]
+ YDF = input[4]
+ XSH = input[5]
+ YSH = input[6]
+ XSF = input[7]
+ YSF = input[8]
+ PY  = input[9]
+ PX = 1
+ Homeppf = 500-(5*XSH)-YSH
+ Foreingppf = 100-(0.2*XSF)-YSF
+ BOPH =-(PX*XDH)-(PY*YDH)+(PX*XSH)+(PY*YSH)
+ BOPF =-(PX*XDF)-(PY*YDF)+(PX*XSF)+(PY*YSF)
+ h = input
+ return(c(Homeppf, Foreingppf, BOPH,BOPF,h)) 
+ }
# Non 
fcnNonNeg = function(input){
+ # constraint 
+ XDH = input[1]
+ YDH = input[2]
+ XDF = input[3]
+ YDF = input[4]
+ XSH = input[5]
+ YSH = input[6]
+ XSF = input[7]
+ YSF = input[8]
+ PY  = input[9]
+ Px = 1
+ f1 = YDH + YDF - (YSH + YSF)
+ return(f1)
+ }
# parameters
v = 1/3 
Omega  = 2/3 
PX =1
library("alabama") # load "alabama" optimization package
stval = c(333.33, 333.33, 166.67, 166.67, 0, 500, 500, 0, 2)
signf = -1 # because 'auglag' is a minimization algorithm
solU = auglag(par = stval,  fn = fcnU, heq = fcnNonNeg, hin = fcnBC)
Min(hin):  -0.01 Max(abs(heq)):  0 
Outer iteration:  1 
Min(hin):  -0.01 Max(abs(heq)):  0 
par:  333.33 333.33 166.67 166.67 0 500 500 0 2 
fval =   -500 
 
Outer iteration:  2 
Min(hin):  0.007005156 Max(abs(heq)):  0.01001136 
par:  333.27 333.317 166.611 166.658 0.00700516 499.956 499.913 0.00901705 1.99997 
fval =   -499.9 
 
Outer iteration:  3 
Min(hin):  -3.456959e-05 Max(abs(heq)):  2.435119e-05 
par:  333.33 333.33 166.67 166.67 -3.45696e-05 500 500 -1.8006e-05 1.99993 
fval =   -500 
 
Outer iteration:  4 
Min(hin):  -5.688871e-05 Max(abs(heq)):  0.0001156948 
par:  333.33 333.33 166.67 166.67 2.52749e-05 500 500 -5.68887e-05 1.99994 
fval =   -500 
 
Outer iteration:  5 
Min(hin):  -5.688738e-05 Max(abs(heq)):  0.0001156917 
par:  333.33 333.33 166.67 166.67 2.52726e-05 500 500 -5.68874e-05 1.99994 
fval =   -500 
 
Outer iteration:  6 
Min(hin):  -1.751629e-06 Max(abs(heq)):  6.690926e-07 
par:  333.33 333.33 166.67 166.67 4.56476e-07 500 500 -2.97387e-07 1.99993 
fval =   -500 
 
Outer iteration:  7 
Min(hin):  -1.750066e-06 Max(abs(heq)):  6.690746e-07 
par:  333.33 333.33 166.67 166.67 4.56173e-07 500 500 -2.97347e-07 1.99993 
fval =   -500 
 
Outer iteration:  8 
Min(hin):  -1.748356e-06 Max(abs(heq)):  6.690387e-07 
par:  333.33 333.33 166.67 166.67 4.55839e-07 500 500 -2.97303e-07 1.99993 
fval =   -500 
 
Outer iteration:  9 
Min(hin):  -1.692806e-08 Max(abs(heq)):  2.802028e-08 
par:  333.33 333.33 166.67 166.67 -1.69281e-08 500 500 -9.55459e-09 1.99993 
fval =   -500 
 
solinput = solU$par
startF = paste(round(solinput,2), collapse=", ")
startF
[1] "333.33, 333.33, 166.67, 166.67, 0, 500, 500, 0, 2"
XDH1 = solinput[1];XDH1
[1] 333.3297
YDH1 = solinput[2];YDH1
[1] 333.3297
XDF1 = solinput[3];XDF1
[1] 166.6703
YDF1 = solinput[4];YDF1
[1] 166.6703
XSH1 = solinput[5];XSH1
[1] -1.692399e-08
YSH1 = solinput[6];YSH1
[1] 500
XSF1 = solinput[7];XSF1
[1] 500
YSF1 = solinput[8];YSF1
[1] -9.555138e-09
PY1 = solinput[9];PY1
[1] 1.999935
Uopt = -1*solU$value;Uopt
[1] 500



# em ad problem 9
# Country H 
fcnU = function(input){
XDH = input[1]
YDH = input[2]
ret = (XDH^v)*(YDH^Omega)
return(signf*ret) # signf=-1
}
# Budget cons
fcnBC = function(input){
# the constraint in the from of "fcnBC == 0"
XDH = input[1]
YDH = input[2]
Homeppf = M-(5*XDH)-YDH
return(c(Homeppf)) 
}
# 
fcnNonNeg = function(input){
# the constraint in the from of "fcnNonNeg >= 0"
  h = input
  return(h)
}
# parameters 
v = 1/3 
Omega  = 2/3 
PX =1
M=500
a=5
library("alabama") # load "alabama" optimization package
stval = c(3,4)
signf = -1 # because 'auglag' is a minimization algorithm
solU = auglag(par = stval,  fn = fcnU, heq = fcnBC, hin = fcnNonNeg)
solinput = solU$par
XDHAH = solinput[1];XDHAH
YDHAH = solinput[2];YDHAH
UoptAH = -1*solU$value;UoptAH




# Country F
fcnU = function(input){
XDF = input[1]
YDF = input[2]
  ret = (XDF^v)*(YDF^Omega)
  return(signf*ret) # signf=-1
}
# Budget constraint 
fcnBC = function(input){
# the constraint in the form of "fcnBC == 0"
XDF = input[1]
YDF = input[2]
Foreingppf = M-XDF-(a*YDF)
  return(c(Foreingppf)) 
}
# -
fcnNonNeg = function(input){
  # the constraint must be in from of "fcnNonNeg >= 0"
  h = input
  return(h)
}
# parameters 
v = 1/3 
Omega  = 2/3 
PX =1
M=500
a=5
library("alabama") # load "alabama" optimization package
stval = c(3,4)
signf = -1 # because 'auglag' is a minimization algorithm
solU = auglag(par = stval,  fn = fcnU, heq = fcnBC, hin = fcnNonNeg)
solinput = solU$par
XDFAF = solinput[1];XDFAF
YDFAF = solinput[2];YDFAF
UoptF = -1*solU$value;UoptF






# World market equilibrium 
fcnU = function(input){
XDH = input[1]
YDH = input[2]
XDF = input[3]
YDF = input[4]
XSH = input[5]
YSH = input[6]
XSF = input[7]
YSF = input[8]
PY  = input[9]
ret = (XDH^v)*(YDH^Omega)+(XDF^v)*(YDF^Omega)
return(signf*ret) # signf=-1
}
# Budget constr
fcnBC = function(input){
# constraint in the form of "fcnBC == 0"
XDH = input[1]
YDH = input[2]
XDF = input[3]
YDF = input[4]
XSH = input[5]
YSH = input[6]
XSF = input[7]
YSF = input[8]
PY  = input[9]
PX = 1
Homeppf = 500-(5*XSH)-YSH
Foreingppf = 100-(0.2*XSF)-YSF
BOPH =-(PX*XDH)-(PY*YDH)+(PX*XSH)+(PY*YSH)
BOPF =-(PX*XDF)-(PY*YDF)+(PX*XSF)+(PY*YSF)
h = input
return(c(Homeppf, Foreingppf, BOPH,BOPF,h)) 
}
# Non negativity condition 
fcnNonNeg = function(input){
# constraint in form of "fcnNonNeg >= 0" equality
XDH = input[1]
YDH = input[2]
XDF = input[3]
YDF = input[4]
XSH = input[5]
YSH = input[6]
XSF = input[7]
YSF = input[8]
PY  = input[9]
Px = 1
f1 = YDH + YDF - (YSH + YSF)
return(f1)
}
# parameters 
v = 1/3 
Omega  = 2/3 
PX =1
library("alabama") # load "alabama" optimization package
stval = c(333.33, 333.33, 166.67, 166.67, 0, 500, 500, 0, 2)
signf = -1 # because 'auglag' is a minimization algorithm
solU = auglag(par = stval,  fn = fcnU, heq = fcnNonNeg, hin = fcnBC)
solinput = solU$par
startF = paste(round(solinput,2), collapse=", ")
startF
XDH1 = solinput[1];XDH1
YDH1 = solinput[2];YDH1
XDF1 = solinput[3];XDF1
YDF1 = solinput[4];YDF1
XSH1 = solinput[5];XSH1
YSH1 = solinput[6];YSH1
XSF1 = solinput[7];XSF1
YSF1 = solinput[8];YSF1
PY1 = solinput[9];PY1
Uopt = -1*solU$value;Uopt
