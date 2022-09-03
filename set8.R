##### MCP ######

#params

v = 0.3
KH = 90
KF = 110
LH = 110
LF = 90
alpha = 0.3
beta = 0.7
gamma = 0.7
v = 0.3
PYFT = 1
a = 1
b = 1
AH = 8 
AF = 8
BH = 6
BF = 6


#Country H 
##Country H Consumer Problem, Max Utility=UH=(YDH)^b (XDH)^a 
############S. T. BC=IH=rH*KH+wH*LH-PYFT*YDH-PXFT*XDH
fcnMCP = function(inputs){
+   YDH = inputs[1]
+   XDH = inputs[2]
+   lamH = inputs[3]
+   YDF = inputs[4]
+   XDF = inputs[5]
+   lamF = inputs[6]
+   PXFT = inputs[7]
+   KXH = inputs[8]
+   LXH = inputs[9]
+   KYH = inputs[10]
+   LYH = inputs[11]
+   rH = inputs[12]
+   wH = inputs[13]
+   KXF = inputs[14]
+   LXF = inputs[15]
+   KYF = inputs[16]
+   LYF = inputs[17]
+   rF = inputs[18]
+   wF = inputs[19]
+   
+   ###System of FOCs for max Utility of country H
+   focYDH = b*(YDH**(b-1))*(XDH**a) - lamH*PYFT
+   focXDH = a*(YDH**b)*(XDH**(a-1)) - lamH*PXFT
+   foclamH = (rH*KH) + (wH*LH) - (PYFT*YDH) - (PXFT*XDH)
+   
+   ###System of FOCs for max Utility of country F
+   focYDF = b*(YDF**(b-1))*(XDF**a) - lamF*PYFT
+   focXDF = a*(YDF**b)*(XDF**(a-1)) - lamF*PXFT
+   foclamF = (rF*KF) + (wF*LF) - (PYFT*YDF) - (PXFT*XDF)
+   
+   ##Producer problem country H
+   ##Max Profit of good X
+   focKXH = (gamma*PXFT*BH*(KXH**(gamma-1))*(LXH**v)) - rH
+   focLXH = (v*PXFT*BH*(KXH**gamma)*(LXH**(v-1))) - wH
+   
+   ##Max Profit of good Y in country H
+   focKYH = (alpha*PYFT*AH*(KYH**(alpha-1))*(LYH**beta)) - rH
+   focLYH = (beta*PYFT*AH*(KYH**alpha)*(LYH**(beta-1))) - wH
+   
+   ##Producer problem country F
+   ##Max profit of good X in country F
+   focKXF = (gamma*PXFT*BF*(KXF**(gamma-1))*(LXF**v)) - rF
+   focLXF = (v*PXFT*BF*(KXF**gamma)*(LXF**(v-1))) - wF
+   
+   ##Max profit of good Y in country F
+   focKYF = (alpha*PYFT*AF*(KYF**(alpha-1))*(LYF**beta)) - rF
+   focLYF = (beta*PYFT*AF*(KYF**alpha)*(LYF**(beta-1))) - wF
+   
+   ##Commodity Market Equilibrium
+   cx = (BH*(KXH**gamma)*(LXH**v) + BF*(KXF**gamma)*(LXF**v)) - XDH - XDF
+   ##Factor Market EQ
+   capitalH = KH - KYH - KXH
+   laborH = LH -LYH - LXH
+   capitalF = KF - KYF - KXF
+   laborF = LF - LYF - LXF
+   
+   return(c(focYDH,focXDH,foclamH,focYDF,focXDF,foclamF,focKXH,focLXH,
+            focKYH,focLYH,focKXF,focLXF,focKYF,focLYF,cx,capitalH,laborH,capitalF,laborF))
+ }
library("rootSolve")
stval = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
solMCPh = multiroot(fcnMCP, stval)
diagonal element is zero 
[1] 13
Warning messages:
1: In stode(y, times, func, parms = parms, ...) :
  error during factorisation of matrix (dgefa);         singular matrix
2: In stode(y, times, func, parms = parms, ...) : steady-state not reached
solinput = solMCPh$root
YDH = solinput[1];YDH
[1] 405.0736
XDH = solinput[2];XDH
[1] 291.9511
lamH = solinput[3];lamH
[1] 291.9511
YDF = solinput[4];YDF
[1] 410.5144
XDF = solinput[5];XDF
[1] 295.8179
lamF = solinput[6];lamF
[1] 295.8179
PXFT = solinput[7];PXFT
[1] 1.395711
KXH = solinput[8];KXH
[1] -2428.009
LXH = solinput[9];LXH
[1] -3058.227
KYH = solinput[10];KYH
[1] 2518.009
LYH = solinput[11];LYH
[1] 3168.227
rH = solinput[12];rH
[1] 8.53736
wH = solinput[13];wH
[1] 0.3798616
KXF = solinput[14];KXF
[1] 2904.813
LXF = solinput[15];LXF
[1] 2274.969
KYF = solinput[16];KYF
[1] -2794.813
LYF = solinput[17];LYF
[1] -2184.969
rF = solinput[18];rF
[1] 9.824935
wF = solinput[19];wF
[1] -2.885712
optUH = (YDH**b)*(XDH**a);optUH
[1] 118261.7
optUF = (YDF**b)*(XDF**a);optUF
[1] 121437.5

#How much quantity of each good produced in country H
YSH = AH*(KYH**alpha)*(LYH**beta);YSH
[1] 23658.03
XSH = BH*(KXH**gamma)*(LXH**v);XSH
[1] NaN

#How much of each good produced in country F
YSF = AF*(KYF**alpha)*(LYF**beta);YSF
[1] NaN
XSF = BF*(KXF**gamma)*(LXF**v);XSF
[1] 16196.7

#Trade flow 
tradeflow = (YSH + XSH);tradeflow
[1] 23658.03
tradefloww = (YSF + XSF);tradefloww
[1] 16196.7

###############################
#QUESTION 2

##### MCP #######


#params

v = 0.3
KH = 90
KF = 110
LH = 110
LF = 90
alpha = 0.3
beta = 0.7
gamma = 0.7
v = 0.3
PYFT = 1
a = 1
b = 1
AH = 8 
AF = 8
BH = 6
BF = 6
#Country H 
##Country H Consumer Problem, Max Utility=UH=(YDH)^b (XDH)^a 
############S. T. BC=IH=rH*KH+wH*LH-PYFT*YDH-PXFT*XDH
fcnMCP = function(inputs){
+   YDH = inputs[1]
+   XDH = inputs[2]
+   lamH = inputs[3]
+   YDF = inputs[4]
+   XDF = inputs[5]
+   lamF = inputs[6]
+   PXFT = inputs[7]
+   KXH = inputs[8]
+   LXH = inputs[9]
+   KYH = inputs[10]
+   LYH = inputs[11]
+   rH = inputs[12]
+   wH = inputs[13]
+   KXF = inputs[14]
+   LXF = inputs[15]
+   KYF = inputs[16]
+   LYF = inputs[17]
+   rF = inputs[18]
+   wF = inputs[19]
+   
+   ###System of FOCs for max Utility of country H
+   focYDH = b*(YDH**(b-1))*(XDH**a) - lamH*PYFT
+   focXDH = a*(YDH**b)*(XDH**(a-1)) - lamH*PXFT
+   foclamH = (rH*KH) + (wH*LH) - (PYFT*YDH) - (PXFT*XDH)
+   
+   ###System of FOCs for max Utility of country F
+   focYDF = b*(YDF**(b-1))*(XDF**a) - lamF*PYFT
+   focXDF = a*(YDF**b)*(XDF**(a-1)) - lamF*PXFT
+   foclamF = (rF*KF) + (wF*LF) - (PYFT*YDF) - (PXFT*XDF)
+   
+   ##Producer problem country H
+   ##Max Profit of good X
+   focKXH = PXFT - (rH**2)*(AH**-2)* (wH**2) * (lamH**2) * (XSH**(-2))
+   focLXH = PXFT - (rH**2)*(AH**-2)* (wH**2) * (lamH**2) * (XSH**(-2))
+   
+   ##Max Profit of good Y in country H
+   focKYH = PYFT - (rH**2)*(AH**-2)* (wH**2) * (lamH**2) * (YSH**(-2))
+   focLYH = PYFT - (rH**2)*(AH**-2)* (wH**2) * (lamH**2) * (YSH**(-2))
+   
+   ##Producer problem country F
+   ##Max profit of good X in country F
+   focKXF = PXFT - (rF**2)*(AF**-2)* (wF**2) * (lamF**2) * (XSF**(-2))
+   focLXF = PXFT - (rF**2)*(AF**-2)* (wF**2) * (lamF**2) * (XSF**(-2))
+   
+   ##Max profit of good Y in country F
+   focKYF = PYFT - (rF**2)*(AF**-2)* (wF**2) * (lamF**2) * (YSH**(-2))
+   focLYF = PYFT - (rF**2)*(AF**-2)* (wF**2) * (lamF**2) * (YSH**(-2))
+   
+   ##Commodity Market Equilibrium
+   cx = (BH*(KXH**gamma)*(LXH**v) + BF*(KXF**gamma)*(LXF**v)) - XDH - XDF
+   ##Factor Market EQ
+   capitalH = KH - KYH - KXH
+   laborH = LH -LYH - LXH
+   capitalF = KF - KYF - KXF
+   laborF = LF - LYF - LXF
+   
+   return(c(focYDH,focXDH,foclamH,focYDF,focXDF,foclamF,focKXH,focLXH,
+            focKYH,focLYH,focKXF,focLXF,focKYF,focLYF,cx,capitalH,laborH,capitalF,laborF))
+ }
library("rootSolve")
stval = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
solMCPh = multiroot(fcnMCP, stval)
 
solinput = solMCPh$root
YDH = solinput[1];YDH
[1] 405.0736
XDH = solinput[2];XDH
[1] 291.9511
lamH = solinput[3];lamH
[1] 291.9511
YDF = solinput[4];YDF
[1] 410.5144
XDF = solinput[5];XDF
[1] 295.8179
lamF = solinput[6];lamF
[1] 295.8179
PXFT = solinput[7];PXFT
[1] 1.395711
KXH = solinput[8];KXH
[1] -2428.009
LXH = solinput[9];LXH
[1] -3058.227
KYH = solinput[10];KYH
[1] 2518.009
LYH = solinput[11];LYH
[1] 3168.227
rH = solinput[12];rH
[1] 8.53736
wH = solinput[13];wH
[1] 0.3798616
KXF = solinput[14];KXF
[1] 2904.813
LXF = solinput[15];LXF
[1] 2274.969
KYF = solinput[16];KYF
[1] -2794.813
LYF = solinput[17];LYF
[1] -2184.969
rF = solinput[18];rF
[1] 9.824935
wF = solinput[19];wF
[1] -2.885712
optUH = (YDH**b)*(XDH**a);optUH
[1] 118261.7
optUF = (YDF**b)*(XDF**a);optUF
[1] 121437.5

#How much quantity of each good produced in country H
YSH = AH*(KYH**alpha)*(LYH**beta);YSH
[1] 23658.03
XSH = BH*(KXH**gamma)*(LXH**v);XSH
[1] NaN

#How much of each good produced in country F
YSF = AF*(KYF**alpha)*(LYF**beta);YSF
[1] NaN
XSF = BF*(KXF**gamma)*(LXF**v);XSF
[1] 16196.7

#Trade flow 
tradeflow = (YSH + XSH);tradeflow
[1] 23658.03
tradefloww = (YSF + XSF);tradefloww
[1] 16196.7

