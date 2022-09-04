#Primal

# Mixed Complimentary Problem ----------------------------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
K = 100
L = 100
A = 8
B = 6
PY=1

# Objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lam = inputs[3]
  PX = inputs[4]
  KX = inputs[5]
  LX = inputs[6]
  KY = inputs[7]
  LY = inputs[8]
  r = inputs[9]
  w = inputs[10]
  
  # F O C for utility maximization Y and Y 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lam*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lam*PY
  FOCLam =(r*K)+(w*L) -(PY*YD)-(PX*XD)
  
  # producer problem
  #Profit Maximization for production of commodity X 
  FOCKX = (gama*PX*B*(KX^(gama-1))*(LX^v))- r
  FOCLX = (v*PX*B*(KX^gama)*(LX^(v-1)))- w
  
  # Profit Maximization for production of commodity Y  
  FOCKY = (alfa*PY*A*(KY^(alfa-1))*(LY^beta))- r
  FOCLY = (beta*PY*A*(KY^alfa)*(LY^(beta-1))) - w
  
  # Commotidy market Equilibrium   
  CX = (B*(KX^gama)*(LX^v))-XD 
  # Factor Market Equilibrium 
  Capital = K - KY - KX
  Labor = L - LY - LX 
  
  return(c(FOCXD, FOCYD, FOCLam, FOCKX, FOCLX, FOCKY, FOCLY,  CX, Capital,Labor)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(300,400,400,1,1,1,1,50,50,50)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YD = solinput[1];YD
XD = solinput[2];XD
Lam = solinput[3];Lam
PX = solinput[4];PX
KX = solinput[5];KX
LX = solinput[6];LX
KY = solinput[7];KY
LY = solinput[8];LY
r = solinput [9];r
w = solinput [10];w

OptUH = YD^((a-1)/a)+XD^((a-1)/a);OptUH

# Quantity of goods produced at home and in foreign
YS=A*(KY^alfa)*(LY^beta);YS
XS=B*(KX^gama)*(LX^v);XS

# Dual approach -----------------------------------------------------------
# Dual profit maximization ------------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
K = 100
L = 100
A = 8
B = 6
PY=1

# objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lam = inputs[3]
  PX = inputs[4]
  XS = inputs[5]
  YS = inputs[6]
  r = inputs[7]
  w = inputs[8]
  
  # F O C for utility maximization Y and Y 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lam*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lam*PY
  FOCLam =(r*K)+(w*L) -(PY*YD)-(PX*XD)
  
#  producer problem -------------------------------------------------------
  #Profit Maximization for production of commodity X and Y at home 
  FOCXS = PX - ( ((r/B)*((gama/v)^v)*((w/r)^v))+((w/B)*((v/gama)^gama)*((r/w)^gama)))
  FOCYS = PY- (((r/A)*((alfa/beta)^beta)*((w/r)^beta))+((w/A)*((beta/alfa)^alfa)*((r/w)^alfa)))
  # Commotidy market Equilibrium 
  CX = XS - XD 
  
  # Factor Market Equilibrium  ----------------------------------------------
  CapH = K - ((YS/A)*((alfa/beta)^beta)*((w/r)^beta)) -((XS/B)*((gama/v)^v)*((w/r)^v))
  LabH = L - ((YS/A)*((beta/alfa)^(1-beta))*((r/w)^(1-beta)))-((XS/B)*((v/gama)^(1-v))*((r/w)^(1-v)))
  return(c(FOCXD, FOCYD, FOCLam, FOCXS, FOCYS,   CX, CapH,LabH)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(276,470,0.02307917, 275.9757,469.3532,1,5,5)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root

# solutions ---------------------------------------------------------------
YD2 = solinput[1];YD2 
XD2 = solinput[2];XD2
Lam2 = solinput[3];Lam2
PX2 = solinput[4];PX2 
XS2 = solinput[5];XS2
YS2 = solinput[6];YS2
r2 = solinput[7];r2 
w2 = solinput[8];w2
OptU2 = YD^((a-1)/a)+XD^((a-1)/a);OptU2

# Quantity of goods produced at home and in foreign
YS2=A*(KY^alfa)*(LY^beta);YS2
XS2=B*(KX^gama)*(LX^v);XS2

KX = (XS2/B)*((gama/v)^v)*((w2/r2)^v);KX
LX = (XS2/B)*((v/gama)^(1-v))*((r2/w2)^(1-v));LX
KY = (YS2/A)*((alfa/beta)^beta)*((w2/r2)^beta);KY
LY = (YS2/A)*((beta/alfa)^(1-beta))*((r2/w2)^(1-beta));LY

# Anthony Macharia question 2 codes ---------------------------------------
# Question 2 --------------------------------------------------------------
#Primal 
# Mixed Complimentary Problem ---------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
K = 100
L = 100
A = 8
B = 6
PY=1

# Objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lamda = inputs[3]
  PX = inputs[4]
  KX = inputs[5]
  LX = inputs[6]
  KY = inputs[7]
  LY = inputs[8]
  r = inputs[9]
  w = inputs[10]
  
  
  prof=(PX*B*(KX^gama*LX^v))-r*KX-w*LX
  num1=(PX^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  den1=(1/(rho-1))*(PX^(2/(rho-1))+PY^(rho/(rho-1))*PX^((2-rho)/(rho-1)))
  den2=((rho/(rho-1))*PX^(2/(rho-1)))
  den3=prof+r*K+w*L
  num2=gama*B*KX^(gama-1)*LX^v
  num3=v*B*KX^gama*LX^(v-1)
  
  # F O C for utility maximization Y and X 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lamda*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lamda*PY
  FOCLamda =(r*K)+(w*L) + prof -(PY*YD)-(PX*XD)
  
  # producer problem
  #Profit Maximization for production of commodity X monopoly  
  FOCKX = (PX + XD*(num1/((den1-den2)*den3)))*num2 -r
  FOCLX = (PX + XD*(num1/((den1-den2)*den3)))*num3- w
  
  # Profit Maximization for production of commodity Y perfect competition  
  FOCKY = (alfa*PY*A*(KY^(alfa-1))*(LY^beta))- r
  FOCLY = (beta*PY*A*(KY^alfa)*(LY^(beta-1))) - w

  # Commotidy market Equilibrium   
  CX = (B*(KX^gama)*(LX^v))-XD
  
  # Factor Market Equilibrium 
  Capital = K - KY - KX
  Labor = L - LY - LX 
  
  return(c(FOCXD, FOCYD, FOCLamda, FOCKX, FOCLX, FOCKY, FOCLY,  CX, Capital,Labor)) 
}
library("rootSolve")  #load "rootSolve" package
#stval = c(400,200,0.06,60,30,40,70,2,5,5)
stval = c(700,100,0.02,3,30,5,80,100,5,5)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root

# solutions ---------------------------------------------------------------
YD = solinput[1];YD
XD = solinput[2];XD
Lamda = solinput[3];Lamda
PX = solinput[4];PX
KX = solinput[5];KX
LX = solinput[6];LX
KY = solinput[7];KY
LY = solinput[8];LY
r = solinput [9];r
w = solinput [10];w
OptUH = YD^((a-1)/a)+XD^((a-1)/a);OptUH

# Quantity of goods produced under perfect competition and monopoly
YS=A*(KY^alfa)*(LY^beta);YS
XS=B*(KX^gama)*(LX^v);XS

# Dual approach -----------------------------------------------------------
# Dual profit maximization ------------------------------------------------
# Mixed Complimentary Problem Dual problem---------------------------------

# parameters --------------------------------------------------------------
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
K = 100
L = 100
A = 8
B = 6
PY=1

# Objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lamda = inputs[3]
  PX = inputs[4]
  XS = inputs[5]
  YS = inputs[6]
  r = inputs[7]
  w = inputs[8]
  
  prof=(PX*B*(KX^gama*LX^v))-r*KX-w*LX
  num1=(PX^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  den1=(1/(rho-1))*(PX^(2/(rho-1))+PY^(rho/(rho-1))*PX^((2-rho)/(rho-1)))
  den2=((rho/(rho-1))*PX^(2/(rho-1)))
  den3=prof+r*K+w*L
  num2=gama*B*KX^(gama-1)*LX^v
  num3=v*B*KX^gama*LX^(v-1)

  # F O C for utility maximization Y and X 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lamda*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lamda*PY
  FOCLamda =(r*K)+(w*L) + prof -(PY*YD)-(PX*XD)
  
  # producer problem
  #Profit Maximization for production of commodity X and Y at home 
  FOCXS = ((PX + XD*(num1/((den1-den2)*den3))))- ( ((r/B)*((gama/v)^v)*((w/r)^v))+((w/B)*((v/gama)^gama)*((r/w)^gama)))
  FOCYS = PY- (((r/A)*((alfa/beta)^beta)*((w/r)^beta))+((w/A)*((beta/alfa)^alfa)*((r/w)^alfa)))
  
  # Commotidy market Equilibrium 
  CX = XS - XD 
  
  # Factor Market Equilibrium 
  CapH = K - ((YS/A)*((alfa/beta)^beta)*((w/r)^beta)) -((XS/B)*((gama/v)^v)*((w/r)^v))
  LabH = L - ((YS/A)*((beta/alfa)^(1-beta))*((r/w)^(1-beta)))-((XS/B)*((v/gama)^(1-v))*((r/w)^(1-v)))
  
  return(c(FOCXD, FOCYD, FOCLamda, FOCXS, FOCYS,   CX, CapH,LabH)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(695.82,92.09,0.02307917,2.74, 92.09,695.8, 2.74,5.3)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root

# solutions ---------------------------------------------------------------
YD2 = solinput[1];YD2 
XD2 = solinput[2];XD2
Lamb2 = solinput[3];Lamb2
PX2 = solinput[4];PX2 
XS2 = solinput[5];XS2
YS2 = solinput[6];YS2
r2 = solinput[7];r2 
w2 = solinput[8];w2
# Optimum utility ---------------------------------------------------------
OptU2 = YD^((a-1)/a)+XD^((a-1)/a);OptU2
# Quantity of goods produced at home and in foreign
YS2=A*(KY^alfa)*(LY^beta);YS2
XS2=B*(KX^gama)*(LX^v);XS2
KX = (XS2/B)*((gama/v)^v)*((w2/r2)^v);KX
LX = (XS2/B)*((v/gama)^(1-v))*((r2/w2)^(1-v));LX
KY = (YS2/A)*((alfa/beta)^beta)*((w2/r2)^beta);KY
LY = (YS2/A)*((beta/alfa)^(1-beta))*((r2/w2)^(1-beta));LY

# Anthony Macharia Question Three Codes -----------------------------------
# Primal approach ---------------------------------------------------------
# Mixed Complimentary Problem ---------------------------------------------

# Parameters 
alfa = 0.3
bheta = 0.7
gama = 0.6
v = 0.4
a = 2
K = 100
L = 100
A = 8
B = 6
PY=1
PX=1.30411

# Objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lam = inputs[3]
  KX = inputs[4]
  LX = inputs[5]
  KY = inputs[6]
  LY = inputs[7]
  r = inputs[8]
  w = inputs[9]
  
  # F O C for utility maximization Y and Y 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lam*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lam*PY
  FOCLam =(r*K)+(w*L) -(PY*YD)-(PX*XD) 
  
  # producer problem
  #Profit Maximization for production of commodity X 
  FOCKX = (gama*PX*B*(KX^(gama-1))*(LX^v))- r
  FOCLX = (v*PX*B*(KX^gama)*(LX^(v-1)))- w
  
  # Profit Maximization for production of commodity Y  
  FOCKY = (alfa*PY*A*(KY^(alfa-1))*(LY^bheta))- r
  FOCLY = (bheta*PY*A*(KY^alfa)*(LY^(bheta-1))) - w 
  
  # Factor Market Equilibrium 
  Capital = K - KY - KX
  Labor = L - LY - LX 
  return(c(FOCXD, FOCYD, FOCLam, FOCKX, FOCLX, FOCKY, FOCLY, Capital,Labor)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(469.35, 275.97,0.023,60.53,30.467,39.469,69.533,3.567,4.725)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root

# solutions ---------------------------------------------------------------
YD = solinput[1];YD
XD = solinput[2];XD
Lam = solinput[3];Lam
KX = solinput[4];KX
LX = solinput[5];LX
KY = solinput[6];KY
LY = solinput[7];LY
r = solinput [8];r
w = solinput [9];w

# optimum utility ---------------------------------------------------------
OptUH = YD^((a-1)/a)+XD^((a-1)/a);OptUH

# Quantity of goods produced 
YS=A*(KY^alfa)*(LY^bheta);YS
XS=B*(KX^gama)*(LX^v);XS

# Dual approach -----------------------------------------------------------
# Dual profit maximization ------------------------------------------------

# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
K = 100
L = 100
A = 8
B = 6
PY=1
PX=1.30411

# Objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YD = inputs[1]
  XD = inputs[2]
  Lam = inputs[3]
  XS = inputs[4]
  YS = inputs[5]
  r = inputs[6]
  w = inputs[7]
  
  
  # F O C for utility maximization Y and Y 
  FOCXD =((a-1)/a) *(XD^(((a-1)/a)-1))-Lam*PX
  FOCYD = ((a-1)/a) *(YD^(((a-1)/a)-1))-Lam*PY
  FOCLam =(r*K)+(w*L) -(PY*YD)-(PX*XD)
  
  # producer problem
  #Profit Maximization for production of commodity X and Y at home 
  
  FOCXS = PX - ( ((r/B)*((gama/v)^v)*((w/r)^v))+((w/B)*((v/gama)^gama)*((r/w)^gama)))
  FOCYS = PY- (((r/A)*((alfa/beta)^beta)*((w/r)^beta))+((w/A)*((beta/alfa)^alfa)*((r/w)^alfa)))

  # Factor Market Equilibrium 
  CapH = K - ((YS/A)*((alfa/beta)^beta)*((w/r)^beta)) -((XS/B)*((gama/v)^v)*((w/r)^v))
  LabH = L - ((YS/A)*((beta/alfa)^(1-beta))*((r/w)^(1-beta)))-((XS/B)*((v/gama)^(1-v))*((r/w)^(1-v)))
  return(c(FOCXD, FOCYD, FOCLam, FOCXS, FOCYS, CapH,LabH)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(276,470,0.02307917, 275.9757,469.3532,5,5)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root

# solutions ---------------------------------------------------------------
YD2 = solinput[1];YD2 
XD2 = solinput[2];XD2
Lam2 = solinput[3];Lam2
XS2 = solinput[4];XS2
YS2 = solinput[5];YS2
r2 = solinput[6];r2 
w2 = solinput[7];w2

# optimum utility ---------------------------------------------------------
OptU2 = YD^((a-1)/a)+XD^((a-1)/a);OptU2
# Quantity of goods produced at home and in foreign
YS2=A*(KY^alfa)*(LY^beta);YS2
XS2=B*(KX^gama)*(LX^v);XS2
KX = (XS2/B)*((gama/v)^v)*((w2/r2)^v);KX
LX = (XS2/B)*((v/gama)^(1-v))*((r2/w2)^(1-v));LX
KY = (YS2/A)*((alfa/beta)^beta)*((w2/r2)^beta);KY
LY = (YS2/A)*((beta/alfa)^(1-beta))*((r2/w2)^(1-beta));LY




 #Question 4
#Primal
# Mixed Complimentary Problem ----------------------------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100

KF = 100
LF = 100

AH = 8
AF=8
BH = 6
BF = 6

PY=1

fcnUFOC = function(inputs){
  YDH = inputs[1] 
  XDH = inputs[2]
  LamH = inputs[3]
  YDF = inputs[4]
  XDF = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  KXH = inputs[8]
  LXH = inputs[9]
  KXF = inputs[10]
  LXF = inputs[11]
  KYH = inputs[12]
  LYH = inputs[13]
  KYF = inputs[14]
  LYF = inputs[15]
  rH = inputs[16]
  wH = inputs[17]  
  rF = inputs[18]
  wF = inputs[19]
  
  
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  X at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization X at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X monopoly  at h 
  
  FOCKXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num2H -rH
  
  FOCLXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num3H- wH
  
  #Profit Maximization for production of commodity X monopoly  at F
  
  FOCKXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num2F -rF
  
  FOCLXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num3F- wF
  
  
  # Profit Maximization for production of commodity Y perfect competition  at H
  FOCKYH = (alfa*PY*AH*(KYH^(alfa-1))*(LYH^beta))- rH
  FOCLYH = (beta*PY*AH*(KYH^alfa)*(LYH^(beta-1))) - wH
  
  # Profit Maximization for production of commodity Y perfect competition  at F
  FOCKYF = (alfa*PY*AF*(KYF^(alfa-1))*(LYF^beta))- rF
  FOCLYF = (beta*PY*AF*(KYF^alfa)*(LYF^(beta-1))) - wF
  
  
  
  # Commotidy market Equilibrium 
  
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF 
  
  # Factor Market Equilibrium at H
  CapitalH = KH - KYH - KXH
  LaborH = LH - LYH - LXH 
  
  # Factor Market Equilibrium at F
  CapitalF = KF - KYF - KXF
  LaborF = LF - LYF - LXF
  
  
  return(c(FOCXDH, FOCYDH, FOCLamH,FOCXDF, FOCYDF, FOCLamF, FOCKXH, FOCLXH, FOCKXF, FOCLXF, FOCKYH, FOCLYH, FOCKYF, FOCLYF, CX, CapitalH,LaborH,CapitalF,LaborF)) 
}
library("rootSolve")  #load "rootSolve" package
#stval = c(400,200,0.06,60,30,40,70,2,5,5)
#stval = c(92.096,695.83,0.02,92.096,695.83,0.02,23.54,8.084,23.54,8.084,76.463,91.92,76.463,91.92,5,2.73,5.299,2.73,5.299)
stval = c(583.73,185.75,0.02,583.73,185.75,0.02,1.77,43.95,18.3,43.95,18.3,56.05,81.7,56.05,81.7,3.12,5,3.12,5)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDH = solinput[1];YDH
XDH= solinput[2];XDH
LamH = solinput[3];LamH
YDF = solinput[4];YDF
XDF = solinput[5];XDF
LamF = solinput[6];LamF

PXFT = solinput[7];PXFT
KXH = solinput[8];KXH
LXH = solinput[9];LXH
KXF = solinput[10];KXF
LXF = solinput[11];LXF

KYH = solinput[12];KYH
LYH = solinput[13];LYH
KYF = solinput[14];KYF
LYF = solinput[15];LYF


rH = solinput [16];rH
wH= solinput [17];wH
rF = solinput [18];rF
wF = solinput [19];wF


OptUH = YDH^((a-1)/a)+XDH^((a-1)/a);OptUH

OptUF = YDF^((a-1)/a)+XDF^((a-1)/a);OptUF

# Quantity of goods produced under perfect competition and monopoly
YSH=AH*(KYH^alfa)*(LYH^beta);YSH

XSH=BH*(KXH^gama)*(LXH^v);XSH

YSF=AF*(KYF^alfa)*(LYF^beta);YSF

XSF=BF*(KXF^gama)*(LXF^v);XSF


#Question 4 Dual
# Dual profit maximization ------------------------------------------------


# Mixed Complimentary Problem Dual problem----------------------------------------------------------------

# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100
KF = 100
LF = 100
AH = 8
AF=8
BH = 6
BF = 6
PY=1


fcnUFOC = function(inputs){
  YDH = inputs[1]
  XDH = inputs[2]
  YDF = inputs[3]
  XDF = inputs[4]
  LamH = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  XSH = inputs[8]
  YSH = inputs[9]
  XSF = inputs[10]
  YSF = inputs[11]
  rH = inputs[12]
  wH = inputs[13]
  rF = inputs[14]
  wF = inputs[15]
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X and Y at home 
  
  
  FOCXSH = ((PXFT + XDH*(num1H/((den1H-den2H)*den3H))))- ( ((rH/BH)*((gama/v)^v)*((wH/rH)^v))+((wH/BH)*((v/gama)^gama)*((rH/wH)^gama)))
  FOCYSH = PY- (((rH/AH)*((alfa/beta)^beta)*((wH/rH)^beta))+((wH/AH)*((beta/alfa)^alfa)*((rH/wH)^alfa)))
  
  #Profit Maximization for production of commodity X and Y at F 
  
  FOCXSF = ((PXFT + XDF*(num1F/((den1F-den2F)*den3F))))- ( ((rF/BF)*((gama/v)^v)*((wF/rF)^v))+((wF/BF)*((v/gama)^gama)*((rF/wF)^gama)))
  FOCYSF = PY- (((rF/AF)*((alfa/beta)^beta)*((wF/rF)^beta))+((wF/AF)*((beta/alfa)^alfa)*((rF/wF)^alfa)))
  
  # Commotidy market Equilibrium 
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF   
  
  # Factor Market Equilibrium at H
  
  CapHH = KH - ((YSH/AH)*((alfa/beta)^beta)*((wH/rH)^beta)) -((XSH/BH)*((gama/v)^v)*((wH/rH)^v))
  LabHH = LH - ((YSH/AH)*((beta/alfa)^(1-beta))*((rH/wH)^(1-beta)))-((XSH/BH)*((v/gama)^(1-v))*((rH/wH)^(1-v)))
  
  # Factor Market Equilibrium at F
  CapHF = KF - ((YSF/AF)*((alfa/beta)^beta)*((wF/rF)^beta)) -((XSF/BF)*((gama/v)^v)*((wF/rF)^v))
  LabHF = LF - ((YSF/AF)*((beta/alfa)^(1-beta))*((rF/wF)^(1-beta)))-((XSF/BF)*((v/gama)^(1-v))*((rF/wF)^(1-v)))
  
  return(c(FOCXDH,FOCYDH, FOCLamH, FOCXDF, FOCYDF, FOCLamF, FOCXSH, FOCYSH, FOCXSF, FOCYSF, CX, CapHH, LabHH, CapHF, LabHF)) 
}


library("rootSolve")  #load "rootSolve" package

#YDH  XDH  YDF  XDF  LamdaH  LamdaF PXFT   XSH YSH XSF YSF  rH wH  rF  wF
stval = c(583.73,185.75,583.73,185.75,0.02,0.02,1.77, 5,5, 5,5, 3.124,5,3.124,5 )

signf = -1 

solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YD2H = solinput[1];YD2H 
XD2H = solinput[2];XD2H
YD2F = solinput[3];YD2F
XD2F = solinput[4];XD2F

Lam2H = solinput[5];Lam2H
Lam2F = solinput[6];Lam2F

PX2FT = solinput[7];PX2FT
XS2H = solinput[8];XS2H
YS2H = solinput[9];YS2H
XS2F = solinput[10];XS2F
YS2F = solinput[11];YS2F

r2H = solinput[12];r2H 
w2H = solinput[13];w2H
r2F = solinput[14];r2F 
w2F = solinput[15];w2F


# Optimum utilities -------------------------------------------------------
OptUH2 = YD2H^((a-1)/a)+XD2H^((a-1)/a);OptUH2
OptUF2 = YD2F^((a-1)/a)+XD2F^((a-1)/a);OptUF2


# Quantity of goods produced at home 
YS2H=AH*(KYH^alfa)*(LYH^beta);YS2H
XS2H=BH*(KXH^gama)*(LXH^v);XS2H


# Quantity of goods produced at foreign
YS2F=AF*(KYF^alfa)*(LYF^beta);YS2F
XS2F=BF*(KXF^gama)*(LXF^v);XS2F


KXH = (XS2H/BH)*((gama/v)^v)*((w2H/r2H)^v);KXH
LXH = (XS2H/BH)*((v/gama)^(1-v))*((r2H/w2H)^(1-v));LXH
KYH = (YS2H/AH)*((alfa/beta)^beta)*((w2H/r2H)^beta);KYH
LYH = (YS2H/AH)*((beta/alfa)^(1-beta))*((r2H/w2H)^(1-beta));LYH

KXF = (XS2F/BF)*((gama/v)^v)*((w2F/r2F)^v);KXF
LXF = (XS2F/BF)*((v/gama)^(1-v))*((r2F/w2F)^(1-v));LXF
KYF = (YS2F/AF)*((alfa/beta)^beta)*((w2F/r2F)^beta);KYF
LYF = (YS2F/AF)*((beta/alfa)^(1-beta))*((r2F/w2F)^(1-beta));LYF
# Mixed Complimentary Problem ----------------------------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100

KF = 100
LF = 100

AH = 8
AF=8
BH = 4.7
BF = 6

PY=1

fcnUFOC = function(inputs){
  YDH = inputs[1] 
  XDH = inputs[2]
  LamH = inputs[3]
  YDF = inputs[4]
  XDF = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  KXH = inputs[8]
  LXH = inputs[9]
  KXF = inputs[10]
  LXF = inputs[11]
  KYH = inputs[12]
  LYH = inputs[13]
  KYF = inputs[14]
  LYF = inputs[15]
  rH = inputs[16]
  wH = inputs[17]  
  rF = inputs[18]
  wF = inputs[19]
  
  
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  X at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization X at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X monopoly  at h 
  
  FOCKXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num2H -rH
  
  FOCLXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num3H- wH
  
  #Profit Maximization for production of commodity X monopoly  at F
  
  FOCKXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num2F -rF
  
  FOCLXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num3F- wF
  
  
  # Profit Maximization for production of commodity Y perfect competition  at H
  FOCKYH = (alfa*PY*AH*(KYH^(alfa-1))*(LYH^beta))- rH
  FOCLYH = (beta*PY*AH*(KYH^alfa)*(LYH^(beta-1))) - wH
  
  # Profit Maximization for production of commodity Y perfect competition  at F
  FOCKYF = (alfa*PY*AF*(KYF^(alfa-1))*(LYF^beta))- rF
  FOCLYF = (beta*PY*AF*(KYF^alfa)*(LYF^(beta-1))) - wF
  
  
  
  # Commotidy market Equilibrium 
  
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF 
  

  # Factor Market Equilibrium at H
  CapitalH = KH - KYH - KXH
  LaborH = LH - LYH - LXH 
  
  # Factor Market Equilibrium at F
  CapitalF = KF - KYF - KXF
  LaborF = LF - LYF - LXF
  
  
  return(c(FOCXDH, FOCYDH, FOCLamH,FOCXDF, FOCYDF, FOCLamF, FOCKXH, FOCLXH, FOCKXF, FOCLXF, FOCKYH, FOCLYH, FOCKYF, FOCLYF, CX, CapitalH,LaborH,CapitalF,LaborF)) 
}
library("rootSolve")  #load "rootSolve" package
#stval = c(400,200,0.06,60,30,40,70,2,5,5)
#stval = c(92.096,695.83,0.02,92.096,695.83,0.02,23.54,8.084,23.54,8.084,76.463,91.92,76.463,91.92,5,2.73,5.299,2.73,5.299)
stval = c( 560.3379,139.486,0.02, 666.915, 173.348,0.02, 1.964557,11.41063,2.89,66.62,32.13,88.59,102.9,33.38,67.87,2.4,5.48,3.63,4.6)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDH = solinput[1];YDH
XDH= solinput[2];XDH
LamH = solinput[3];LamH
YDF = solinput[4];YDF
XDF = solinput[5];XDF
LamF = solinput[6];LamF

PXFT = solinput[7];PXFT
KXH = solinput[8];KXH
LXH = solinput[9];LXH
KXF = solinput[10];KXF
LXF = solinput[11];LXF

KYH = solinput[12];KYH
LYH = solinput[13];LYH
KYF = solinput[14];KYF
LYF = solinput[15];LYF


rH = solinput [16];rH
wH= solinput [17];wH
rF = solinput [18];rF
wF = solinput [19];wF


OptUH = YDH^((a-1)/a)+XDH^((a-1)/a);OptUH

OptUF = YDF^((a-1)/a)+XDF^((a-1)/a);OptUF

# Quantity of goods produced under perfect competition and monopoly
YSH=AH*(KYH^alfa)*(LYH^beta);YSH

XSH=BH*(KXH^gama)*(LXH^v);XSH

YSF=AF*(KYF^alfa)*(LYF^beta);YSF

XSF=BF*(KXF^gama)*(LXF^v);XSF



# Dual profit maximization ------------------------------------------------


# Mixed Complimentary Problem Dual problem----------------------------------------------------------------

# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100
KF = 100
LF = 100
AH = 8
AF=8
BH = 4.7
BF = 6
PY=1


fcnUFOC = function(inputs){
  YDH = inputs[1]
  XDH = inputs[2]
  YDF = inputs[3]
  XDF = inputs[4]
  LamH = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  XSH = inputs[8]
  YSH = inputs[9]
  XSF = inputs[10]
  YSF = inputs[11]
  rH = inputs[12]
  wH = inputs[13]
  rF = inputs[14]
  wF = inputs[15]
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X and Y at home 
  
  
  FOCXSH = ((PXFT + XDH*(num1H/((den1H-den2H)*den3H))))- ( ((rH/BH)*((gama/v)^v)*((wH/rH)^v))+((wH/BH)*((v/gama)^gama)*((rH/wH)^gama)))
  FOCYSH = PY- (((rH/AH)*((alfa/beta)^beta)*((wH/rH)^beta))+((wH/AH)*((beta/alfa)^alfa)*((rH/wH)^alfa)))
  
  #Profit Maximization for production of commodity X and Y at F 
  
  FOCXSF = ((PXFT + XDF*(num1F/((den1F-den2F)*den3F))))- ( ((rF/BF)*((gama/v)^v)*((wF/rF)^v))+((wF/BF)*((v/gama)^gama)*((rF/wF)^gama)))
  FOCYSF = PY- (((rF/AF)*((alfa/beta)^beta)*((wF/rF)^beta))+((wF/AF)*((beta/alfa)^alfa)*((rF/wF)^alfa)))
  
  # Commotidy market Equilibrium 
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF   
  
  # Factor Market Equilibrium at H
  
  CapHH = KH - ((YSH/AH)*((alfa/beta)^beta)*((wH/rH)^beta)) -((XSH/BH)*((gama/v)^v)*((wH/rH)^v))
  LabHH = LH - ((YSH/AH)*((beta/alfa)^(1-beta))*((rH/wH)^(1-beta)))-((XSH/BH)*((v/gama)^(1-v))*((rH/wH)^(1-v)))
  
  # Factor Market Equilibrium at F
  CapHF = KF - ((YSF/AF)*((alfa/beta)^beta)*((wF/rF)^beta)) -((XSF/BF)*((gama/v)^v)*((wF/rF)^v))
  LabHF = LF - ((YSF/AF)*((beta/alfa)^(1-beta))*((rF/wF)^(1-beta)))-((XSF/BF)*((v/gama)^(1-v))*((rF/wF)^(1-v)))
  
  return(c(FOCXDH,FOCYDH, FOCLamH, FOCXDF, FOCYDF, FOCLamF, FOCXSH, FOCYSH, FOCXSF, FOCYSF, CX, CapHH, LabHH, CapHF, LabHF)) 
}


library("rootSolve")  #load "rootSolve" package

#YDH  XDH  YDF  XDF  LamdaH  LamdaF PXFT   XSH YSH XSF YSF  rH wH  rF  wF
stval = c(583.73,185.75,583.73,185.75,0.02,0.02,1.77, 5,5, 5,5, 3.124,5,3.124,5 )

signf = -1 

solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YD2H = solinput[1];YD2H 
XD2H = solinput[2];XD2H
YD2F = solinput[3];YD2F
XD2F = solinput[4];XD2F

Lam2H = solinput[5];Lam2H
Lam2F = solinput[6];Lam2F

PX2FT = solinput[7];PX2FT
XS2H = solinput[8];XS2H
YS2H = solinput[9];YS2H
XS2F = solinput[10];XS2F
YS2F = solinput[11];YS2F

r2H = solinput[12];r2H 
w2H = solinput[13];w2H
r2F = solinput[14];r2F 
w2F = solinput[15];w2F


OptU2H = YDH^((a-1)/a)+XDH^((a-1)/a);OptU2H
OptU2F = YDF^((a-1)/a)+XDF^((a-1)/a);OptU2F


# Quantity of goods produced at home 
YS2H=AH*(KYH^alfa)*(LYH^beta);YS2H
XS2H=BH*(KXH^gama)*(LXH^v);XS2H


# Quantity of goods produced at foreign
YS2F=AF*(KYF^alfa)*(LYF^beta);YS2F
XS2F=BF*(KXF^gama)*(LXF^v);XS2F


KXH = (XS2H/BH)*((gama/v)^v)*((w2H/r2H)^v);KXH
LXH = (XS2H/BH)*((v/gama)^(1-v))*((r2H/w2H)^(1-v));LXH
KYH = (YS2H/AH)*((alfa/beta)^beta)*((w2H/r2H)^beta);KYH
LYH = (YS2H/AH)*((beta/alfa)^(1-beta))*((r2H/w2H)^(1-beta));LYH

KXF = (XS2F/BF)*((gama/v)^v)*((w2F/r2F)^v);KXF
LXF = (XS2F/BF)*((v/gama)^(1-v))*((r2F/w2F)^(1-v));LXF
KYF = (YS2F/AF)*((alfa/beta)^beta)*((w2F/r2F)^beta);KYF
LYF = (YS2F/AF)*((beta/alfa)^(1-beta))*((r2F/w2F)^(1-beta));LYF





# Question 6 primal

# Mixed Complimentary Problem ----------------------------------------------------------------
# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100

KF = 40
LF = 40

AH = 8
AF=8
BH = 6
BF = 6

PY=1

fcnUFOC = function(inputs){
  YDH = inputs[1] 
  XDH = inputs[2]
  LamH = inputs[3]
  YDF = inputs[4]
  XDF = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  KXH = inputs[8]
  LXH = inputs[9]
  KXF = inputs[10]
  LXF = inputs[11]
  KYH = inputs[12]
  LYH = inputs[13]
  KYF = inputs[14]
  LYF = inputs[15]
  rH = inputs[16]
  wH = inputs[17]  
  rF = inputs[18]
  wF = inputs[19]
  
  
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  X at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization X at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X monopoly  at h 
  
  FOCKXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num2H -rH
  
  FOCLXH = (PXFT + XDH*(num1H/((den1H-den2H)*den3H)))*num3H- wH
  
  #Profit Maximization for production of commodity X monopoly  at F
  
  FOCKXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num2F -rF
  
  FOCLXF = (PXFT + XDF*(num1F/((den1F-den2F)*den3F)))*num3F- wF
  
  
  # Profit Maximization for production of commodity Y perfect competition  at H
  FOCKYH = (alfa*PY*AH*(KYH^(alfa-1))*(LYH^beta))- rH
  FOCLYH = (beta*PY*AH*(KYH^alfa)*(LYH^(beta-1))) - wH
  
  # Profit Maximization for production of commodity Y perfect competition  at F
  FOCKYF = (alfa*PY*AF*(KYF^(alfa-1))*(LYF^beta))- rF
  FOCLYF = (beta*PY*AF*(KYF^alfa)*(LYF^(beta-1))) - wF
  
  
  
  # Commotidy market Equilibrium 
  
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF 
  
  
  
  # Factor Market Equilibrium at H
  CapitalH = KH - KYH - KXH
  LaborH = LH - LYH - LXH 
  
  # Factor Market Equilibrium at F
  CapitalF = KF - KYF - KXF
  LaborF = LF - LYF - LXF
  
  
  return(c(FOCXDH, FOCYDH, FOCLamH,FOCXDF, FOCYDF, FOCLamF, FOCKXH, FOCLXH, FOCKXF, FOCLXF, FOCKYH, FOCLYH, FOCKYF, FOCLYF, CX, CapitalH,LaborH,CapitalF,LaborF)) 
}
library("rootSolve")  #load "rootSolve" package
#         YDh,  XDH, LamH, YDF, XDF, LAMF, PFXT ,KXH ,LXH, KXF, LXF, KYH, LYH, KYF, LYF,   rh, wH, rF, wF
stval = c(579.54, 166.16, 0.02,136.98, 39.27, 0.04, 1.87, 30.84, 11.3, 15.99, 10.65, 69.16, 88.7, 4.01, 9.35, 2.86, 5.2, 4.34, 4.34)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDH = solinput[1];YDH
XDH= solinput[2];XDH
LamH = solinput[3];LamH
YDF = solinput[4];YDF
XDF = solinput[5];XDF
LamF = solinput[6];LamF

PXFT = solinput[7];PXFT
KXH = solinput[8];KXH
LXH = solinput[9];LXH
KXF = solinput[10];KXF
LXF = solinput[11];LXF

KYH = solinput[12];KYH
LYH = solinput[13];LYH
KYF = solinput[14];KYF
LYF = solinput[15];LYF


rH = solinput [16];rH
wH= solinput [17];wH
rF = solinput [18];rF
wF = solinput [19];wF


OptUH = YDH^((a-1)/a)+XDH^((a-1)/a);OptUH

OptUF = YDF^((a-1)/a)+XDF^((a-1)/a);OptUF

# Quantity of goods produced under perfect competition and monopoly
YSH=AH*(KYH^alfa)*(LYH^beta);YSH

XSH=BH*(KXH^gama)*(LXH^v);XSH

YSF=AF*(KYF^alfa)*(LYF^beta);YSF

XSF=BF*(KXF^gama)*(LXF^v);XSF





# Dual approach -----------------------------------------------------------
# Dual profit maximization ------------------------------------------------
# Mixed Complimentary Problem Dual problem----------------------------------------------------------------

# parameters 
alfa = 0.3
beta = 0.7
gama = 0.6
v = 0.4
a = 2
rho=0.5
KH = 100
LH = 100
KF = 40
LF = 40
AH = 8
AF=8
BH = 6
BF = 6
PY=1

# objective function ------------------------------------------------------
fcnUFOC = function(inputs){
  YDH = inputs[1]
  XDH = inputs[2]
  YDF = inputs[3]
  XDF = inputs[4]
  LamH = inputs[5]
  LamF = inputs[6]
  PXFT = inputs[7]
  XSH = inputs[8]
  YSH = inputs[9]
  XSF = inputs[10]
  YSF = inputs[11]
  rH = inputs[12]
  wH = inputs[13]
  rF = inputs[14]
  wF = inputs[15]
  
  profH=(PXFT*BH*(KXH^gama*LXH^v))-rH*KXH-wH*LXH
  
  profF=(PXFT*BF*(KXF^gama*LXF^v))-rF*KXF-wF*LXF
  
  num1H=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  num1F=(PXFT^(rho/(rho-1))+PY^(rho/(rho-1)))^2
  
  den1H=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den1F=(1/(rho-1))*(PXFT^(2/(rho-1))+PY^(rho/(rho-1))*PXFT^((2-rho)/(rho-1)))
  
  den2H=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den2F=((rho/(rho-1))*PXFT^(2/(rho-1)))
  
  den3H=profH+rH*KH+wH*LH +profF+rF*KF+wF*LF
  
  den3F=profF+rF*KF+wF*LF+ profH+rH*KH+wH*LH
  
  num2H=gama*BH*KXH^(gama-1)*LXH^v
  
  num2F=gama*BF*KXF^(gama-1)*LXF^v
  
  num3H=v*BH*KXH^gama*LXH^(v-1)
  
  num3F=v*BF*KXF^gama*LXF^(v-1)
  
  
  # F O C for utility maximization  at H
  FOCXDH =((a-1)/a) *(XDH^(((a-1)/a)-1))-LamH*PXFT
  FOCYDH = ((a-1)/a) *(YDH^(((a-1)/a)-1))-LamH*PY
  FOCLamH =(rH*KH)+(wH*LH) + profH -(PY*YDH)-(PXFT*XDH)
  
  # F O C for utility maximization at F 
  FOCXDF =((a-1)/a) *(XDF^(((a-1)/a)-1))-LamF*PXFT
  FOCYDF = ((a-1)/a) *(YDF^(((a-1)/a)-1))-LamF*PY
  FOCLamF =(rF*KF)+(wF*LF) + profF -(PY*YDF)-(PXFT*XDF)
  
  
  # producer problem
  #Profit Maximization for production of commodity X and Y at home 
  
  
  FOCXSH = ((PXFT + XDH*(num1H/((den1H-den2H)*den3H))))- ( ((rH/BH)*((gama/v)^v)*((wH/rH)^v))+((wH/BH)*((v/gama)^gama)*((rH/wH)^gama)))
  FOCYSH = PY- (((rH/AH)*((alfa/beta)^beta)*((wH/rH)^beta))+((wH/AH)*((beta/alfa)^alfa)*((rH/wH)^alfa)))
  
  #Profit Maximization for production of commodity X and Y at F 
  
  FOCXSF = ((PXFT + XDF*(num1F/((den1F-den2F)*den3F))))- ( ((rF/BF)*((gama/v)^v)*((wF/rF)^v))+((wF/BF)*((v/gama)^gama)*((rF/wF)^gama)))
  FOCYSF = PY- (((rF/AF)*((alfa/beta)^beta)*((wF/rF)^beta))+((wF/AF)*((beta/alfa)^alfa)*((rF/wF)^alfa)))
  
  # Commotidy market Equilibrium 
  CX = (BH*(KXH^gama)*(LXH^v)+BF*(KXF^gama)*(LXF^v))-XDH-XDF   
  
  # Factor Market Equilibrium at H
  
  CapHH = KH - ((YSH/AH)*((alfa/beta)^beta)*((wH/rH)^beta)) -((XSH/BH)*((gama/v)^v)*((wH/rH)^v))
  LabHH = LH - ((YSH/AH)*((beta/alfa)^(1-beta))*((rH/wH)^(1-beta)))-((XSH/BH)*((v/gama)^(1-v))*((rH/wH)^(1-v)))
  
  # Factor Market Equilibrium at F
  CapHF = KF - ((YSF/AF)*((alfa/beta)^beta)*((wF/rF)^beta)) -((XSF/BF)*((gama/v)^v)*((wF/rF)^v))
  LabHF = LF - ((YSF/AF)*((beta/alfa)^(1-beta))*((rF/wF)^(1-beta)))-((XSF/BF)*((v/gama)^(1-v))*((rF/wF)^(1-v)))
  
  return(c(FOCXDH,FOCYDH, FOCLamH, FOCXDF, FOCYDF, FOCLamF, FOCXSH, FOCYSH, FOCXSF, FOCYSF, CX, CapHH, LabHH, CapHF, LabHF)) 
}


library("rootSolve")  #load "rootSolve" package

#YDH  XDH  YDF  XDF  LamH  LamF PXFT   XSH YSH XSF YSF  rH wH  rF  wF
stval = c( 542.967, 154.5073, 283.0672, 80.5499, 0.0214, 0.0297, 1.8746, 42.8982, 752.1911, 192.159, 73.843, 2.547,5.459, 4.836, 4.147)

signf = -1 

solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YD2H = solinput[1];YD2H 
XD2H = solinput[2];XD2H
YD2F = solinput[3];YD2F
XD2F = solinput[4];XD2F

Lam2H = solinput[5];Lam2H
Lam2F = solinput[6];Lam2F

PX2FT = solinput[7];PX2FT
XS2H = solinput[8];XS2H
YS2H = solinput[9];YS2H
XS2F = solinput[10];XS2F
YS2F = solinput[11];YS2F

r2H = solinput[12];r2H 
w2H = solinput[13];w2H
r2F = solinput[14];r2F 
w2F = solinput[15];w2F


OptU2H = YDH^((a-1)/a)+XDH^((a-1)/a);OptU2H
OptU2F = YDF^((a-1)/a)+XDF^((a-1)/a);OptU2F


# Quantity of goods produced at home 
YS2H=AH*(KYH^alfa)*(LYH^beta);YS2H
XS2H=BH*(KXH^gama)*(LXH^v);XS2H


# Quantity of goods produced at foreign
YS2F=AF*(KYF^alfa)*(LYF^beta);YS2F
XS2F=BF*(KXF^gama)*(LXF^v);XS2F


KXH = (XS2H/BH)*((gama/v)^v)*((w2H/r2H)^v);KXH
LXH = (XS2H/BH)*((v/gama)^(1-v))*((r2H/w2H)^(1-v));LXH
KYH = (YS2H/AH)*((alfa/beta)^beta)*((w2H/r2H)^beta);KYH
LYH = (YS2H/AH)*((beta/alfa)^(1-beta))*((r2H/w2H)^(1-beta));LYH

KXF = (XS2F/BF)*((gama/v)^v)*((w2F/r2F)^v);KXF
LXF = (XS2F/BF)*((v/gama)^(1-v))*((r2F/w2F)^(1-v));LXF
KYF = (YS2F/AF)*((alfa/beta)^beta)*((w2F/r2F)^beta);KYF
LYF = (YS2F/AF)*((beta/alfa)^(1-beta))*((r2F/w2F)^(1-beta));LYF


