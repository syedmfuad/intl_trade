# parameters 
alfa = 0.3
beta = 0.7
gama = 0.7
v = 0.3
b = 1
a = 1
AH = 8
AF = 8
BH = 6
BF = 6
KH = 100
KF = 100
LH = 100
LF = 100
PYFT = 1
fcnUFOC = function(inputs){
  YDH = inputs[1]
  XDH = inputs[2]
  lamH = inputs[3]
  YDF = inputs[4]
  XDF = inputs[5]
  lamF = inputs[6]
  PXFT = inputs[7]
  KHX = inputs[8]
  LHX = inputs[9]
  KHY = inputs[10]
  LHY = inputs[11]
  rH = inputs[12]
  wH = inputs[13]
  KFX = inputs[14]
  LFX = inputs[15]
  KFY = inputs[16]
  LFY = inputs[17]
  rF = inputs[18]
  wF = inputs[19]
  
  # F O C FOR COUNTRY H 
  FOCYDH = b*(YDH^(b-1))*(XDH^a)-lamH*PYFT
  FOCXDH = a*(YDH^b)*(XDH^(a-1))-lamH*PXFT
  FOClamH =(rH*KH)+(wH*LH) -(PYFT*YDH)-(PXFT*XDH)
  
  # F O C FOR COUNTRY F
  FOCYDF = b*(YDF^(b-1))*(XDF^a)-lamF*PYFT
  FOCXDF = a*(YDF^b)*(XDF^(a-1))-lamF*PXFT
  FOClamF = (rF*KF)+(wF*LF) -(PYFT*YDF)-(PXFT*XDF)
  
  # COUNRTRY H PROFIT FOR X
  FOCKHX = (gama*PXFT*BH*(KHX^(gama-1))*(LHX^v))- rH
  FOCLHX = (v*PXFT*BH*(KHX^gama)*(LHX^(v-1)))- wH
  
  # COUNRTRY H PROFIT FOR Y
  FOCKHY = (alfa*PYFT*AH*(KHY^(alfa-1))*(LHY^beta))- rH
  FOCLHY = (beta*PYFT*AH*(KHY^alfa)*(LHY^(beta-1))) - wH
  
  # COUNRTRY F PROFIT FOR X
  FOCKFX = (gama*PXFT*BF*(KFX^(gama-1))*(LFX^v)) - rF
  FOCLFX = (v*PXFT*BF*(KFX^gama)*(LFX^(v-1))) - wF
  
  # COUNRTRY F PROFIT FOR Y
  FOCKFY = (alfa*PYFT*AF*(KFY^(alfa-1))*(LFY^beta)) - rF
  FOCLFY = (beta*PYFT*AF*(KFY^alfa)*(LFY^(beta-1))) - wF
  
  # C M EQ
  CX = (BH*(KHX^gama)*(LHX^v)+BF*(KFX^gama)*(LFX^v))-XDH-XDF 
  
  # F M EQ
  CapitalH = KH - KHY - KHX
  laborH = LH - LHY - LHX  
  capitalF = KF - KFY- KFX
  laborF = LF- LFY - LFX
  return(c(FOCYDH, FOCXDH, FOClamH, FOCYDF, FOCXDF, FOClamF, FOCKHX, FOCLHX, FOCKHY, FOCLHY, FOCKFX, FOCLFX, FOCKFY, FOCLFY, CX, CapitalH,laborH, capitalF, laborF)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(430,320,400,1,1,1,1,50,50,50,50,3,2,50,50,50,50,1,3)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDH = solinput[1];YDH 
XDH = solinput[2];XDH
lamH = solinput[3];lamH
YDF = solinput[4];YDF 
XDF = solinput[5];XDF
lamF = solinput[6];lamF
PXFT = solinput[7];PXFT
KHX = solinput[8];KHX
LHX = solinput[9];LHX
KHY = solinput[10];KHY
LHY = solinput[11];LHY
rH =  solinput[12];rH
wH =  solinput[13];wH
KFX = solinput[14];KFX
LFX = solinput[15];LFX
KFY = solinput[16];KFY
LFY = solinput[17];LFY
rF =  solinput[18];rF
wF =  solinput[19];wF
OptUH = (YDH**b)*(XDH**a);OptUH
OptUF = (YDF**b)*(XDF**a);OptUF
# COUNTRY H GOOD PROD
YSH=AH*(KHY^alfa)*(LHY^beta);YSH
XSH=BH*(KHX^gama)*(LHX^v);XSH
# COUNTRY F GOOD PROD
YSF=AF*(KFY^alfa)*(LFY^beta);YSF
XSF=BF*(KFX^gama)*(LFX^v);XSF


# parameters--------------------------------------------------------------- 
b = 1
a = 1
AH = 8
AF = 8
BH = 6
BF = 6
KH = 100
KF = 100
LH = 100
LF = 100
alfa = 0.3
beta = 0.7
gama = 0.7
v = 0.3
PYFT = 1

# OB F
fcnUFOC = function(inputs){
  
  YDH = inputs[1]
  XDH = inputs[2]
  LamH = inputs[3]
  YDF = inputs[4]
  XDF = inputs[5]
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
  
  # COUNTRY H c p 
  FOCYDH = b*(YDH^(b-1))*(XDH^a)-LamH*PYFT
  FOCXDH = a*(YDH^b)*(XDH^(a-1))-LamH*PXFT
  FOCLamH =(rH*KH)+(wH*LH) -(PXFT*XDH)-(PYFT*YDH)
  
  # COUNTRY f c p
  FOCYDF = b*(YDF^(b-1))*(XDF^a)-LamF*PYFT
  FOCXDF = a*(YDF^b)*(XDF^(a-1))-LamF*PXFT
  FOCLamF = (rF*KF)+(wF*LF) -(PYFT*YDF)-(PXFT*XDF)
  
  # COUNTRY H p p
  FOCXSH = PXFT - ( ((rH/BH)*((gama/v)^v)*((wH/rH)^v))+((wH/BH)*((v/gama)^gama)*((rH/wH)^gama)))
  FOCYSH = PYFT- (((rH/AH)*((alfa/beta)^beta)*((wH/rH)^beta))+((wH/AH)*((beta/alfa)^alfa)*((rH/wH)^alfa)))
  
  # COUNTRY f p p
  FOCXSF = PXFT - (((rF/BF)*((gama/v)^v)*((wF/rF)^v))+((wF/BF)*((v/gama)^gama)*((rF/wF)^gama)))
  FOCYSF = PYFT - (((rF/AF)*((alfa/beta)^beta)*((wF/rF)^beta))+((wF/AF)*((beta/alfa)^alfa)*((rF/wF)^alfa)))
  
  # c m eq
  CX = XSH + XSF - XDH - XDF
  
  
  # f m eq
  KHH = KH - ((YSH/AH)*((alfa/beta)^beta)*((wH/rH)^beta)) -((XSH/BH)*((gama/v)^v)*((wH/rH)^v))
  LHH = LH - ((YSH/AH)*((beta/alfa)^(1-beta))*((rH/wH)^(1-beta)))-((XSH/BH)*((v/gama)^(1-v))*((rH/wH)^(1-v)))
  KFF = KF - ((YSF/AF)*((alfa/beta)^beta)*((wF/rF)^beta)) -((XSF/BF)*((gama/v)^v)*((wF/rF)^v))
  LFF = LF - ((YSF/AF)*((beta/alfa)^(1-beta))*((rF/wF)^(1-beta)))-((XSF/BF)*((v/gama)^(1-v))*((rF/wF)^(1-v)))
  return(c(FOCYDH, FOCXDH, FOCLamH, FOCYDF, FOCXDF, FOCLamF, FOCXSH, FOCYSH, FOCXSF, FOCYSF, CX, KHH, LHH, KFF, LFF)) 
}
library("rootSolve")  #load "rootSolve" package
stval = c(300,400,400,1,1,1,1,50,50,50,50,3,2,50,50)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDHD = solinput[1];YDHD 
XDHD = solinput[2];XDHD
lamHD = solinput[3];lamHD
YDFD = solinput[4];YDFD
XDFD = solinput[5];XDFD
lamFD = solinput[6];lamFD
PXFTD = solinput[7];PXFTD
XSHD = solinput[8];XSHD
YSHD = solinput[9];YSHD
XSFD = solinput[10];XSFD
YSFD = solinput[11];YSFD
rHD =  solinput[12];rHD
wHD =  solinput[13];wHD
rFD =  solinput[14];rFD
wFD =  solinput[15];wFD
optUHD = (YDHD**b)*(XDHD**a);optUHD
optUFD = (YDFD**b)*(XDFD**a);optUFD
# k l in x and y 
KHXD = (XSHD/BH)*((gama/v)^v)*((wHD/rHD)^v);KHXD
LHXD = (XSHD/BH)*((v/gama)^(1-v))*((rHD/wHD)^(1-v));LHXD
KHYD = (YSHD/AH)*((alfa/beta)^beta)*((wHD/rHD)^beta);KHYD
LHYD = (YSHD/AH)*((beta/alfa)^(1-beta))*((rHD/wHD)^(1-beta));LHYD
KFXD = (XSFD/BF)*((gama/v)^v)*((wFD/rFD)^v);KFX
LFXD = (XSFD/BF)*((v/gama)^(1-v))*((rFD/wFD)^(1-v));LFXD
KFYD = (YSFD/AF)*((alfa/beta)^beta)*((wFD/rFD)^beta);KFYD
LFYD = (YSFD/AF)*((beta/alfa)^(1-beta))*((rFD/wFD)^(1-beta));LFYD

# parameters 
alfa = 0.3
beta = 0.7
gama = 0.7
v = 0.3
b = 1
a = 1
AH = 8
AF = 8
BH = 6
BF = 6
KH = 100
KF = 100
LH = 100
LF = 100
PYFT = 1
fcnUFOC = function(inputs){
+   YDH = inputs[1]
+   XDH = inputs[2]
+   lamH = inputs[3]
+   YDF = inputs[4]
+   XDF = inputs[5]
+   lamF = inputs[6]
+   PXFT = inputs[7]
+   KHX = inputs[8]
+   LHX = inputs[9]
+   KHY = inputs[10]
+   LHY = inputs[11]
+   rH = inputs[12]
+   wH = inputs[13]
+   KFX = inputs[14]
+   LFX = inputs[15]
+   KFY = inputs[16]
+   LFY = inputs[17]
+   rF = inputs[18]
+   wF = inputs[19]
+   
+   # F O C FOR COUNTRY H 
+   FOCYDH = b*(YDH^(b-1))*(XDH^a)-lamH*PYFT
+   FOCXDH = a*(YDH^b)*(XDH^(a-1))-lamH*PXFT
+   FOClamH =(rH*KH)+(wH*LH) -(PYFT*YDH)-(PXFT*XDH)
+   
+   # F O C FOR COUNTRY F
+   FOCYDF = b*(YDF^(b-1))*(XDF^a)-lamF*PYFT
+   FOCXDF = a*(YDF^b)*(XDF^(a-1))-lamF*PXFT
+   FOClamF = (rF*KF)+(wF*LF) -(PYFT*YDF)-(PXFT*XDF)
+   
+   # COUNRTRY H PROFIT FOR X
+   FOCKHX = (gama*PXFT*BH*(KHX^(gama-1))*(LHX^v))- rH
+   FOCLHX = (v*PXFT*BH*(KHX^gama)*(LHX^(v-1)))- wH
+   
+   # COUNRTRY H PROFIT FOR Y
+   FOCKHY = (alfa*PYFT*AH*(KHY^(alfa-1))*(LHY^beta))- rH
+   FOCLHY = (beta*PYFT*AH*(KHY^alfa)*(LHY^(beta-1))) - wH
+   
+   # COUNRTRY F PROFIT FOR X
+   FOCKFX = (gama*PXFT*BF*(KFX^(gama-1))*(LFX^v)) - rF
+   FOCLFX = (v*PXFT*BF*(KFX^gama)*(LFX^(v-1))) - wF
+   
+   # COUNRTRY F PROFIT FOR Y
+   FOCKFY = (alfa*PYFT*AF*(KFY^(alfa-1))*(LFY^beta)) - rF
+   FOCLFY = (beta*PYFT*AF*(KFY^alfa)*(LFY^(beta-1))) - wF
+   
+   # C M EQ
+   CX = (BH*(KHX^gama)*(LHX^v)+BF*(KFX^gama)*(LFX^v))-XDH-XDF 
+   
+   # F M EQ
+   CapitalH = KH - KHY - KHX
+   laborH = LH - LHY - LHX  
+   capitalF = KF - KFY- KFX
+   laborF = LF- LFY - LFX
+   return(c(FOCYDH, FOCXDH, FOClamH, FOCYDF, FOCXDF, FOClamF, FOCKHX, FOCLHX, FOCKHY, FOCLHY, FOCKFX, FOCLFX, FOCKFY, FOCLFY, CX, CapitalH,laborH, capitalF, laborF)) 
+ }
library("rootSolve")  #load "rootSolve" package
stval = c(430,320,400,1,1,1,1,50,50,50,50,3,2,50,50,50,50,1,3)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDH = solinput[1];YDH 
[1] 434.3052
XDH = solinput[2];XDH
[1] 325.7289
lamH = solinput[3];lamH
[1] 325.7289
YDF = solinput[4];YDF 
[1] 434.3052
XDF = solinput[5];XDF
[1] 325.7289
lamF = solinput[6];lamF
[1] 325.7289
PXFT = solinput[7];PXFT
[1] 1.333333
KHX = solinput[8];KHX
[1] 70
LHX = solinput[9];LHX
[1] 30
KHY = solinput[10];KHY
[1] 30
LHY = solinput[11];LHY
[1] 70
rH =  solinput[12];rH
[1] 4.343052
wH =  solinput[13];wH
[1] 4.343052
KFX = solinput[14];KFX
[1] 70
LFX = solinput[15];LFX
[1] 30
KFY = solinput[16];KFY
[1] 30
LFY = solinput[17];LFY
[1] 70
rF =  solinput[18];rF
[1] 4.343052
wF =  solinput[19];wF
[1] 4.343052
OptUH = (YDH**b)*(XDH**a);OptUH
[1] 141465.7
OptUF = (YDF**b)*(XDF**a);OptUF
[1] 141465.7
# COUNTRY H GOOD PROD
YSH=AH*(KHY^alfa)*(LHY^beta);YSH
[1] 434.3052
XSH=BH*(KHX^gama)*(LHX^v);XSH
[1] 325.7289
# COUNTRY F GOOD PROD
YSF=AF*(KFY^alfa)*(LFY^beta);YSF
[1] 434.3052
XSF=BF*(KFX^gama)*(LFX^v);XSF
[1] 325.7289


# parameters--------------------------------------------------------------- 
b = 1
a = 1
AH = 8
AF = 8
BH = 6
BF = 6
KH = 100
KF = 100
LH = 100
LF = 100
alfa = 0.3
beta = 0.7
gama = 0.7
v = 0.3
PYFT = 1

# OB F
fcnUFOC = function(inputs){
+   
+   YDH = inputs[1]
+   XDH = inputs[2]
+   LamH = inputs[3]
+   YDF = inputs[4]
+   XDF = inputs[5]
+   LamF = inputs[6]
+   PXFT = inputs[7]
+   XSH = inputs[8]
+   YSH = inputs[9]
+   XSF = inputs[10]
+   YSF = inputs[11]
+   rH = inputs[12]
+   wH = inputs[13]
+   rF = inputs[14]
+   wF = inputs[15]
+   
+   # COUNTRY H c p 
+   FOCYDH = b*(YDH^(b-1))*(XDH^a)-LamH*PYFT
+   FOCXDH = a*(YDH^b)*(XDH^(a-1))-LamH*PXFT
+   FOCLamH =(rH*KH)+(wH*LH) -(PXFT*XDH)-(PYFT*YDH)
+   
+   # COUNTRY f c p
+   FOCYDF = b*(YDF^(b-1))*(XDF^a)-LamF*PYFT
+   FOCXDF = a*(YDF^b)*(XDF^(a-1))-LamF*PXFT
+   FOCLamF = (rF*KF)+(wF*LF) -(PYFT*YDF)-(PXFT*XDF)
+   
+   # COUNTRY H p p
+   FOCXSH = PXFT - ( ((rH/BH)*((gama/v)^v)*((wH/rH)^v))+((wH/BH)*((v/gama)^gama)*((rH/wH)^gama)))
+   FOCYSH = PYFT- (((rH/AH)*((alfa/beta)^beta)*((wH/rH)^beta))+((wH/AH)*((beta/alfa)^alfa)*((rH/wH)^alfa)))
+   
+   # COUNTRY f p p
+   FOCXSF = PXFT - (((rF/BF)*((gama/v)^v)*((wF/rF)^v))+((wF/BF)*((v/gama)^gama)*((rF/wF)^gama)))
+   FOCYSF = PYFT - (((rF/AF)*((alfa/beta)^beta)*((wF/rF)^beta))+((wF/AF)*((beta/alfa)^alfa)*((rF/wF)^alfa)))
+   
+   # c m eq
+   CX = XSH + XSF - XDH - XDF
+   
+   
+   # f m eq
+   KHH = KH - ((YSH/AH)*((alfa/beta)^beta)*((wH/rH)^beta)) -((XSH/BH)*((gama/v)^v)*((wH/rH)^v))
+   LHH = LH - ((YSH/AH)*((beta/alfa)^(1-beta))*((rH/wH)^(1-beta)))-((XSH/BH)*((v/gama)^(1-v))*((rH/wH)^(1-v)))
+   KFF = KF - ((YSF/AF)*((alfa/beta)^beta)*((wF/rF)^beta)) -((XSF/BF)*((gama/v)^v)*((wF/rF)^v))
+   LFF = LF - ((YSF/AF)*((beta/alfa)^(1-beta))*((rF/wF)^(1-beta)))-((XSF/BF)*((v/gama)^(1-v))*((rF/wF)^(1-v)))
+   return(c(FOCYDH, FOCXDH, FOCLamH, FOCYDF, FOCXDF, FOCLamF, FOCXSH, FOCYSH, FOCXSF, FOCYSF, CX, KHH, LHH, KFF, LFF)) 
+ }
library("rootSolve")  #load "rootSolve" package
stval = c(300,400,400,1,1,1,1,50,50,50,50,3,2,50,50)
signf = -1 
solUFOC = multiroot(fcnUFOC, stval)
solinput = solUFOC$root
YDHD = solinput[1];YDHD 
[1] 434.3052
XDHD = solinput[2];XDHD
[1] 325.7289
lamHD = solinput[3];lamHD
[1] 325.7289
YDFD = solinput[4];YDFD
[1] 434.3052
XDFD = solinput[5];XDFD
[1] 325.7289
lamFD = solinput[6];lamFD
[1] 325.7289
PXFTD = solinput[7];PXFTD
[1] 1.333333
XSHD = solinput[8];XSHD
[1] 325.7289
YSHD = solinput[9];YSHD
[1] 434.3052
XSFD = solinput[10];XSFD
[1] 325.7289
YSFD = solinput[11];YSFD
[1] 434.3052
rHD =  solinput[12];rHD
[1] 4.343052
wHD =  solinput[13];wHD
[1] 4.343052
rFD =  solinput[14];rFD
[1] 4.343052
wFD =  solinput[15];wFD
[1] 4.343052
optUHD = (YDHD**b)*(XDHD**a);optUHD
[1] 141465.7
optUFD = (YDFD**b)*(XDFD**a);optUFD
[1] 141465.7
# k l in x and y 
KHXD = (XSHD/BH)*((gama/v)^v)*((wHD/rHD)^v);KHXD
[1] 70
LHXD = (XSHD/BH)*((v/gama)^(1-v))*((rHD/wHD)^(1-v));LHXD
[1] 30
KHYD = (YSHD/AH)*((alfa/beta)^beta)*((wHD/rHD)^beta);KHYD
[1] 30
LHYD = (YSHD/AH)*((beta/alfa)^(1-beta))*((rHD/wHD)^(1-beta));LHYD
[1] 70
KFXD = (XSFD/BF)*((gama/v)^v)*((wFD/rFD)^v);KFX
[1] 70
LFXD = (XSFD/BF)*((v/gama)^(1-v))*((rFD/wFD)^(1-v));LFXD
[1] 30
KFYD = (YSFD/AF)*((alfa/beta)^beta)*((wFD/rFD)^beta);KFYD
[1] 30
LFYD = (YSFD/AF)*((beta/alfa)^(1-beta))*((rFD/wFD)^(1-beta));LFYD
[1] 70
