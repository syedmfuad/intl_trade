dIus=250 # demand intercept for US
dSus=-2.5# demand slope for US

sIus=25 # supply intercept for US
sSus=1.25 # supply slope for US

dIeu=125 # demand intercept EU
dSeu=-1.75 # demand slope for EU

sIeu=50 # supply intercept for EU
sSeu=2 # supply slope for EU

fcnSDUS=function(x){
  QA=x[1]
  PA=x[2]
  
  supplyus= QA- (sIus + sSus*PA) # QA-(25+1.25*p) #R assumes this is equal to zero
  demandus= QA- (dIus + dSus*PA) # QA-(250+2.5*p)
  
  return(c(supplyus,demandus))
}

fcnSDEU=function(x){
  
  QB =x[1]
  PB =x[2]
  
  supplyeu = QB- (sIeu + sSeu*PB) # QB-(50+2*p)
  demandeu = QB- (dIeu + dSeu*PB)
  return(c(supplyeu,demandeu))
}

fncforWP= function(x){ # world price
  QW = x[1]
  PW = x[2]
  
  ED = QW - ((dIus + dSus*PW) - (sIus + sSus*PW)) # (qd-qs)
  
  ES= QW - ((sIeu + sSeu*PW) - (dIeu + dSeu*PW)) # (qs-qd)
  
  return(c(ED,ES))
}

library("rootSolve")
solnus= multiroot(fcnSDUS, c(1,1))
QA = solnus$root[1];QA
PA = solnus$root[2];PA

solneu = multiroot(fcnSDEU, c(1,1))
QB = solneu$root[1];QB
PB = solneu$root[2];PB

solnw = multiroot(fncforWP, c(1,1))
QW = solnw$root[1];QW
PW = solnw$root[2];PW

## Welfare Analysis ##

# change in country A (United States)
ChPS.A = sIus*(PW-PA) + (sSus/2)*(PW**2-PA**2); ChPS.A
ChCS.A = dIus*(PW-PA) + (dSus/2)*(PW**2-PA**2); ChCS.A

# change in country B (European Union)
ChPS.B = sIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChPS.B
ChCS.B = dIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChCS.B

# net change 
NchA = ChPS.A + ChCS.A; NchA #net welfare gains for A
NchB = ChPS.B + ChCS.B; NchB #net welfare gains for B
NchW = NchA + NchB; NchW #net world gains





#problem set1
#using MCP 
dIus=250 # demand intercept for US
dSus=-2.5# demand slope for US

sIus=25 # supply intercept for US
sSus=1.25 # supply slope for US

dIeu=125 # demand intercept EU
dSeu=-1.75 # demand slope for EU

sIeu=50 # supply intercept for EU
sSeu=2 # supply slope for EU

fcnSDUS=function(x){
+   QA=x[1]
+   PA=x[2]
+   
+   supplyus= QA- (sIus + sSus*PA) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandus= QA- (dIus + dSus*PA) # QA-(250+2.5*p)
+   
+   return(c(supplyus,demandus))
+ }

fcnSDEU=function(x){
+   
+   QB =x[1]
+   PB =x[2]
+   
+   supplyeu = QB- (sIeu + sSeu*PB) # QB-(50+2*p)
+   demandeu = QB- (dIeu + dSeu*PB)
+   return(c(supplyeu,demandeu))
+ }

fncforWP= function(x){ # world price
+   QW = x[1]
+   PW = x[2]
+   
+   ED = QW - ((dIus + dSus*PW) - (sIus + sSus*PW)) # (qd-qs)
+   
+   ES= QW - ((sIeu + sSeu*PW) - (dIeu + dSeu*PW)) # (qs-qd)
+   
+   return(c(ED,ES))
+ }

library("rootSolve")
solnus= multiroot(fcnSDUS, c(1,1))
QA = solnus$root[1];QA
[1] 99.99998
PA = solnus$root[2];PA
[1] 60

solneu = multiroot(fcnSDEU, c(1,1))
QB = solneu$root[1];QB
[1] 90
PB = solneu$root[2];PB
[1] 20

solnw = multiroot(fncforWP, c(1,1))
QW = solnw$root[1];QW
[1] 75
PW = solnw$root[2];PW
[1] 40

## Welfare Analysis ##

# change in country A (United States)
ChPS.A = sIus*(PW-PA) + (sSus/2)*(PW**2-PA**2); ChPS.A
[1] -1750
ChCS.A = dIus*(PA-PW) + (dSus/2)*(PA**2-PW**2); ChCS.A
[1] 2500

# change in country B (European Union)
ChPS.B = sIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChPS.B
[1] 2200
ChCS.B = dIeu*(PB-PW) + (dSeu/2)*(PB**2-PW**2); ChCS.B
[1] -1450

# net change 
NchA = ChPS.A + ChCS.A; NchA #net welfare gains for A
[1] 750
NchB = ChPS.B + ChCS.B; NchB #net welfare gains for B
[1] 750
NchW = NchA + NchB; NchW #net world gains
[1] 1500
#problem set1
#using MCP 
dIus=250 # demand intercept for US
dSus=-2.5# demand slope for US

sIus=25 # supply intercept for US
sSus=1.25 # supply slope for US

dIeu=125 # demand intercept EU
dSeu=-1.75 # demand slope for EU

sIeu=50 # supply intercept for EU
sSeu=2 # supply slope for EU

fcnSDUS=function(x){
+   QA=x[1]
+   PA=x[2]
+   
+   supplyus= QA- (sIus + sSus*PA) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandus= QA- (dIus + dSus*PA) # QA-(250+2.5*p)
+   
+   return(c(supplyus,demandus))
+ }

fcnSDEU=function(x){
+   
+   QB =x[1]
+   PB =x[2]
+   
+   supplyeu = QB- (sIeu + sSeu*PB) # QB-(50+2*p)
+   demandeu = QB- (dIeu + dSeu*PB)
+   return(c(supplyeu,demandeu))
+ }

fncforWP= function(x){ # world price
+   QW = x[1]
+   PW = x[2]
+   
+   ED = QW - ((dIus + dSus*PW) - (sIus + sSus*PW)) # (qd-qs)
+   
+   ES= QW - ((sIeu + sSeu*PW) - (dIeu + dSeu*PW)) # (qs-qd)
+   
+   return(c(ED,ES))
+ }

library("rootSolve")
solnus= multiroot(fcnSDUS, c(1,1))
QA = solnus$root[1];QA
[1] 99.99998
PA = solnus$root[2];PA
[1] 60

solneu = multiroot(fcnSDEU, c(1,1))
QB = solneu$root[1];QB
[1] 90
PB = solneu$root[2];PB
[1] 20

solnw = multiroot(fncforWP, c(1,1))
QW = solnw$root[1];QW
[1] 75
PW = solnw$root[2];PW
[1] 40

## Welfare Analysis ##

# change in country A (United States)
ChPS.A = sIus*(PW-PA) + (sSus/2)*(PW**2-PA**2); ChPS.A
[1] -1750
ChCS.A = dIus*(PA-PW) + (dSus/2)*(PA**2-PW**2); ChCS.A
[1] 2500

# change in country B (European Union)
ChPS.B = sIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChPS.B
[1] 2200
ChCS.B = dIeu*(PB-PW) + (dSeu/2)*(PB**2-PW**2); ChCS.B
[1] -1450

# net change 
NchA = ChPS.A + ChCS.A; NchA #net welfare gains for A
[1] 750
NchB = ChPS.B + ChCS.B; NchB #net welfare gains for B
[1] 750
NchW = NchA + NchB; NchW #net world gains
[1] 1500


dIus=250 # demand intercept for US
dSus=-2.5# demand slope for US

sIus=25 # supply intercept for US
sSus=1.25 # supply slope for US

dIeu=125 # demand intercept EU
dSeu=-1.75 # demand slope for EU

sIeu=50 # supply intercept for EU
sSeu=2 # supply slope for EU

fcnSDUS=function(x){
+   QA=x[1]
+   PA=x[2]
+   
+   supplyus= QA- (sIus + sSus*PA) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandus= QA- (dIus + dSus*PA) # QA-(250+2.5*p)
+   
+   return(c(supplyus,demandus))
+ }

fcnSDEU=function(x){
+   
+   QB =x[1]
+   PB =x[2]
+   
+   supplyeu = QB- (sIeu + sSeu*PB) # QB-(50+2*p)
+   demandeu = QB- (dIeu + dSeu*PB)
+   return(c(supplyeu,demandeu))
+ }

fncforWP= function(x){ # world price
+   QW = x[1]
+   PW = x[2]
+   
+   ED = QW - ((dIus + dSus*PW) - (sIus + sSus*PW)) # (qd-qs)
+   
+   ES= QW - ((sIeu + sSeu*PW) - (dIeu + dSeu*PW)) # (qs-qd)
+   
+   return(c(ED,ES))
+ }

library("rootSolve")
solnus= multiroot(fcnSDUS, c(1,1))
QA = solnus$root[1];QA
[1] 99.99998
PA = solnus$root[2];PA
[1] 60

solneu = multiroot(fcnSDEU, c(1,1))
QB = solneu$root[1];QB
[1] 90
PB = solneu$root[2];PB
[1] 20

solnw = multiroot(fncforWP, c(1,1))
QW = solnw$root[1];QW
[1] 75
PW = solnw$root[2];PW
[1] 40

## Welfare Analysis ##

# change in country A (United States)
ChPS.A = sIus*(PW-PA) + (sSus/2)*(PW**2-PA**2); ChPS.A
[1] -1750
ChCS.A = dIus*(PW-PA) + (dSus/2)*(PW**2-PA**2); ChCS.A
[1] -2500

# change in country B (European Union)
ChPS.B = sIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChPS.B
[1] 2200
ChCS.B = dIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChCS.B
[1] 3700

# net change 
NchA = ChPS.A + ChCS.A; NchA #net welfare gains for A
[1] -4249.999
NchB = ChPS.B + ChCS.B; NchB #net welfare gains for B
[1] 5900
NchW = NchA + NchB; NchW #net world gains
[1] 1650.001
#problem set1
#using MCP 
dIus=250 # demand intercept for US
dSus=-2.5# demand slope for US

sIus=25 # supply intercept for US
sSus=1.25 # supply slope for US

dIeu=125 # demand intercept EU
dSeu=-1.75 # demand slope for EU

sIeu=50 # supply intercept for EU
sSeu=2 # supply slope for EU

fcnSDUS=function(x){
+   QA=x[1]
+   PA=x[2]
+   
+   supplyus= QA- (sIus + sSus*PA) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandus= QA- (dIus + dSus*PA) # QA-(250+2.5*p)
+   
+   return(c(supplyus,demandus))
+ }

fcnSDEU=function(x){
+   
+   QB =x[1]
+   PB =x[2]
+   
+   supplyeu = QB- (sIeu + sSeu*PB) # QB-(50+2*p)
+   demandeu = QB- (dIeu + dSeu*PB)
+   return(c(supplyeu,demandeu))
+ }

fncforWP= function(x){ # world price
+   QW = x[1]
+   PW = x[2]
+   
+   ED = QW - ((dIus + dSus*PW) - (sIus + sSus*PW)) # (qd-qs)
+   
+   ES= QW - ((sIeu + sSeu*PW) - (dIeu + dSeu*PW)) # (qs-qd)
+   
+   return(c(ED,ES))
+ }

library("rootSolve")
solnus= multiroot(fcnSDUS, c(1,1))
QA = solnus$root[1];QA
[1] 99.99998
PA = solnus$root[2];PA
[1] 60

solneu = multiroot(fcnSDEU, c(1,1))
QB = solneu$root[1];QB
[1] 90
PB = solneu$root[2];PB
[1] 20

solnw = multiroot(fncforWP, c(1,1))
QW = solnw$root[1];QW
[1] 75
PW = solnw$root[2];PW
[1] 40

## Welfare Analysis ##

# change in country A (United States)
ChPS.A = sIus*(PW-PA) + (sSus/2)*(PW**2-PA**2); ChPS.A
[1] -1750
ChCS.A = dIus*(PA-PW) + (dSus/2)*(PA**2-PW**2); ChCS.A
[1] 2500

# change in country B (European Union)
ChPS.B = sIeu*(PW-PB) + (sSeu/2)*(PW**2-PB**2); ChPS.B
[1] 2200
ChCS.B = dIeu*(PB-PW) + (dSeu/2)*(PB**2-PW**2); ChCS.B
[1] -1450

# net change 
NchA = ChPS.A + ChCS.A; NchA #net welfare gains for A
[1] 750
NchB = ChPS.B + ChCS.B; NchB #net welfare gains for B
[1] 750
NchW = NchA + NchB; NchW #net world gains
[1] 1500
