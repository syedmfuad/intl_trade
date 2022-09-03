region1DI=100 # region 1 demand intercept
region1DS=-8 # region 1 demand slope

region1SI=-50 # region 1 supply Intercept 
region1SS=10 # region 1 supply slope

region2DI=100 # region2 demand intercept 
region2DS=-6 # region2 demand slope

regio2SI=-35 #region 2 supply intercept
region2SS=7 # region 2 supply slope

region3DI=133 #region 3 demand intercept
region3DS=-6 # region 3 demand slope
 region3SI=-26 # region 3 supply intercept
 region3SS=2.5 # region 3  supply slope 
 
region4DI=123 # region 4 demand intercept
region4DS=-4 # region 4  demand slope 
 region44SI=-16 # region 4 supply intercept
 region4SS=3.6 # region 4 supply slope
 
#autarcky equlibrium price and quantity for region 1
 fcnregion1=function(x){
   QA1=x[1]
   PA1=x[2]
   
   supplyR1= QA1- (region1SI + region1SS*PA1) # QA-(25+1.25*p) #R assumes this is equal to zero
   demandR1= QA1- (region1DI + region1DS*PA1) # QA-(250+2.5*p)
   
   return(c(supplyR1,demandR1))
 }

 #autarcky equlibrium price and quantity for region 2
 fcnregion2=function(x){
   QA2=x[1]
   PA2=x[2]
   
   supplyR2= QA2- (regio2SI + region2SS*PA2) # QA-(25+1.25*p) #R assumes this is equal to zero
   demandR2= QA2- ( region2DI+ region2DS*PA2) # QA-(250+2.5*p)
   
   return(c(supplyR2,demandR2))
 }
 
 #autarcky equlibrium price and quantity for region 3
 
 fcnregion3=function(x){
   QA3=x[1]
   PA3=x[2]
   
   supplyR3= QA3- ( region3SI+ region3SS*PA3) # QA-(25+1.25*p) #R assumes this is equal to zero
   demandR3= QA3- (region3DI + region3DS*PA3) # QA-(250+2.5*p)
   
   return(c(supplyR3,demandR3))
 }
 
 #autarcky equlibrium price and quantity for region 4
 
 fcnregion4=function(x){
   QA4=x[1]
   PA4=x[2]
   
   supplyR4= QA4- ( region44SI + region4SS*PA4) # QA-(25+1.25*p) #R assumes this is equal to zero
   demandR4= QA4- (region4DI + region4DS*PA4) # QA-(250+2.5*p)
   
   return(c(supplyR4,demandR4))
 }
 
fncforworldprice= function(x){
  QW= x[1]
  PW = x[2]
  
  TotalExcessSupply = QW- (((region1SI + region1SS*PW) - (region1DI + region1DS*PW)) + ((regio2SI + region2SS*PW) - (region2DI+region2DS*PW)))
  TotalExcessDemeand= QW- (((region3DI + region3DS*PW)- (region3SI+ region3SS*PW))+ ((region4DI + region4DS* PW)- (region44SI+region4SS*PW)))
  #qw is total import for the region 3 and 4 in case of total excess demand
  return(c(TotalExcessSupply,TotalExcessDemeand))
  
}


library("rootSolve")
#to print region 1 autarcky price and quantity 
solnregion1= multiroot(fcnregion1, c(1,1))
QA1 = solnregion1$root[1];QA1
PA1 = solnregion1$root[2];PA1

#to print region 2 autarcky price and quantity
solnregion2= multiroot(fcnregion2, c(1,1))
QA2 = solnregion2$root[1];QA2
PA2 = solnregion2$root[2];PA2

#to print region 3 autarcky price and quantity
solnregion3= multiroot(fcnregion3, c(1,1))
QA3 = solnregion3$root[1];QA3
PA3 = solnregion3$root[2];PA3

#to print region 4 autarcky price and quantity
solnregion4= multiroot(fcnregion4, c(1,1))
QA4 = solnregion4$root[1];QA4
PA4 = solnregion4$root[2];PA4

# to print free trade price and quantity 
solnw = multiroot(fncforworldprice, c(1,1))
QW = solnw$root[1];QW
PW = solnw$root[2];PW

# qt supplied  for Each region 
A1 = (region1SI + region1SS*PW);A1
A2 = (regio2SI + region2SS*PW);A2
A3 = ( region3SI+ region3SS*PW);A3
A4= (region44SI + region4SS*PW);A4


# qt sdemanded for each region 
sA1= (region1DI + region1DS*PW);sA1
 sA2 =( region2DI+ region2DS*PW);sA2
 sA3= (region3DI + region3DS*PW);sA3
 sA4 =(region4DI + region4DS*PW); sA4

# change in region 1
ChPS.R1=region1SI*(PW-PA1) + (region1SS/2)*(PW**2-PA1**2);ChPS.R1
ChCS.R1=  region1DI*(PA1-PW) + (region1DS/2)*(PA1**2-PW**2);ChCS.R1

# change in region 2
ChPS.R2=regio2SI*(PW-PA2) + (region2SS/2)*(PW**2-PA2**2);ChPS.R2
ChCS.R2=  region2DI*(PA2-PW) + (region2DS/2)*(PA2**2-PW**2);ChCS.R2

# change in region 3
ChPS.R3=region3SI*(PW-PA3) + (region3SS/2)*(PW**2-PA3**2);ChPS.R3
ChCS.R3= region3DI *(PA3-PW) + (region3DS/2)*(PA3**2-PW**2);ChCS.R3

# change in region 4
ChPS.R4=region44SI*(PW-PA4) + (region4SS/2)*(PW**2-PA4**2);ChPS.R4
ChCS.R4= region4DI*(PA4-PW) + (region4DS/2)*(PA4**2-PW**2);ChCS.R4

#net change 
NchR1=ChPS.R1+ChCS.R1;NchR1 # net change for region 1
NchR2=ChPS.R2+ChCS.R2;NchR2 # net change for region 2
NchR3=ChPS.R3+ChCS.R3;NchR3 # net change for region 3
NchR4=ChPS.R4+ChCS.R4;NchR4 #net change for region 4

NchalR=NchR1+NchR2+NchR3+NchR4;NchalR # world net gain






































#Problem set 2
region1ed=100 # region 1demand intercept
region1es=-8 # region 1 demand slope

region1sn=-50 # region 1 supply Intercept 
region1Sf=10 # region 1 supply slope

region2dy=100 # region2 demand intercept 
region2Ddf=-6 # region2 demand slope

regio2dm=-35 #region 2 supply intercept
region2dh=7 # region 2 supply slope

region3ss=133 #region 3 demand intercept
region3sn=-6 # region 3 demand slope
region3yd=-26 # region 3 supply intercept
region3SS=2.5 # region 3  supply slope 

region4Da=123 # region 4 demand intercept
region4Do=-4 # region 4  demand slope 
region44mI=-16 # region 4 supply intercept
region4SS=3.6 # region 4 supply slope

#autarcky equlibrium price and quantity for region 1
fcnregion1=function(x){
+   QA1=x[1]
+   PA1=x[2]
+   
+   supplyR1= QA1- (region1SI + region1SS*PA1) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandR1= QA1- (region1DI + region1DS*PA1) # QA-(250+2.5*p)
+   
+   return(c(supplyR1,demandR1))
+ }

#autarcky equlibrium price and quantity for region 2
fcnregion2=function(x){
+   QA2=x[1]
+   PA2=x[2]
+   
+   supplyR2= QA2- (regio2SI + region2SS*PA2) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandR2= QA2- ( region2DI+ region2DS*PA2) # QA-(250+2.5*p)
+   
+   return(c(supplyR2,demandR2))
+ }

#autarcky equlibrium price and quantity for region 3

fcnregion3=function(x){
+   QA3=x[1]
+   PA3=x[2]
+   
+   supplyR3= QA3- ( region3SI+ region3SS*PA3) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandR3= QA3- (region3DI + region3DS*PA3) # QA-(250+2.5*p)
+   
+   return(c(supplyR3,demandR3))
+ }

#autarcky equlibrium price and quantity for region 4

fcnregion4=function(x){
+   QA4=x[1]
+   PA4=x[2]
+   
+   supplyR4= QA4- ( region44SI + region4SS*PA4) # QA-(25+1.25*p) #R assumes this is equal to zero
+   demandR4= QA4- (region4DI + region4DS*PA4) # QA-(250+2.5*p)
+   
+   return(c(supplyR4,demandR4))
+ }

fncforworldprice= function(x){
+   QW= x[1]
+   PW = x[2]
+   
+   TotalExcessSupply = QW- (((region1SI + region1SS*PW) - (region1DI + region1DS*PW)) + ((regio2SI + region2SS*PW) - (region2DI+region2DS*PW)))
+   TotalExcessDemeand= QW- (((region3DI + region3DS*PW)- (region3SI+ region3SS*PW))+ ((region4DI + region4DS* PW)- (region44SI+region4SS*PW)))
+   #qw is total import for the region 3 and 4 in case of total excess demand
+   return(c(TotalExcessSupply,TotalExcessDemeand))
+   
+ }


library("rootSolve")
#to print region 1 autarcky price and quantity 
solnregion1= multiroot(fcnregion1, c(1,1))
QA1 = solnregion1$root[1];QA1
[1] 33.33333
PA1 = solnregion1$root[2];PA1
[1] 8.333333

#to print region 2 autarcky price and quantity
solnregion2= multiroot(fcnregion2, c(1,1))
QA2 = solnregion2$root[1];QA2
[1] 37.69231
PA2 = solnregion2$root[2];PA2
[1] 10.38462

#to print region 3 autarcky price and quantity
solnregion3= multiroot(fcnregion3, c(1,1))
QA3 = solnregion3$root[1];QA3
[1] 20.7647
PA3 = solnregion3$root[2];PA3
[1] 18.70588

#to print region 4 autarcky price and quantity
solnregion4= multiroot(fcnregion4, c(1,1))
QA4 = solnregion4$root[1];QA4
[1] 49.84211
PA4 = solnregion4$root[2];PA4
[1] 18.28947

# to print free trade price and quantity 
solnw = multiroot(fncforworldprice, c(1,1))
QW = solnw$root[1];QW
[1] 98.7155
PW = solnw$root[2];PW
[1] 12.37792

# qt supplied  for Each region 
A1 = (region1SI + region1SS*PW);A1
[1] 73.77919
A2 = (regio2SI + region2SS*PW);A2
[1] 51.64544
A3 = ( region3SI+ region3SS*PW);A3
[1] 4.944798
A4= (region44SI + region4SS*PW);A4
[1] 28.56051


# qt sdemanded for each region 
sA1= (region1DI + region1DS*PW);sA1
[1] 0.9766454
sA2 =( region2DI+ region2DS*PW);sA2
[1] 25.73248
sA3= (region3DI + region3DS*PW);sA3
[1] 58.73248
sA4 =(region4DI + region4DS*PW); sA4
[1] 73.48832

# change in region 1
ChPS.R1=region1SI*(PW-PA1) + (region1SS/2)*(PW**2-PA1**2);ChPS.R1
[1] 216.6129
ChCS.R1=  region1DI*(PA1-PW) + (region1DS/2)*(PA1**2-PW**2);ChCS.R1
[1] -69.38483

# change in region 2
ChPS.R2=regio2SI*(PW-PA2) + (region2SS/2)*(PW**2-PA2**2);ChPS.R2
[1] 89.03864
ChCS.R2=  region2DI*(PA2-PW) + (region2DS/2)*(PA2**2-PW**2);ChCS.R2
[1] -63.21244

# change in region 3
ChPS.R3=region3SI*(PW-PA3) + (region3SS/2)*(PW**2-PA3**2);ChPS.R3
[1] -81.34442
ChCS.R3= region3DI *(PA3-PW) + (region3DS/2)*(PA3**2-PW**2);ChCS.R3
[1] 251.5277

# change in region 4
ChPS.R4=region44SI*(PW-PA4) + (region4SS/2)*(PW**2-PA4**2);ChPS.R4
[1] -231.7407
ChCS.R4= region4DI*(PA4-PW) + (region4DS/2)*(PA4**2-PW**2);ChCS.R4
[1] 364.5373

#net change 
NchR1=ChPS.R1+ChCS.R1;NchR1 # net change for region 1
[1] 147.2281
NchR2=ChPS.R2+ChCS.R2;NchR2 # net change for region 2
[1] 25.82619
NchR3=ChPS.R3+ChCS.R3;NchR3 # net change for region 3
[1] 170.1832
NchR4=ChPS.R4+ChCS.R4;NchR4 #net change for region 4
[1] 132.7966

NchalR=NchR1+NchR2+NchR3+NchR4;NchalR # world net gain
[1] 476.0341

