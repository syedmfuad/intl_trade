#EXAM CODING 

#country one usa for import oil : 

#Q-P
dus1=100 #demand intercept for country usa oil  
dus2=-8 #demand slope for usa oil import 

sus1=-50 #supply Intercept for import usa oil
sus2=10 #supply slope for import usa oil

#country two export country saudi arabia of oil:

#Q-P
dsa3=100 #demand intercept for country two saudi arabia 
dsa4=-6 #demand slope for export country saudi arabia 

ssa3=-35 #supply intercpet for country two for saudi of oil 
ssa4=7 #supply slope for country two for saudi of oil

# : rest of the world export countries of oil 

#Q-P
dre5=133 #demand  for all rest countries 
dre6=-6 #demand slope for all rest countries 
  
sre5=-26 #supply for all rest countries 
sre6=2.5 #supply slope for all rest countries 
  
 

##Autarky Equilibrium Quentity and Price  

#Autarky Equilibrium Quentity  and Price usa

fcnSDforus=function(x){
  Qus1=x[1]
  Pus1=x[2]
  
  sus= Qus1- (sus1 + sus2*Pus1) # Qus1-(-50+10*p)
  dus= Qus1- (dus1 + dus2*Pus1) #Qus1-(100-8*p)
  
  return(c(sus,dus))
}

#Autarky Equilibrium Quentity  and Price saudi arabia 
fcnSDforsa=function(x){
  
  Qsa2 =x[1]
  Psa2 =x[2]
  
  ssa = Qsa2- (ssa3 + ssa4*Psa2) #Qsa2-(-35+7*p)
  dsa = Qsa2- (dsa3 + dsa4*Psa2) #Qsa2-(100-6p)
  return(c(ssa,dsa))
}

#Autarky Equilibrium Quentity  and Price rest of the world
fcnSDforre=function(x){
  
  Qre3 =x[1]
  Pre3 =x[2]
  
  sre = Qre3- (sre5 + sre6*Pre3) #Qre3-(-26+2.5*p)
  dre = Qre3- (dre5 + dre6*Pre3) #Qre3-(133-6*p)
  return(c(sre,dre))
}


##Freetrade Quentity  and Price no tariff
fncforfreetrade= function(x){
  FtQ = x[1]
  FtP = x[2]
  
  ED1 = FtQ - (sus1 + sus2*FtP) - (dus1 + dus2*FtP) #Sum ED (QD-QS) for all importers (us).
  ES1 = FtQ - (((ssa3 + ssa4*FtP) - (dsa3 + dsa4*FtP)) + ((sre5 + sre6*FtP) - (dre5 + dre6*FtP))) #Sum ES (QS-QD)for all exporters (sa and re)
  return(c(ED1,ES1))
}


library("rootSolve")
solnus = multiroot(fcnSDforus, c(1,1))
Qus1 = solnus$root[1];Qus1
Pus1 = solnus$root[2];Pus1

solnsa = multiroot(fcnSDforsa, c(1,1))
Qsa2 = solnsa$root[1];Qsa2
Psa2 = solnsa$root[2];Psa2

solnre = multiroot(fcnSDforre, c(1,1))
Qre3 = solnre$root[1];Qre3
Pre3 = solnre$root[2];Pre3


solnfreetrade = multiroot(fncforfreetrade, c(1,1))
FtQ = solnfreetrade$root[1];FtQ
FtP = solnfreetrade$root[2];FtP

#quantity demanded for countries


#demanded for  countires 
Qdus = (dus1 + dus2*FtP);Qdus
Qdsa = (dsa3 + dsa4*FtP);Qdsa
Qdre = (dre5 + dre6*FtP);Qdre


#supplied for countries
Qsus = (sus1 + sus2*FtP);Qsus
Qssa = (ssa3 + ssa4*FtP);Qssa
Qsre = (sre5 + sre6*FtP);Qsre


#Welfare Analyis

##Change in procucer and consumer surplus from free trade

#country 1 us 
Cps.1 = sus1*(FtP - Pus1) + (sus2/2)*(FtP**2 - Pus1**2);Cps.1 #us change in producer surplus (Cps.1)
Ccs.1 = dus1*(Pus1 - FtP) + (dus2/2)*(Pus1**2 - FtP**2);Ccs.1 #us chagne in consumer surplus (Ccs.1)

#sa
Cps.2 = ssa3*(FtP - Psa2) + (ssa4/2)*(FtP**2 - Psa2**2);Cps.2 #sa Change in producer surplus (Cps.2)
Ccs.2 = dsa3*(Psa2 - FtP) + (dsa4/2)*(Psa2**2 - FtP**2);Ccs.2 #sa change in consumer surplus (Ccs.2)

#re
Cps.3 = sre5*(FtP - Pre3) + (sre6/2)*(FtP**2 - Pre3**2);Cps.3 #rest Change in producer surplus (Cps.3)
Ccs.3 = dre5*(Pre3 - FtP) + (dre6/2)*(Pre3**2 - FtP**2);Ccs.3 #rest change in consumer surplus (Ccs.3)



## T c in p s f f t

nCps.1 = Cps.1 + Ccs.1;nCps.1 #net c in p s in us
nCps.2 = Cps.2 + Ccs.2;nCps.2 #net c in p s in sa
nCps.3 = Cps.3 + Ccs.3;nCps.3 #net c in p s in re

nCpsft = nCps.1 + nCps.2 + nCps.3;nCpsft #total c in p s f f t

## T c in c s f f t

nCcs.1 = Ccs.1 + Cps.1;nCcs.1 #net c in p s in us
nCcs.2 = Ccs.2 + Cps.2;nCcs.2 #net c in p s in sa
nCcs.3 = Ccs.3 + Cps.3;nCcs.3 #net c in p s in re

nCcsft = nCcs.1 + nCcs.2 + nCcs.3 ;nCcsft #t c in c s f f t

# import tariff on oil us from saudi arabia 

##now we will add tariff on oil to us

##freetrade Q and P
fncforfreetrade= function(x) {
  FtQ = x[1]
  FtP = x[2]
  
  tariFtp=(Ftp+5)# if Tariff is 5$
  ED1 = FtQ - (sus1 + sus2*tariFtp) - (dus1 + dus2*tariFtp) #Sum ED (QD-QS) for all importers (us).
  ES1 = FtQ - (((ssa3 + ssa4*FtP) - (dsa3 + dsa4*FtP)) + ((sre5 + sre6*FtP) - (dre5 + dre6*FtP))) #Sum ES (QS-QD)for all exporters (sa and re)
  return(c(ED1,ES1))
}

# solutions
library("rootSolve")
solnus = multiroot(fcnSDforus, c(1,1))
Qus1 = solnus$root[1];Qus1
Pus1 = solnus$root[2];Pus1

solnsa = multiroot(fcnSDforsa, c(1,1))
Qsa2 = solnsa$root[1];Qsa2
Psa2 = solnsa$root[2];Psa2

solnre = multiroot(fcnSDforre, c(1,1))
Qre3 = solnre$root[1];Qre3
Pre3 = solnre$root[2];Pre3


solnfreetrade = multiroot(fncforfreetrade, c(1,1))
FtQ = solnfreetrade$root[1];FtQ
FtP = solnfreetrade$root[2];FtP

#quantity demanded for countries


#demanded for  countires 
Qdus = (dus1 + dus2*FtP);Qdus
Qdsa = (dsa3 + dsa4*FtP);Qdsa
Qdre = (dre5 + dre6*FtP);Qdre


#supplied for countries
Qsus = (sus1 + sus2*FtP);Qsus
Qssa = (ssa3 + ssa4*FtP);Qssa
Qsre = (sre5 + sre6*FtP);Qsre

#Welfare Analyis

##Change in procucer and consumer surplus from free trade

#country 1 us 
Cps.1 = sus1*(FtP - Pus1) + (sus2/2)*(tariFtp**2 - Pus1**2);Cps.1 #us change in producer surplus (Cps.1)
Ccs.1 = dus1*(Pus1 - FtP) + (dus2/2)*(Pus1**2 - tariFtp**2);Ccs.1 #us chagne in consumer surplus (Ccs.1)

#sa
Cps.2 = ssa3*(FtP - Psa2) + (ssa4/2)*(FtP**2 - Psa2**2);Cps.2 #sa Change in producer surplus (Cps.2)
Ccs.2 = dsa3*(Psa2 - FtP) + (dsa4/2)*(Psa2**2 - FtP**2);Ccs.2 #sa change in consumer surplus (Ccs.2)

#re
Cps.3 = sre5*(FtP - Pre3) + (sre6/2)*(FtP**2 - Pre3**2);Cps.3 #rest Change in producer surplus (Cps.3)
Ccs.3 = dre5*(Pre3 - FtP) + (dre6/2)*(Pre3**2 - FtP**2);Ccs.3 #rest change in consumer surplus (Ccs.3)



## T c in p s f f t

nCps.1 = Cps.1 + Ccs.1;nCps.1 #net c in p s in us
nCps.2 = Cps.2 + Ccs.2;nCps.2 #net c in p s in sa
nCps.3 = Cps.3 + Ccs.3;nCps.3 #net c in p s in re

nCpsft = nCps.1 + nCps.2 + nCps.3;nCpsft #total c in p s f f t

## T c in c s f f t

nCcs.1 = Ccs.1 + Cps.1;nCcs.1 #net c in p s in us
nCcs.2 = Ccs.2 + Cps.2;nCcs.2 #net c in p s in sa
nCcs.3 = Ccs.3 + Cps.3;nCcs.3 #net c in p s in re

nCcsft = nCcs.1 + nCcs.2 + nCcs.3 ;nCcsft #t c in c s f f 

