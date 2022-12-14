#NLP

#m  U      U=(Xd)^gamma (Yd)^1-gamma
maxUfnc = function(inputs){
  xd = inputs[1]
  yd = inputs[2]
  ret = xd^gamma * yd^(1-gamma) ##gamma = g
  return(signf*ret)
}

#cons
#PPF  m c c

fcnConst = function(inputs){
  xd = inputs[1]
  yd = inputs[2]
  xs = inputs[3]
  ys = inputs[4]
  
  f1 = C - (eta*xs^p + (1-eta)*(ys^p))^(1/p) #PPF c=(eta(Xs)^phi + (1-eta)(Ys)^phi))^1/phi 
  f2 = (xd - xs) #Market clearing condition Xd=Xs
  f3 = (yd - ys) #Market clearing condition Yd=Ys
  return(c(f1,f2,f3))
  #mc  
}


#params

gamma = 0.3 #gamma
eta = 0.5 #eta
p = 5   #phi
C = 100

library("alabama")
stval = c(2,2,2,2)
signf = -1 # because 'auglag' is a minimization algorithm
solmax = auglag(par = stval, fn = maxUfnc, heq = fcnConst)
solinput1 = solmax$par;solinput1
X =solinput1[1]; X
Y = solinput1[2]; Y
max = -1*solmax$value;max
Pratio = gamma/(1-gamma)*Y/X; Pratio
#maxopt = -1*solmax$output;maxopt


# MCP #

#params

g = 0.3 #gamma
eta = 0.5 #eta
p = 5   #phi
c = 100



#max U
fcnumax = function(input){
  xd = input[1]
  yd = input[2]
  xs = input[3]
  ys = input[4]
  lam1 = input[5]
  lam2 = input[6]
  lam3 = input[7]
  
  ##system of FOCs to solve
  focxd = g*(xd^(g-1))*(yd^(1-g)) - lam2
  focyd =   (xd^g)*(1-g)*(yd^(-g)) - lam3
  focxs = -(1/p)*lam1*((eta*(xs^p) + (1-eta)*(ys^p))^((1/p)-1)) * (eta*p*(xs^(p-1))) + lam2
  focys = -(1/p)*lam1*((eta*(xs^p) + (1-eta)*(ys^p))^((1/p)-1)) * ((1-eta)*p*(ys^(p-1))) + lam3
  foclam1 = c - (eta*(xs^p) + (1-eta)*(ys^p))^(1/p)
  foclam2 = xs - xd
  foclam3 = ys - yd
  
  return(c(focxd,focyd,focxs,focys,foclam1,foclam2,foclam3))
}

library("rootSolve")
stval = c(100,100,90,90,1,1,1) 
solUmax = multiroot(fcnumax, stval)
solinput = solUmax$root
xdstar = solinput[1];xdstar
ydstar = solinput[2];ydstar
xsstar = solinput[3];xsstar
ysstar = solinput[4];ysstar
lam1star = solinput[5];lam1star
lam2star = solinput[6];lam2star
lam3star = solinput[7];lam3star
Pratio = gamma/(1-gamma)*Y/X; Pratio


##Q 3

#NLP

#m U U=(Xd)^gamma (Yd)^1-gamma
maxUfnc = function(inputs){
  xd = inputs[1]
  yd = inputs[2]
  ret = xd^gamma * yd^(1-gamma) ##gamma = g
  return(signf*ret)
}

#cons
#PPF  ma c c

fcnConst = function(inputs){
  xd = inputs[1]
  yd = inputs[2]
  xs = inputs[3]
  ys = inputs[4]
  
  f1 = C - (eta*xs^p + (1-eta)*(ys^p))^(1/p) #PPF c=(eta(Xs)^phi + (1-eta)(Ys)^phi))^1/phi   
  f2 = (Px*xs + Py*ys - (Px*xd + Py*yd))
  return(c(f1,f2))
}


#params

g = 0.3 #gamma
eta = 0.5 #eta
p = 5   #phi
c = 100
Px = 1 #world price  x
Py = 1 #world price  y

library("alabama")
stval = c(2,2,2,2)
signf = -1 # because 'auglag' is a minimization algorithm
solmax = auglag(par = stval, fn = maxUfnc, heq = fcnConst)
solinput1 = solmax$par;solinput1
max = -1*solmax$value;max
#maxopt = -1*solmax$output;maxopt
