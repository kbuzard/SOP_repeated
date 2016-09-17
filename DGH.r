#Solve for optimal lobby effort under DGH97-style model with obj fcn W + e

#reserve space for loop output
tau = seq(0.001,.166,0.001) #this will be counter variable in loop
PSx = matrix(NA,length(tau),1)
CSx = matrix(NA,length(tau),1)
TR = matrix(NA,length(tau),1)
PSy = matrix(NA,length(tau),1)
CSy = matrix(NA,length(tau),1)

#calculate producer surplus, consumer surplus, tariff revenue for each possible
#value of the tariff on the grid (just above zero to prohibitive tariff 1/6)
for (j in 1:length(tau)) {
  t = tau[j]

  PSx[j] = ((2 +2*t)^2)/49
  CSx[j] = .5*((3 -4*t)^2)/49
  TR[j] = (t - 6*t^2)/7
  CSy[j] = ((3 +3*t)^2)/98
  PSy[j] = ((4 -3*t)^2)/98
}

#Calculations for when lobby has all the bargaining power

#calculate government welfare when tau = 0 (baseline)
b = ((2 +2*0)^2)/49 + .5*((3 -4*0)^2)/49 + (0 - 6*0^2)/7 + ((3 +3*0)^2)/98 + ((4 -3*0)^2)/98
W = PSx + CSx + TR + CSy + PSy  #social welfare
e = ((b - W)/PSx)^5             #gov't indifference condition when WG = W + e
pi = PSx - e                    #net profits

value = max(pi) #the value at which profits are maximized (over non-negative values)
ind = which.max(pi) #the location at which profits are maximized
RC = arrayInd(ind,c(dim(pi),dim(pi))) #row/column version of maximand location


#Calculations for when government has all the bargaining power
bl = ((2 +2*0)^2)/49           #baseline for lobby: profits when tau = 0
e = PSx - bl                   #effort level giving all excess profits over tau=0 to gov't

g = e^1.2                       #little g(e) function to add to social welfare
G = W + g                      #gov't welfare a la DGH97
plot(G)

value = max(G) #the value at which profits are maximized (over non-negative values)
ind = which.max(G) #the location at which profits are maximized
RC = arrayInd(ind,c(dim(G),dim(G))) #row/column version of maximand location