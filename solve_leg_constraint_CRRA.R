#given gamma above, solve for trade war effort level, etw$root
E=.39
ei = 1/E
etw <- uniroot(function(cn) (8/49*(1+(8*(1 + ei*cn^E)-5)/(68-8*(1 + ei*cn^E)))*(8*63*.2*(ei*cn^(-.8)))/((68-8*(1 + ei*cn^E))^2))-1, lower=0, upper = 1, tol = 0.00001, maxiter = 1000)

# given trade war effort level, compute trade war tariff, 'ttw'
ttw = (8*(1 + ei*etw$root^E)-5)/(68-8*(1 + ei*etw$root^E))

#producer and consumer surpluses (in X and Y industries) and tariff revenue at trade war tariff
PSxtw = ((2 +2*ttw)^2)/49
CSxtw = .5*((3 -4*ttw)^2)/49
TRtw = (ttw - 6*ttw^2)/7
CSytw = ((3 +3*ttw)^2)/98
PSytw = ((4 -3*ttw)^2)/98

# discounting
d = .99 #legislature's discount rate
dl = .99 # lobby's discount rate
T=54
fd = (d -d^(T+1))/(1-d) #total discounting for leg
fdl = (dl -dl^(T+1))/(1-dl) #total discounting for lobby

ta = seq(.0756,.0758,.000001) #possible bribe values for legislator located at 0
ea = (E*((68*ta + 5)/(8+8*ta)-1))^ei
out = matrix(NA,length(ta),4) #pre-reserve space in output matrix

#producer and consumer surpluses (in X and Y industries) and tariff revenue at trade agreement tariff
PSxta = ((2 +2*ta)^2)/49
CSxta = .5*((3 -4*ta)^2)/49
TRta = (ta - 6*ta^2)/7
CSyta = ((3 +3*ta)^2)/98
PSyta = ((4 -3*ta)^2)/98

for (j in 1:length(ta)) {
  t = ta[j]
  
#legislature's incentive constraint, with all e's set to the break e (instead of e_a and e_tw)
f <- function (e) fd*(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j]) + (CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j]) - fd*(CSxtw + (1 + ei*e^E)*PSxtw + CSytw + PSytw + TRtw) - ((1 + ei*e^E)*((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + .5*((3 -4*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + ((8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)) - 6*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E))^2)/7 + CSyta[j] + PSyta[j])

#solve for effort level that makes legislature indifferent (\ov{e} in text)
ove = uniroot(f, lower = 0, upper = .1, tol = 0.00001, maxiter = 1000)

#This is benefit to lobby
bl = ((2 +2*(8*(1 + ei*ove$root^E)-5)/(68-8*(1 + ei*ove$root^E)))^2)/49 - PSxta +ea + fdl*(PSxtw - etw$root - PSxta +ea)
ove$root - bl #lobby's constraint (need this to be positive, as close to zero as possible)'

out[j,1] = t #I needed a loop counter in Matlab; may not need it in R
out[j,2] = ove$root
out[j,3] = ove$root - bl[j]
out[j,4] = T
}
View(out)