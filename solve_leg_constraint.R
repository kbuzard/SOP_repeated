#given gamma above, solve for trade war effort level, etw$root
etw <- uniroot(function(cn) (8/49*(1+(8*(1 + cn^.3)-5)/(68-8*(1 + cn^.3)))*(8*63*.2*(cn^(-.8)))/((68-8*(1 + cn^.3))^2))-1, lower=0, upper = 1, tol = 0.00001, maxiter = 1000)

# given trade war effort level, compute trade war tariff, 'ttw'
ttw = (8*(1 + etw$root^.3)-5)/(68-8*(1 + etw$root^.3))

#producer and consumer surpluses (in X and Y industries) and tariff revenue at trade war tariff
PSxtw = ((2 +2*ttw)^2)/49
CSxtw = .5*((3 -4*ttw)^2)/49
TRtw = (ttw - 6*ttw^2)/7
CSytw = ((3 +3*ttw)^2)/98
PSytw = ((4 -3*ttw)^2)/98

# total discounting for legislature
d = .99
T=100
fd = (d -d^(T+1))/(1-d)

# trade agreement tariff
ta = .052

#producer and consumer surpluses (in X and Y industries) and tariff revenue at trade agreement tariff
PSxta = ((2 +2*ta)^2)/49
CSxta = .5*((3 -4*ta)^2)/49
TRta = (ta - 6*ta^2)/7
CSyta = ((3 +3*ta)^2)/98
PSyta = ((4 -3*ta)^2)/98

#legislature's incentive constraint, with all e's set to the break e (instead of e_a and e_tw)
f <- function (e) fd*(CSxta + (1 + e^.3)*PSxta + CSyta + PSyta + TRta) + (CSxta + (1 + e^.3)*PSxta + CSyta + PSyta + TRta) - fd*(CSxtw + (1 + e^.3)*PSxtw + CSytw + PSytw + TRtw) - ((1 + e^.3)*((2 +2*(8*(1 + e^.3)-5)/(68-8*(1 + e^.3)))^2)/49 + .5*((3 -4*(8*(1 + e^.3)-5)/(68-8*(1 + e^.3)))^2)/49 + ((8*(1 + e^.3)-5)/(68-8*(1 + e^.3)) - 6*(8*(1 + e^.3)-5)/(68-8*(1 + e^.3))^2)/7 + CSyta + PSyta)

#solve for effort level that makes legislature indifferent (\ov{e} in text)
ove = uniroot(f, lower = 0, upper = .1, tol = 0.00001, maxiter = 1000)

#discount term for lobby. length of punishment is same as for leg, but delta can be different
dl = .80
fdl = (dl -dl^(T+1))/(1-dl)

#This is benefit to lobby
bl = ((2 +2*(8*(1 + ove$root^.3)-5)/(68-8*(1 + ove$root^.3)))^2)/49 - PSxta + fdl*(PSxtw - etw$root - PSxta)

#lobby's constraint (need this to be negative, as close to zero as possible)'
ove$root - bl