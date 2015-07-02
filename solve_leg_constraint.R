#gamma: 1 + cn^.2

#given gamma above, solve for trade war effort level, etw$root
etw <- uniroot(function(cn) (8/49*(1+(8*(1.2 + cn^.2)-5)/(68-8*(1.2 + cn^.2)))*(8*63*.2*(cn^(-.8)))/((68-8*(1.2 + cn^.2))^2))-1, lower=0, upper = 1, tol = 0.00001, maxiter = 1000)
#c_n = .00145, ie. 'etw$root'

# given trade war effort level, compute trade war tariff, 'ttw'
ttw = (8*(1.2 + etw$root^.2)-5)/(68-8*(1.2 + etw$root^.2))


PSxtw = ((2 +2*ttw)^2)/49
CSxtw = .5*((3 -4*ttw)^2)/49
TRtw = (ttw - 6*ttw^2)/7
CSytw = ((3 +3*ttw)^2)/98
PSytw = ((4 -3*ttw)^2)/98

# given trade agreement tariff 'ta', compute trade agreement level of ex-post lobbying
ta = .06
#ea = ((60*ta - 3)/(8*(ta+1)))^5

PSxta = ((2 +2*ta)^2)/49
CSxta = .5*((3 -4*ta)^2)/49
TRta = (ta - 6*ta^2)/7
CSyta = ((3 +3*ta)^2)/98
PSyta = ((4 -3*ta)^2)/98

d = .9
T=50
fd = (d -d^(T+1))/(1-d)

#now have changed all e's to the break e (instead of e_a and e_tw)
f <- function (e) fd*(CSxta + (1.2 + e^.2)*PSxta + CSyta + PSyta + TRta) + (CSxta + (1.2 + e^.2)*PSxta + CSyta + PSyta + TRta) - fd*(CSxtw + (1.2 + e^.2)*PSxtw + CSytw + PSytw + TRtw) - ((1.2 + e^.2)*((2 +2*(8*(1.2 + e^.2)-5)/(68-8*(1.2 + e^.2)))^2)/49 + .5*((3 -4*(8*(1.2 + e^.2)-5)/(68-8*(1.2 + e^.2)))^2)/49 + ((8*(1.2 + e^.2)-5)/(68-8*(1.2 + e^.2)) - 6*(8*(1 + e^.2)-5)/(68-8*(1.2 + e^.2))^2)/7 + CSyta + PSyta)

#f1 <- function (e) fd*(CSxta + (1 + e^.2)*PSxta + CSyta + PSyta + TRta)
#f2 <- function (e) (CSxta + (1. + e^.2)*PSxta + CSyta + PSyta + TRta) 
#f3 <- function (e) fd*(CSxtw + (1 + e^.2)*PSxtw + CSytw + PSytw + TRtw)
#f4 <- function (e) (1 + e^.2)*((2 +2*(8*(1 + e^.2)-5)/(68-8*(1 + e^.2)))^2)/49 + .5*((3 -4*(8*(1 + e^.2)-5)/(68-8*(1 + e^.2)))^2)/49 + ((8*(1 + e^.2)-5)/(68-8*(1 + e^.2)) - 6*(8*(1 + e^.2)-5)/(68-8*(1 + e^.2))^2)/7 + CSyta + PSyta

ove = uniroot(f, lower = 0, upper = .1, tol = 0.00001, maxiter = 1000)

((2 +2*(8*(1.2 + 0.005284347^.2)-5)/(68-8*(1.2 + 0.005284347^.2)))^2)/49 - PSxta

(8*(1.2 + 0.005284347^.2)-5)/(68-8*(1.2 + 0.005284347^.2))

fdl*(PSxtw - etw$root - PSxta)

dl = .80
T=100
fdl = (dl -dl^(T+1))/(1-dl)
