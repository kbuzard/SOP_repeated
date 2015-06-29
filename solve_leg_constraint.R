#gamma: 1.25 + cn^.2


#given gamma above, solve for trade war effort level, etw$root
etw <- uniroot(function(cn) (8/49*(1+(8*(1.25 + cn^.2)-5)/(68-8*(1.25 + cn^.2)))*(8*63*.2*(cn^(-.8)))/((68-8*(1.25 + cn^.2))^2))-1, lower=0, upper = 1, tol = 0.00001, maxiter = 1000)
#c_n = .00166, ie. 'etw$root'

# given trade war effort level, compute trade war tariff, 'ttw'
ttw = (8*(1.25 + etw$root^.2)-5)/(68-8*(1.25 + etw$root^.2))

# given trade agreement tariff 'ta', compute trade agreement level of ex-post lobbying
ta = .10
ea = ((58*ta - 5)/(8*(ta+1)))^5


PSxta = ((2 +2*ta)^2)/49
CSxta = .5*((3 -4*ta)^2)/49
TRta = (ta - 6*ta^2)/7
CSyta = ((3 +3*ta)^2)/98
PSyta = ((4 -3*ta)^2)/98

PSxtw = ((2 +2*ttw)^2)/49
CSxtw = .5*((3 -4*ttw)^2)/49
TRtw = (ttw - 6*ttw^2)/7
CSytw = ((3 +3*ttw)^2)/98
PSytw = ((4 -3*ttw)^2)/98

tb = (8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2))

Wttw = CSxta + (1.25 + etw$root^.2)*PSxta + CSyta + PSyta + TRta

f <- function (e) CSxta + (1.25 + ea^.2)*PSxta + CSyta + PSyta + TRta + (CSxta + (1.25 + e^.2)*PSxta + CSyta + PSyta + TRta) - (CSxtw + (1.25 + etw$root^.2)*PSxtw + CSytw + PSytw + TRtw) - ((1.25 + e^.2)*((2 +2*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)))^2)/49 + .5*((3 -4*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)))^2)/49 + ((8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)) - 6*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2))^2)/7 + CSyta + PSyta)

f1 <- function (e) CSxta + (1.25 + ea^.2)*PSxta + CSyta + PSyta + TRta 
f2 <- function (e) (CSxta + (1.25 + e^.2)*PSxta + CSyta + PSyta + TRta) 
f3 <- function (e) (CSxtw + (1.25 + etw$root^.2)*PSxtw + CSytw + PSytw + TRtw)
f4 <- function (e) (1.25 + e^.2)*((2 +2*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)))^2)/49 + .5*((3 -4*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)))^2)/49 + ((8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2)) - 6*(8*(1.25 + e^.2)-5)/(68-8*(1.25 + e^.2))^2)/7 + CSyta + PSyta


uniroot(f, lower = 0, upper = .1, tol = 0.00001, maxiter = 1000)