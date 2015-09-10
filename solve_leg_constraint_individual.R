#code for calculating leg and lobby constraint for the cut-off value
#(find from "out" created by solve_leg_constraint_CRRA.R)

j = 200
e = out[j,2] #e = 0.006976324 

fd*(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j])
fd = 3.524381
0.4739361
1.670331

(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j])
0.4739361

fd*(CSxtw + (1 + ei*e^E)*PSxtw + CSytw + PSytw + TRtw)
fd* 0.473849 = 1.670024

((1 + ei*e^E)*((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + .5*((3 -4*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + (8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E))/7 - 6/7*((8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2 + CSyta[j] + PSyta[j])
0.4742428

1.670331 + 0.4739361 - 1.670024 - 0.4742428

e - (((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 - PSxta[j] +ea[j] + fdl*(PSxtw - etw$root - PSxta[j] +ea[j]))
e 
((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 - PSxta[j] +ea[j] -e
fdl*(PSxtw - etw$root - PSxta[j] +ea[j]) 

ta=.049
ea = (E*((68*ta + 5)/(8+8*ta)-1))^ei


#additional code for calculating individual constraints
ta = .0756
ta = .07559
ea = (E*((68*ta + 5)/(8+8*ta)-1))^ei

PSxta = ((2 +2*ta)^2)/49 #producer and consumer surpluses (in X and Y industries) and tariff revenue at trade agreement tariff
CSxta = .5*((3 -4*ta)^2)/49
TRta = (ta - 6*ta^2)/7
CSyta = ((3 +3*ta)^2)/98
PSyta = ((4 -3*ta)^2)/98

j=1

#legislature's incentive constraint, with all e's set to the break e (instead of e_a and e_tw)
f <- function (e) fd*(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j]) + (CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j]) - fd*(CSxtw + (1 + ei*e^E)*PSxtw + CSytw + PSytw + TRtw) - ((1 + ei*e^E)*((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + .5*((3 -4*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + (8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E))/7 - 6/7*((8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2 + CSyta[j] + PSyta[j])
ove = uniroot(f, lower = 0, upper = .10, tol = 0.00001, maxiter = 1000) #solve for effort level that makes legislature indifferent (\ov{e} in text)

#This is benefit to lobby
b1 = ((2 +2*(8*(1 + ei*ove$root^E)-5)/(68-8*(1 + ei*ove$root^E)))^2)/49
b2 = - PSxta[j] +ea[j] + fdl*(PSxtw - etw$root - PSxta[j] +ea[j])

ove$root - b1 - b2

e=0
t1 = fd*(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j])
t2 = CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j]
t3 = fd*(CSxtw + (1 + ei*e^E)*PSxtw + CSytw + PSytw + TRtw)
t4 = ((1 + ei*e^E)*((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + .5*((3 -4*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + (8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E))/7 - 6/7*((8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2 + CSyta[j] + PSyta[j])
