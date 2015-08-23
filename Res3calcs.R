E = .35
de = .95
ttw = .1213
etw =  0.006002821
ta = .1022411
optT = 4

j = 412
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
((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 - PSxta[j] +ea[j] = 0.006807572
fdl*(PSxtw - etw$root - PSxta[j] +ea[j]) = 0.0001687454

#-------------------------------
E = .36
de = .95
ttw = .11404
etw = .00566
ta = .0935867
optT = 4

j = 868
e = out[j,2] #e = 0.006957286 

fd*(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j])
fd = 3.524381
0.4701646
1.657039

(CSxta[j] + (1 + ei*e^E)*PSxta[j] + CSyta[j] + PSyta[j] + TRta[j])
0.4701646

fd*(CSxtw + (1 + ei*e^E)*PSxtw + CSytw + PSytw + TRtw)
fd* 0.4700566 = 1.656659

((1 + ei*e^E)*((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + .5*((3 -4*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 + (8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E))/7 - 6/7*((8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2 + CSyta[j] + PSyta[j])
0.4705449

1.670331 + 0.4739361 - 1.670024 - 0.4742428
1.657039 + 0.4701646 - 1.656659 - 0.4705449

e - (((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 - PSxta[j] +ea[j] + fdl*(PSxtw - etw$root - PSxta[j] +ea[j]))
e 
((2 +2*(8*(1 + ei*e^E)-5)/(68-8*(1 + ei*e^E)))^2)/49 - PSxta[j] +ea[j] = 0.006807572
fdl*(PSxtw - etw$root - PSxta[j] +ea[j]) = 0.0001687454