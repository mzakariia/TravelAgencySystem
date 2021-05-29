
offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin, 79).

subSet([], []).
subSet([H|Tail], [H|Tail2]):-
  subSet(Tail, Tail2).
subSet([_|Tail], Tail2):-
  subSet(Tail, Tail2).
replace([],_,Acc,Acc).
replace([H|T],E,Acc,AccN):-
    H=activity(_),
    replace(T,E,Acc,Acc2),
    append([E],Acc2,AccN).
replace([H|T],E,Acc,AccN):-
    H\=activity(_),
    replace(T,E,Acc,Acc2),
    append([H],Acc2,AccN).
possibleSubset(L1,L2):-
    subSet(L1,L3),
    permutation(L3,L2).
choosePreferences(L1,L2):-
    subSet(L1,L3),
    member(activity(L),L3),
    subSet(L,L4),
    L4\=[],
    replace(L3,activity(L4),[],L2).
choosePreferences(L1,L2):-
    subSet(L1,L3),
    \+member(activity(_),L3),
    L2=L3.
preferenceSatisfaction(O,C,Cp,S):-
  O=offer(_,L,_,_,_,_,_,_),
  member(activity(L1),Cp),
  getActivitySum(C,L,L1,S1),
  getMeanSum(C,O,Cp,S2),
  getAccommodationSum(C,O,Cp,S3),
  S is S1+S2+S3.
preferenceSatisfaction(O,C,Cp,S):-
  \+member(activity(_),Cp),
  getMeanSum(C,O,Cp,S1),
  getAccommodationSum(C,O,Cp,S2),
  S is S1+S2.
getActivitySum(_,_,[],0).
getActivitySum(C,L,[H|T],S):-
  member(H,L),
  getActivitySum(C,L,T,S1),
  customerPreferredActivity(C,H,S2),
  S is S1+S2.
getActivitySum(C,L,[H|T],S):-
  \+member(H,L),
  getActivitySum(C,L,T,S).
getAccommodationSum(_,O,Cp,0):-
 \+member(accommodation(_),Cp);
  (member(accommodation(A),Cp),offerAccommodation(O,A1),A\=A1).
getAccommodationSum(C,O,Cp,S):-
  member(accommodation(A),Cp),
  offerAccommodation(O,A),
  customerPreferredAccommodation(C,A,S).
getMeanSum(_,O,Cp,0):-
 \+member(means(_),Cp);
 (member(means(M),Cp),offerMean(O,M1),M\=M1).
getMeanSum(C,O,Cp,S):-
  member(means(M),Cp),
  offerMean(O,M),
  customerPreferredMean(C,M,S).
overlapPeriod(P1,P2):-
  period(Y1-M1-D1,Y2-M2-D2)=P1,
  period(Y1b-M1b-D1b,Y2b-M2b-D2b)=P2,
  (((Y1b=Y1,M1b=M1,D1b>=D1,M2=M1b,D1b=<D2,Y1b=Y2);(Y1b=Y1,Y1b=Y2,M1b>M1,M1b<M2);(Y1b=Y1,Y1b=Y2,M1b>M1,M1b=M2,D1b=<D2);(Y1b=Y1,Y1b<Y2,M1b=M1,D1b>=D1);
   (Y1b=Y1,Y1b<Y2,M1b>M1);(Y1b>Y1,Y1b=Y2,M1b=M2,D1b=<D2);(Y1b>Y1,Y1b=Y2,M1b<M2);(Y1b>Y1,Y1b<Y2));
   ((Y1=Y1b,M1=M1b,D1>=D1b,M1=M2b,D1=<D2b,Y1=Y2b);(Y1=Y1b,Y1=Y2b,M1>M1b,M1<M2b);(Y1=Y1b,Y1=Y2b,M1>M1b,M1=M2b,D1=<D2b);(Y1=Y1b,Y1<Y2b,M1=M1b,D1>=D1b);
   (Y1=Y1b,Y1<Y2b,M1>M1b);(Y1>Y1b,Y1=Y2b,M1=M2b,D1=<D2b);(Y1>Y1b,Y1=Y2b,M1<M2b);(Y1>Y1b,Y1<Y2b))).
getOffer(ChosenPrefs,Offer):-
      getOfferHelper(ChosenPrefs,0,_,Offer).
getOfferHelper([],0,O,O):-
  offerMean(O,_).
getOfferHelper([],1,O,O).
getOfferHelper([H|T],1,O,Offer):-
  ((H=dest(D),offerMean(O1,_),O1=offer(D,_,_,_,_,_,_,_),O=O1);
  (H=means(M),offerMean(O,M));
  (H=accommodation(A),offerAccommodation(O,A));
  (H=period(Y1-M1-D1,Y2-M2-D2), O=offer(_,_,_,_,_,P,_,_),overlapPeriod(P,period(Y1-M1-D1,Y2-M2-D2)));
  (H=activity(A),O=offer(_,L,_,_,_,_,_,_),((A=[V],member(V,L));(activityMatch(A,L))));
  (H=budget(N),O=offer(_,_,C,_,_,_,_,_),N>=C)),
  getOfferHelper(T,1,O,Offer).
getOfferHelper([H|T],0,_,O):-
  (
  (H=dest(D),offerMean(Offer,_),Offer=offer(D,_,_,_,_,_,_,_));
  (H=means(M),offerMean(Offer,M));
  (H=accommodation(A),offerAccommodation(Offer,A));
  (H=period(_-_-_,_-_-_),offerMean(Offer,_),Offer=offer(_,_,_,_,_,P,_,_),overlapPeriod(P,H));
  (H=activity(A),offerMean(Offer,_),Offer=offer(_,L,_,_,_,_,_,_),((A=[H],member(H,L));(activityMatch(A,L))));
  (H=budget(N),offerMean(Offer,_),Offer=offer(_,_,C,_,_,_,_,_),N>=C)
  ),
  getOfferHelper(T,1,Offer,O).
activityMatch([],_).
activityMatch([H|T],L):-
  member(H,L),
  activityMatch(T,L).
recommendOfferForCustomer(Prefs,ChosenPrefs,O):-
  choosePreferences(Prefs,ChosenPrefs),
  ChosenPrefs\=[],
  getOffer(ChosenPrefs,O).
recommendOffer(L1, L2, Offer, CustomersChosen):-
  recommendOfferHelper(L1, L2,[], Offer, CustomersChosen).
recommendOfferHelper(L1,L2,Acc, Offer, CustomersChosen):-
  offerAccommodation(Offer,_),
  prefsHelper(L1,L2,Offer,[],L),
  Offer= offer(_,_,_,_,_,_,_,N),
  getCustomerList(L1,L,N,Acc,CustomersChosen).
getCustomerList(_,_,0,Acc,Acc).
getCustomerList(L1,L2,N,Acc,L):-
  N>0,
  max2(L1,L2,Acc,0,0,C),
  C\=0,
  append(Acc,[C],AccN),
  N1 is N-1,
  getCustomerList(L1,L2,N1,AccN,L).
getCustomerList(L1,L2,N,Acc,Acc):-
  N>0,
  max2(L1,L2,Acc,0,0,0).
prefsHelper([],[],_,Acc,Acc).
prefsHelper([H1|T1],[H|T],Offer,Acc,L):-
  preferenceSatisfaction(Offer,H1,H,S),
  append(Acc,[S],AccN),
  prefsHelper(T1,T,Offer,AccN,L).
max2([],[],_,_,C,C).
max2([H|T],[H1|T1],Acc,N,C,C1):-
  \+member(H,Acc),
  (H1>=N,max2(T,T1,Acc,H1,H,C1));
  (H1<N,max2(T,T1,Acc,N,C,C1)).
max2([H|T],[_|T1],Acc,N,C,C1):-
  member(H,Acc),
  max2(T,T1,Acc,N,C,C1).
getAllActivities(L):-
    setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).
mostPreferredActivity(C,A):-
  getAllActivities(L),
  activityHelper(C,L,0,_,A).
activityHelper(_,[],_,A,A).
activityHelper(C,[H|T],N,A,A1):-
  customerPreferredActivity(C,H,S),
  ((S>=N,activityHelper(C,T,S,H,A1));
  (S<N, activityHelper(C,T,N,A,A1))).
activityHelper(C,[H|T],N,A,A1):-
  \+customerPreferredActivity(C,H,_),
   activityHelper(C,T,N,A,A1).



















