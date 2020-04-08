event(e001, s001, 1030, 18).
event(e002, s001, 1130, 24).
event(e003, s001, 1230, 30).
event(e004, s001, 1330, 34).
event(e005, s001, 1430, 24).
event(e006, s001, 1530, 34).
%event(e007, s001, 1630, 45).

%test case
event(e001, s001, 1030, 36).
% test case

event(e008, s002, 1030, 18).
event(e009, s002, 1130, 24).
event(e010, s002, 1230, 24).
event(e011, s002, 1330, 18).
event(e012, s002, 1430, 24).
event(e013, s002, 1530, 34).
event(e014, s002, 1630, 34).

event(e001, s003, 1030, 30). %humidity
event(e002, s003, 1130, 25).
event(e003, s003, 1230, 40).
event(e004, s003, 1330, 50).
event(e005, s003, 1430, 60).
event(e006, s003, 1530, 80).


featureByActuator(temperature,[a001,a004]).
featureByActuator(humidity,[a009,a003]).

actuator(a001, [increase, decrease], thermostat, a1).

actuator(a004, [increase, decrease], thermostat, a5).

actuator(a002, [on, off], alarm, a5).

actuator(a003, [on, off], alarm, a1).

actuator(a005, [lock, unlock], door, a1).

actuator(a006, [lock, unlock], window, a1).

actuator(a007, [lock, unlock], door, a5).

actuator(a008, [lock, unlock], window, a5).

actuator(a009, [increase, decrease], humidifier, a1).

actuator(a010, [increase, decrease], humidifier, a5).

actuator(a011, [on, off], light, a1).

actuator(a012, [on, off], light, a5).


rule(r001, s001, moreThan, 15, a001, increase, 10). % test purpose
rule(r002, s001, lessThan, 35, a001, decrease, 10). % test purpose

%rule(r001, s001, lessThan, 15, a001, increase, 10). %commented out for testing
%rule(r002, s001, moreThan, 35, a001, decrease, 10). %commented out for testing
rule(r003, s002, moreThan, 35, a004, decrease, 10).
rule(r004, s002, lessThan, 15, a004, increase, 10).
rule(r005, s005, equalsTo, 1, a003, equalsTo, 1).

rule(r006, s003, lessThan, 35, a009, increase, 10).
rule(r007, s003, moreThan, 70, a009, decrease, 10).
%rule(r008, s003, moreThan, 72, a009, decrease, 10).

controller(c001,[a001, a002, a003, s001, s002]). % the sensor values are arbitray. make it reasonable
controller(c002, [a004, a005,s002, s003,a009]). % the sensor values are arbitray. make it reasonable




isDuplicate(L) :-
    select(V, L, L1),
    member(V, L1).
isMultipleElement([],0).
isMultipleElement([_|T],C):-
   isMultipleElement(T,C1),  C is C1+1.

findController(_,[],[]).

findController(X,[H|T],L):-
    controller(H, L1), not(member(X, L1)),
    findController(X, T, L).

findController(X,[H|T],[H|T1]):-
    controller(H, L1), member(X, L1),
    findController(X, T, T1).
    

%accessedByMulController(a002,L)
%accessedByMulController(a001)
	
%eventSubExpression(event1, operation, event2, allowabletimeLimit).
 checkMultController(L):- isMultipleElement(L,C), C >1.

accessedByMulController(X):-
    findall(A, controller(A,_), MyL),
    findController(X,MyL,L), checkMultController(L).

% conflictC1Count([a001],C)
% 
conflictC1Count([],A,A).
conflictC1Count([H|T],Acc,AccF):-not(accessedByMulController(H)),
    	conflictC1Count(T,Acc,AccF).
conflictC1Count([H|T],Acc,AccF):-accessedByMulController(H),
    	AccT is Acc+1, conflictC1Count(T,AccT,AccF).

%findActuator([s001],L)
findActuator([],A,A).
findActuator([H|T],Acc, AccF):- findall(X,rule(_,H,_,_,X,_,_),L1),list_setB(L1,Xs), % find actuators that triggers with the event from sensor H
    							 conflictC1Count(Xs,Acc,AccT),
    							 findActuator(T,AccT,AccF).

%findall(X,rule(_,s001,_,_,X,_,_),L1),list_setB(L1,Xs)

findSensor([],A,A).
findSensor([H|T],Acc,AccF):-  findall(X,event(_,X,H,_),L1), list_setB(L1,Xs), % H is the time
    							findActuator(Xs,Acc, AccTemp), % send the sensor list that is made based on time H
    							findSensor(T,AccTemp, AccF).

conflictC1(Acc,AccF):-findall(X, event(_,_,X,_),L1),list_setB(L1,Xs),findSensor(Xs,Acc,AccF).


dependentFeature(temperature, humidity).
dependentFeature(humidity, abc).
dependentFeature(smoke, co2).

isDependentFeature(Feature, Dependent) :- Feature == Dependent.

isDependentFeature(Feature, Dependent) :-
        dependentFeature(Feature, Dependent).
isDependentFeature(Feature, Dependent) :-
        dependentFeature(Dependent,Feature).
isDependentFeature(Feature, Dependent) :-
        (dependentFeature(Dependent, X)),
        isDependentFeature(X, Feature).
isDependentFeature(Feature, Dependent) :-
        (dependentFeature(Feature, X)),
        isDependentFeature(X, Dependent).
		
% actuator(a001, [increase, decrease], thermostat, a1).

whatRelation(RuleValue, SensorValue, equalTo):-
    RuleValue == SensorValue.

whatRelation(RuleValue, SensorValue, moreThan):-
    RuleValue < SensorValue.

%whatRelation(RuleValue, SensorValue, moreOrEqual):-
%    RuleValue =< SensorValue.

whatRelation(RuleValue, SensorValue, lessThan):-
    RuleValue > SensorValue.

%whatRelation(RuleValue, SensorValue, lessOrEqual):-
%    RuleValue >= SensorValue.
		
isSameLocation(H1,H2):- actuator(H1,_,_,X),actuator(H2,_,_,X).
		
isDependentFeatureByActuator(H1,H2):- isSameLocation(H1,H2), featureByActuator(X,L1),member(H1,L1), 
    featureByActuator(Y,L2),member(H2,L2),
    (X==Y ; isDependentFeature(X,Y)).

isSameController(H1,H2):- controller(A,L1),member(H1,L1), controller(B,L2),member(H2,L2), A==B.

conflictC2Count([],_,[], Acc, Acc).
conflictC2Count([],_,[H1|T],Acc,AccF):- conflictC2Count(T,H1,T,Acc,AccF).

conflictC2Count([H2|Tsmall],H1, T,Acc,AccF):-isSameController(H1,H2),  % this is for no conflict. skipping
    	not(isDependentFeatureByActuator(H1,H2)),
    	conflictC2Count(Tsmall,H1,T,Acc,AccF).

conflictC2Count([H2|Tsmall],H1, T,Acc,AccF):-isSameController(H1,H2),  % this is for no conflict. skipping
    	isDependentFeatureByActuator(H1,H2),
    	conflictC2Count(Tsmall,H1,T,Acc,AccF).

conflictC2Count([H2|Tsmall],H1, T,Acc,AccF):-not(isSameController(H1,H2)),  % this is for no conflict. skipping
    	not(isDependentFeatureByActuator(H1,H2)),
    	conflictC2Count(Tsmall,H1,T,Acc,AccF).

conflictC2Count([H2|Tsmall],H1,T, Acc,AccF):-not(isSameController(H1,H2)), 
    	isDependentFeatureByActuator(H1,H2),
    	AccT is Acc+1, conflictC2Count(Tsmall,H1,T,AccT,AccF).

%conflictC2CountOuter([],[H1|T],Acc,AccF):- conflictC2Count(T,H1,T,Acc,AccF).
conflictC2CountOuter([H1|T],Acc,AccF):- conflictC2Count(T,H1,T,Acc,AccF).

% conflictC2Count([a001,a004,a009],[a001,a004,a009],0,A)
% conflictC2CountOuter([a001,a004,a009],0,A)
% rule(r003, s002, moreThan, 35, a004, decrease, 10).
findActuatorC2([],_, A,A).
findActuatorC2([H|T],L2, Acc, AccF):-  findall(X,rule(_,H,_,_,X,_,_),L1), % find actuators that triggers with the event from sensor H
    							append(L1,L2,L3),
    							findActuatorC2(T,L3,Acc,AccT),  list_setB(L3,Xs),
      							conflictC2CountOuter(Xs,AccT,AccF).

findSensorC2([],A,A).
findSensorC2([H|T],Acc,AccF):- findall(X,event(_,X,H,_),L1),
    							findActuatorC2(L1, [], Acc, AccTemp),% list of sensor L1, H is time
    							findSensorC2(T,AccTemp, AccF).
conflictC2(Acc,AccF):-findall(X, event(_,_,X,_),L1),findSensorC2(L1,Acc,AccF).

%event(e012, s002, 1430, 24).
%rule(r001, s001, lessThan, 15, a001, increase, 10).

len([],0). 
len([_|T],N)  :-  len(T,X),  N  is  X+1.

confictA1Count([],_,A,A).

confictTryCount([H|T], PreviousLength, SensorValue, Acc, AccF):- len(T,L2), Length is L2+1, 
    											PreviousLength == Length,
    											rule(H,_,Relation1,RuleValue,_,_,_),
												whatRelation(RuleValue, SensorValue, Relation2), 
												Relation1 \== Relation2,  % this rule is not triggered
    											Length2 is Length-1,
												confictTryCount(T, Length2, SensorValue, Acc, AccF).

confictTryCount([H|T], PreviousLength, SensorValue, Acc, AccF):- len(T,L2), Length is L2+1, 
    										    PreviousLength == Length,
    											rule(H,_,Relation1,RuleValue,_,_,_),
												whatRelation(RuleValue, SensorValue, Relation2), 
												Relation1 == Relation2,Length2 is Length-1,
												confictTryCount(T, Length2, SensorValue, Acc, AccF).

confictTryCount([H|T], PreviousLength, SensorValue, Acc, AccF):- len(T,L2), Length is L2+1, 
    											PreviousLength> Length,
    											rule(H,_,Relation1,RuleValue,_,_,_),
												whatRelation(RuleValue, SensorValue, Relation2), 
												 Relation1 == Relation2, AccT is Acc+1,
												confictTryCount(T, Length, SensorValue, AccT, AccF).
%findTriggeredActuators([H|T], SensorValue, Acc, AccF):- findall(X,rule(X,H, _, _, _, _, _),L1) , % find rules that is triggered with the sensor measurement
														

findActuatorTry([], A, A).
findActuatorTry([H|T], Time, Acc, AccF):- findall(X,rule(X,H,_,_,_,_,_),L1), % find rules that can trigger with the event from sensor H
												len(L1,L),
												event(_,H,Time,SensorValue),
    							 				confictA1Count(L1,L, SensorValue, Acc,AccT),
    							 				findActuatorA1(T,Time, AccT,AccF).

findSensorTry([],A,A).
findSensorTry([H|T],Acc,AccF):- findall(X,event(_,X,H,_),L1), % L1 is the list of sensor associated with time H or associated with the event happening
    							findActuatorA1(L1, H, Acc, AccTemp), % find actuator based on the sensor list
    							findSensorA1(T, AccTemp, AccF).

conflictTry(Acc,AccF):-findall(X, event(_,_,X,_),L1),findSensorTry(L1,Acc,AccF). % L1 is the list of time Acc should start from 0

% get triggered rules
% conflictA1Count2([a001,a002, a001,a003],0,A)
%conflictA1Count2([],_,A).
conflictA1Count2(L,Acc,AccF):-  length(L,LenghtMain), list_setB(L,L1), length(L1, LenghtPruned),
    										AccTemp is LenghtMain - LenghtPruned, AccF is Acc+AccTemp.

%traverseRuleA1([r001,r002],L,25,0,A).
traverseRuleA1([],L,_,L).
traverseRuleA1([H|T], Lmain, SensorValue, Lnew):- rule(H, _, Relation1, RuleValue, _, _, _),
    											whatRelation(RuleValue, SensorValue, Relation2), Relation1 \= Relation2,
    											traverseRuleA1(T, Lmain, SensorValue, Lnew).
    %											conflictA1Count2(Lmain, Acc, AccF).

traverseRuleA1([H|T], Lmain, SensorValue, Lnew):- rule(H, _, Relation1, RuleValue, Actuator, _, _),
    											whatRelation(RuleValue, SensorValue, Relation2), Relation1 == Relation2,
    											add_tail(Lmain, Actuator, Lupdated),
    											traverseRuleA1(T, Lupdated,SensorValue, Lnew).
    %											conflictA1Count2(Lupdated, Acc, AccF). % this has the problem

findRuleA1([],_,_,A,A).
findRuleA1([H|T],L2, Time, Acc, AccF):- event(_,H, Time, SensorValue),
    										findall(X, rule(X,H,_,_,_,_,_),RIDList),
    										traverseRuleA1(RIDList, L2, SensorValue, Lnew),
    										conflictA1Count2(Lnew, Acc, AccF).
   											 
% findSensorA1([1030], 0,A)
% findSensorA1([1030,1130], 0,A)
findSensorA1([],A,A).
findSensorA1([H|T],Acc,AccF):- findall(X,event(_,X,H,_),L1), % get all sensor id  at H time
    							findRuleA1(L1, [], H, Acc, AccTemp),% list of sensor L1, H is time
    							findSensorA1(T,AccTemp, AccF).

conflictA1(Acc, AccF):- findall(X, event(_,_,X,_),L1),findSensorA1(L1,Acc,AccF).

add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

% findall(X, event(_,_,X,_),L)
=(X, Y, R) :- X == Y,    !, R = true.
=(X, Y, R) :- ?=(X, Y),  !, R = false. % syntactically different
=(X, Y, R) :- X \= Y,    !, R = false. % semantically different
=(X, Y, R) :- R == true, !, X = Y.
=(X, X, true).
=(X, Y, false) :-
   dif(X, Y).

if_(C_1, Then_0, Else_0) :-
   call(C_1, Truth),
   functor(Truth,_,0),  % safety check
   ( Truth == true -> Then_0 ; Truth == false, Else_0 ).



list_item_subtracted([],_,[]).
list_item_subtracted([A|As],E,Bs1) :-
    if_(A = E, Bs = Bs1, Bs1 = [A|Bs]),
    list_item_subtracted(As,E,Bs).

list_setB([],[]).
list_setB([X|Xs1],[X|Ys]) :-
    list_item_subtracted(Xs1,X,Xs),
    list_setB(Xs,Ys).

max(X,Y,Z) :-   X =< Y -> Z = Y ; Z = X .