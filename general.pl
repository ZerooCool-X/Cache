convertBinToDec(X, Ans):-
	atom_chars(X, Y),
	convertBinToDec(Y, 0 ,Ans).

convertBinToDec([], X, X).

convertBinToDec([H|T], X, Ans):-
	atom_number(H, Val),
	X_new is X*2 + Val,
	convertBinToDec(T, X_new, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replaceIthItem(X, L, I, R):-
	replaceIthItem(X, L, I, 0, R).

replaceIthItem(X, [_|T], I, I, [X|T]).

replaceIthItem(X, [H|T], I, CurrIdx, [H|T2]):-
	I \= CurrIdx,
	NewIdx is CurrIdx + 1,
	replaceIthItem(X, T, I, NewIdx, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitEvery(_,[],[]).

splitEvery(N,List,Res):-
	splitEvery(N,N,List,[],[],Res).

splitEvery(N,0,List,X,Y,Res):-
	append(Y, [X], Y1),
	splitEvery(N,N,List,[],Y1,Res).

splitEvery(_,_,[],X,Y,Y1):-
	X\=[],
	append(Y,[X],Y1).

splitEvery(N,C,[H|T],X,Y,Res):-
	C\=0,
	C1 is C - 1,
	append(X,[H],X1),
	splitEvery(N,C1,T,X1,Y,Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logBase2(N,0):-
	N=<1.

logBase2(N,R):-
	N > 1,
	N1 is N/2,
	
	logBase2(N1,R1),
	R is R1 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getNumBits(_,fullyAssoc,_,0).

getNumBits(X,setAssoc,_,Y):-
	logBase2(X,Y).

getNumBits(_,directMap,X,Y):-
	length(X,X1),
	logBase2(X1,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fillZeros(X,0,X).

fillZeros(S,X,R):-
	X>0,
	X1 is X-1,
	fillZeros(S,X1,R1),
	string_concat("0", R1, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%