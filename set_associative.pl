getNth([H|T],0,H).
getNth([H|T],N,V):-
	N>0,
	N1 is N-1,
	getNth(T,N1,V).
getInd([H|T],0,V):-
	H=item(tag(Tag),Data,1,_),
	V=item(tag(Tag2),Data2,_,_),
	atom_number(Tag, H1),
	atom_number(Tag2, V1),
	V1=H1,
	Data=Data2.
getInd([H|T],N,V):-
	getInd(T,N1,V),
	N is N1+1.
	
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	atom_number(StringAddress, Number),
	convertAddress(Number,SetsNum,Tag,Idx,setAssoc),
	convertBinToDec(Idx, Idx2),
	length(Cache,L),
	X is L//SetsNum,
	splitEvery(X,Cache,Res),
	getNth(Res,Idx2,Set),
	atom_number(Tag2, Tag),
	string_length(Tag2,Y),
	string_length(StringAddress,Y1),
	logBase2(SetsNum,Y3),
	Y2 is Y1-Y3-Y,
	fillZeros(Tag2,Y2,Tag3),
	getInd(Set,HopsNum,item(tag(Tag3),data(Data),1,_)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
	logBase2(SetsNum,B),
	Tag is Bin // 10**B,
	Idx is Bin mod 10**B.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,
	[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
	getNumBits(NumOfSets,Type,OldCache,BitsNum),
	(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
	getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
	runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
	getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
	\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	atom_number(StringAddress,Address),
	convertAddress(Address,BitsNum,Tag,Idx,Type),
	replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

	
	