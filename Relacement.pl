%% Direct Mapping

replaceInCache(Tag, Idx, Mem, OldCache, NewCache, ItemData, directMap, BitsNum):-
    convertBinToDec(Tag, TagVal),
    convertBinToDec(Idx, IdxVal),
    Address is (TagVal << BitsNum) + IdxVal,        % X << Y = X * (2^Y)
    incrementValidOrders(OldCache, OldCache2),
    writeln(OldCache2),
    getIdx(Mem, Address, ItemData),
    getNumBits(0, directMap, Mem, MemBits),
    TagBits is MemBits - BitsNum,
    len(Tag, L),
    AdditionalTagBits is TagBits - L,
    fillZeros(Tag, AdditionalTagBits, NewTag),
    replaceIthItem(item(tag(NewTag), data(ItemData), 1, 0), OldCache2, IdxVal, NewCache).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fully Associative

replaceInCache(Tag, _, Mem, OldCache, NewCache, ItemData, fullyAssoc, 0):-
    convertBinToDec(Tag, Address),
    incrementValidOrders(OldCache, OldCache2),
    getIdx(Mem, Address, ItemData),
    getNumBits(0, directMap, Mem, MemBits),
    len(Tag, L),
    AdditionalTagBits is MemBits - L,
    fillZeros(Tag, AdditionalTagBits, NewTag),
    replaceFifo(item(tag(NewTag), data(ItemData), 1, 0), OldCache2, NewCache).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set Associative

replaceInCache(Tag, Idx, Mem, OldCache, NewCache, ItemData, setAssoc, SetsNum):-
    convertBinToDec(Tag, TagVal),
    convertBinToDec(Idx, IdxVal),
    getNumBits(0, directMap, Mem, MemBits),
    getNumBits(SetsNum, setAssoc, OldCache, Idx_bits),
    Address is (TagVal << Idx_bits) + IdxVal,         % X << Y = X * (2^Y)
    incrementValidOrders(OldCache, OldCache2),
    getIdx(Mem, Address, ItemData),
    length(OldCache, L),
    SetSize is L // SetsNum,
    splitEvery(SetSize, OldCache2, Divided),
    getIdx(Divided, IdxVal, Set),
    TagBits is MemBits - Idx_bits,
    len(Tag, Tag_length),
    AdditionalTagBits is TagBits - Tag_length,
    fillZeros(Tag, AdditionalTagBits, NewTag),
    replaceFifo(item(tag(NewTag), data(ItemData), 1, 0), Set, NewSet),
    replaceIthItem(NewSet, Divided, IdxVal, Divided_NewCache),
    merge(Divided_NewCache, NewCache).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Replacement Helper Functions

%% increments the order of every item in the list
incrementValidOrders([], []).

incrementValidOrders([item(Tag, Data, 1, Order)|T1], [H|T2]):-
    Order2 is Order + 1,
    H = item(Tag, Data, 1, Order2),
    incrementValidOrders(T1, T2).

incrementValidOrders([item(Tag, Data, 0, Order)|T1], [H|T2]):-
    H = item(Tag, Data, 0, Order),
    incrementValidOrders(T1, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% getIdx returns the element at the position: Idx
getIdx(L, Idx, X):-
    getIdx(L, 0, Idx, X).

getIdx([H|_], Idx, Idx, H).

getIdx([_|T], CurrIdx, Idx, X):-
    CurrIdx < Idx,
    NewIdx is CurrIdx + 1,
    getIdx(T, NewIdx, Idx, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% to find the length of a binary number
len(X, L):-
    atom_chars(X, Y),
    length(Y, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replaceFifo(X, OldList, NewList):-
    idxOfMaxOrder(OldList, Idx),
    replaceIthItem(X, OldList, Idx, NewList).  %% from the general component

idxOfMaxOrder(L, Idx):-
    idxOfMaxOrder(L, _, 0, Idx).

idxOfMaxOrder([X], Order, Idx, Idx):-
    orderOf(X, Order).

idxOfMaxOrder([H|T], Order, CurrIdx, Idx):-
    T \= [],
    orderOf(H, CurrOrder),
    NewIdx is CurrIdx + 1,
    idxOfMaxOrder(T, NextOrder, NewIdx, NextIdx),
    max(CurrOrder, CurrIdx, NextOrder, NextIdx, Order, Idx).

max(infinity, Idx1, Order2, Idx2, infinity, Idx1).

max(Order1, Idx1, infinity, Idx2, infinity, Idx2):-
    Order1 \= infinity.

max(Order1, Idx1, Order2, Idx2, Order1, Idx1):-
    number(Order1),
    number(Order2),
    Order1 >= Order2.

max(Order1, Idx1, Order2, Idx2, Order2, Idx2):-
    number(Order1),
    number(Order2),
    Order1 < Order2.

orderOf(item(_, _, 0, _), infinity).

orderOf(item(_, _, 1, Order), Order).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turns a list of lists into 1 list that is the result of concatinating all the internal lists

merge([], []).

merge([H|T], Ans):-
    merge(T, Ans2),
    append(H, Ans2, Ans).