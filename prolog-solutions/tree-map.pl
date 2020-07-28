split(null, _, null, null) :- !.

split(tree_node(Y, K, V,  NL, NR), Key, L, tree_node(Y, K, V,  Right, NR)) :-
    \+ Key >= K,
    split(NL, Key, L, Right).

split(tree_node(Y, K, V, NL, NR), Key, tree_node(Y, K, V, NL, Left), R) :-
    Key >= K,
    split(NR, Key, Left, R).


map_remove(T, K, TR) :-
	split(T, K - 1, L, R),
	split(R, K, _, NR),
	merge(L, NR, TR).

map_put(TreeMap, Key, Val, R) :-
    map_remove(TreeMap, Key, T),
    rand_int(2000000, Y),
    split(T, Key, R1, R2),
    merge(R1, tree_node(Y, Key, Val, null, null), TR),
    merge(TR, R2, R).

merge(null, Right, Right) :- !.
merge(Left, null, Left) :- !.

merge(tree_node(YL, KL, VL, LeftL, RightL), tree_node(YR, KR, VR,  LeftR, RightR), tree_node(YL, KL, VL, LeftL, R)) :-
    YL > YR,
    merge(RightL, tree_node(YR, KR, VR,  LeftR, RightR), R).

merge(tree_node(YL, KL, VL,  LeftL, RightL), tree_node(YR, KR, VR, LeftR, RightR), tree_node(YR, KR, VR, L, RightR)) :-
    \+ YL > YR,
    merge(tree_node(YL, KL, VL, LeftL, RightL), LeftR, L).

map_build([], null) :- !.

map_build([(CK, CV) | Tail], T) :-
    map_build(Tail, TR),
    map_put(TR, CK, CV, T).

% map_submapSize(Map, FromKey, ToKey, Size) :-
%       map_floorKey(T, Key, CK),
%       split(T, Key - 1, R1, R2),
%      map_minKey(tree_node(_, K, _,  null, _), K).
%


map_minKey(tree_node(_, K, _,  null, _), K) :- !.

map_minKey(tree_node(_, _, _, L, _), Result) :-
    map_minKey(L, Result).

map_maxKey(tree_node(_, K, _,  _, null), K) :- !.

map_maxKey(tree_node(_, _, _, _, R), Result) :-
    map_maxKey(R, Result).

map_ceilingKey(T, Key, CK) :-
    split(T, Key - 1, R1, R2),
    map_minKey(R2, CK).

map_get(tree_node(_, Key, Val, _, _), Key, Val).

map_get(tree_node(Y, K, V, _, Right), Key, Val) :-
    K < Key,
    map_get(Right, Key, Val).

map_get(tree_node(Y, K, V, Left, _), Key, Val) :-
    \+ K < Key,
    map_get(Left, Key, Val).

map_floorKey(T, Key, CK) :-
    split(T, Key, R1, R2),
    map_maxKey(R1, CK).
