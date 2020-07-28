composite_table(1).

fill(C, N, P) :-
    N > C,
    CN is C + P,
    assert(composite_table(C)),
    fill(CN, N, P).

erato(C, N) :-
    \+ composite_table(C),
    CN is C * C,
    fill(CN, N, C).

erato(C, N) :-
    C * C < N,
    CN is C + 1,
    erato(CN, N).

init(N) :- erato(2, N).

divisor(N, C) :-
	0 is mod(N, C).
divisor(N, C) :-
    N >= C * C,
    С1 is C + 1,
    divisor(N, С1), !.

prime(2).
prime(C) :-
    \+ composite_table(C).

composite(N) :-
	composite_table(N).

gen_divisors(1, _, []).
gen_divisors(N, _, [N]) :- prime(N), !.

gen_divisors(N, DI, [H|T]) :-
    DI - 1 < N,
    N > 1,
    (0 is mod(N, DI) ->
    N1 is div(N, DI),
    H is DI,
    gen_divisors(N1, DI, T);
    DI1 is DI + 1,
    gen_divisors(N, DI1, [H|T])).

mull_divisors(1, _, []) :- !.
mull_divisors(R, L, [H|T]) :-
	H >= L,
	\+ composite(H),
   	mull_divisors(R1, H, T),
  	R is R1 * H.

prime_divisors(N, R) :-
     (integer(N) ->
       gen_divisors(N, 2, R);
       mull_divisors(N, 2, R)).

create_b(0, _, []) :- !.
create_b(N, K, [H | T]) :-
    N > 0,
    NN is div(N, K),
    H is mod(N, K),
    create_b(NN, K, T).

prime_palindrome(N, K) :-
	prime(N),
	create_b(N, K, R),
	reverse(R, R).