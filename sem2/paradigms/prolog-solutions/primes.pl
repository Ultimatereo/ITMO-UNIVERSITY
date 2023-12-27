concat_n_times(_, 0, []) :- !.
concat_n_times(Mas, N, R) :- 
	N > 0, 1 is mod(N, 2), !,
	N1 is N - 1, concat_n_times(Mas, N1, R1),
	append(Mas, R1, R).
concat_n_times(Mas, N, R) :-
	N > 0, 0 is mod(N, 2), !,
	N2 is div(N, 2), concat_n_times(Mas, N2, R2),
	append(R2, R2, R).
concat_n_times_with_tail(Mas, N, Res, T) :-
	append(R, T, Res),
	concat_n_times(Mas, N, R), !.
prime(N) :- 2 is N, !.
prime(N) :- N > 2, not composite(N).
init(N) :-
	composite_gen(2, N).

check_boundaries(Index, N) :-
	Square is Index * Index,
	Square =< N.

composite_generator(I, N, J) :-
	J =< N,
	assert(composite_table(J)),
	J1 is J + I,
	composite_generator(I, N, J1).
	
composite_gen(I, N) :-
	Square is I * I,
	Square =< N,
	prime(I),
	composite_generator(I, N, Square).

composite_gen(I, N) :-
	check_boundaries(I, N),
	I1 is I + 1,
	composite_gen(I1, N).
	
getNextPrime(N, R1, I) :-
	prime(I), !,
	R1 is I.
	
getNextPrime(N, R1, I) :-
	I1 is I + 1,
	getNextPrime(N, R1, I1).
next_prime(N, R) :- next_prime_table(N, R), !.
next_prime(N, R) :-
	I is N + 1,
	getNextPrime(N, R1, I),
	R is R1,
	assert(next_prime_table(N, R)).
	
composite(J) :- composite_table(J), !.
		
foldLeft([], Z, _, Z).
foldLeft([H | T], Z, F, R) :- 
	G =.. [F, Z, H, RH], 
	call(G), 
	foldLeft(T, RH, F, R).
multiply(A, B, R) :- number(A), number(B), !, R is A * B.
multiply(R, H) :-  foldLeft(H, 1, multiply, R).

check_prime([]).
check_prime([H | T]) :-
	prime(H),
	check_prime(T).

check_monotone(R, []).
check_monotone(H1, [H2 | T]) :-
	H1 =< H2,
	check_monotone(H2, T).
check_monotone([H | T]) :-
	check_monotone(H, T).
shrink([], _, []) :- !.
shrink(Divisors, N, [H2 | T2]) :-
	concat_n_times_with_tail([H2], N, Divisors, T3),
	shrink(T3, N, T2).
multiply(Num, N, Divisors) :-
	shrink(Divisors, N, D1),
	multiply(Num, D1).

power_divisors(1, _, []) :- !.
power_divisors(Num, N, Divisors) :-
	not(number(Num)), !,
	check_prime(Divisors),
	check_monotone(Divisors),
	multiply(Num, N, Divisors).

divide(N, [N], I) :- 
	I * I > N, !. 
	
divide(N, [H | T], I) :-
	0 is mod(N, I), !,
	H is I,
	N1 is div(N, I),
	divide(N1, T, I).
	
divide(N, Divisors, I) :-
	!, next_prime(I, I1),
	divide(N, Divisors, I1).
power_divisors(Num, N, Divisors) :-
	number(Num),
	divide(Num, D1, 2),
	shrink(Divisors, N, D1).	
	
dec_to_kth(0, _, []) :- !.

dec_to_kth(N, K, [Head | Tail]) :-
    N1 is div(N, K),
    Head is mod(N, K),
    dec_to_kth(N1, K, Tail).

is_palindrome_(N, K) :-
    dec_to_kth(N, K, List),
    reverse(List, List).

prime_palindrome(N, K) :-
    prime(N),
    is_palindrome_(N, K).
prime_divisors(N, Divisors) :-
	power_divisors(N, 1, Divisors).

unique_list([H], [H], H1) :- not(H1 = H), !.

unique_list([H], [], H) :- !.

unique_list([H1 | T1], D, Prev) :-
	H1 = Prev, !,
	unique_list(T1, D, Prev).

unique_list([H1 | T1], [H2 | T2], Prev) :-
	H2 is H1,
	unique_list(T1, T2, H1).
	
unique_prime_divisors(1, []) :- !.	
unique_prime_divisors(N, Divisors) :-
	prime_divisors(N, D2), 
	unique_list(D2, Divisors, 0).
	