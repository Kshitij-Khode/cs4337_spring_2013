/* Author: Brian W. Devries
 * Email:  brian.devries@utdallas.edu
 *
 * Some Prolog examples.
 *
 * Note that not all of these examples are fully reversible, but they should
 * give you a sufficient idea of what you're being asked to do.
 */


% If the output from your Prolog system is not already colored, you can try
% uncommenting the following line.

%:- use_module( library( ansi_term ) ).


/* Unit Testing
 * ===========================================================================
 *
 * These let us use the PLUnit unit testing framework. Uncomment one of the
 * following lines, if appropriate.
 */

% TODO: Make sure unit testing works on your machine

% Uncomment this line if you are running on your own machine and encounter
% errors regarding PLUnit-related constructs (e.g., =begin_tests=, etc.)

% :- use_module( library( plunit ) ).


% Uncomment this line if you are running on CS1 or uncommenting the above line
% still doesn't work.
%
% The file `plunit` **MUST** be in the same directory as this file!!!

%:- use_module( plunit ).

% CLP(FD) is an extension to Prolog that permits us to use constraints over
% finite domains (i.e., finite ranges of integers). You can find more
% information at 
%
%     http://www.swi-prolog.org/man/clpfd.html.
:- use_module( library( clpfd ) ).



/* Example: Quicksort
 * ============================================================================
 *
 * The following predicates are used to implement quicksort in Prolog. Observe
 * that the calls unify their arguments with the parameters of the predicate,
 * letting us perform pattern matching (in this case between the empty and
 * non-empty lists) via the arguments to the predicate.
 */

/**
 * smaller_elems( + X, + L0 : list, - L1 : list)
 *
 * Succeeds if `L1` is the list containing all elements in `L0` that are
 * smaller (via =<) than `X`. The relative ordering of the elements is
 * preserved.
 */

% If `L0` is empty, then `L1` should be empty as well.
%
% In order to keep Prolog from complaining that the first argument here is
% only used once (i.e., is a singleton variable), we begin its name with an
% underscore.
smaller_elems( _X, [], [] ).


% If the head of `L0` (which we call `H`) is smaller than `X` (via `=<`), then
% the head of `L1` should also be `H`. The tail of `L1` (which we call `T1`
% should be the elements from the tail of `L0` (which we call `T`) that are
% smaller than `X`. 
%
% We use different variables `T` and `T1` because the two tails may not be the
% same.
smaller_elems( X, [ H | T ], [ H | T1 ] ) :-
    H #=< X,
    smaller_elems( X, T, T1 ).


% If `H` is larger than `X`, then `L1` should not contain `H`. `L1` will just
% be the elements of `T` that are smaller than `X`.
smaller_elems( X, [ H | T ], L1 ) :-
    H #> X,
    smaller_elems( X, T, L1 ).


:- begin_tests( smaller_elems ).


test( smaller_elems_empty,
	  [ L == [] ]
	) :-
	smaller_elems( 5, [], L ),
	!.


test( smaller_elems_single_equal,
	  [ L == [ 5 ] ]
	) :-
	smaller_elems( 5, [ 5 ], L ),
	!.


test( smaller_elems_single_smaller, 
	  [ L == [ 1 ] ]
	) :-
	smaller_elems( 5, [ 1 ], L ),
	!.


test( smaller_elems_single_larger,
	  [ L == [] ]
	) :-
	smaller_elems( 5, [ 8 ], L ),
	!.


test( smaller_elems_all_eq,
	  [ L == [ 5, 5, 5 ] ]
	) :-
	smaller_elems( 5, [ 5, 5, 5 ], L ),
	!.


test( smaller_elems_all_smaller,
	  [ L == [ 1, 2, 3, 4 ] ]
	) :-
	smaller_elems( 5, [ 1, 2, 3, 4 ], L ),
	!.


test( smaller_elems_all_larger,
	  [ L == [] ]
	) :-
	smaller_elems( 5, [ 6, 7, 8, 9 ], L ),
	!.


test( smaller_elems_mixed,
	  [ L == [ 1, 5, 3, 2 ] ]
	) :-
	smaller_elems( 5, [ 6, 1, 10, 8, 5, 3, 7, 2, 9 ], L ),
	!.


:- end_tests( smaller_elems ).


%:- run_tests( smaller_elems ).



/**
 * larger_elems( + X, + L0, - L1 )
 *
 * Succeeds if `L1` is the list containing all elements in `L1` that are
 * larger (via >) than `X`. The relative ordering of the elements is
 * preserved.
 */

% Again, if `L0` is empty, then `L1` should be empty as well.
larger_elems( _X, [], [] ).


% If `H` is larger than `X`, then `L1` should have `H` as its head as well.
% The tail of `L1` is the elements of `T` that are larger than `X`.
larger_elems( X, [ H | T ], [ H | T1 ] ) :-
    H #> X,
    larger_elems( X, T, T1 ).


% Similarly, if `H` is smaller than `X`, then `L1` should just be the elements
% of `T` that are larger than `X` --- `H` should not be included.
larger_elems( X, [ H | T ], L1 ) :-
    H #=< X,
    larger_elems( X, T, L1 ).


:- begin_tests( larger_elems ).


test( larger_elems_empty, 
	  [ L == [] ]
	) :-
	larger_elems( 5, [], L ),
	!.


% Observe that if `L0` contains `X`, `L1` will not.
test( larger_elems_single_equal,
	  [ L == [] ]
	) :-
	larger_elems( 5, [ 5 ], L ),
	!.


test( larger_elems_single_smaller,
	  [ L == [] ]
	) :-
	larger_elems( 5, [ 1 ], L ),
	!.


test( larger_elems_single_larger,
	  [ L == [ 8 ] ]
	) :-
	larger_elems( 5, [ 8 ], L ),
	!.


test( larger_elems_all_eq, 
	  [ L == [] ]
	) :-
	larger_elems( 5, [ 5, 5, 5 ], L ),
	!.


test( larger_elems_all_smaller,
	  [ L == [] ]
	) :-
	larger_elems( 5, [ 1, 2, 3, 4 ], L ),
	!.


test( larger_elems_all_larger,
	  [ L == [ 6, 7, 8, 9 ] ]
	) :-
	larger_elems( 5, [ 6, 7, 8, 9 ], L ),
	!.


test( larger_elems_mixed,
	  [ L == [ 6, 10, 8, 7, 9 ] ]
	) :-
	larger_elems( 5, [ 5, 6, 1, 10, 8, 5, 3, 7, 2, 9 ], L ),
	!.


:- end_tests( larger_elems ).


%:- run_tests( larger_elems ).



/**
 * quicksort( + L0, - L1 )
 *
 * Succeeds if `L1` is the result of quicksorting `L0`.
 *
 * We just apply a naive version where the first element is taken as the
 * pivot.
 */

% If `L0` is empty, then `L1` should also be empty.
quicksort( [], [] ).


% If `L1` is not empty, then split it into its head and tail (`H` and `T`
% respectively). Use `H` as the pivot and obtain the elements of `T` that are
% smaller and larger than `H`, then recursively sort these two sub-lists.
% Finally, use `append / 3` to build the result `L1`.
quicksort( [ H | T ], L1 ) :-
    % Bind `Small` to the elements smaller than `H` (using =<).
    smaller_elems( H, T, Small ),

    % Bind `Big` to the elements bigger than `H` (using >).
    larger_elems( H, T, Big ),

    % Quicksort `Small` and `Big`
    quicksort( Small, SmallSorted ),
    quicksort( Big, BigSorted ),

    % Now append the list containing just the pivot to `SmallSorted`
    append( SmallSorted, [ H ], Prefix ),

    % Finally, append `BigSorted` to the `Prefix` to get the final sorted list
    % `L1`.
    append( Prefix, BigSorted, L1 ).


:- begin_tests( quicksort ).


test( quicksort_empty,
	  [ L == [] ]
	) :-
	quicksort( [], L ),
	!.


test( quicksort_single, 
	  [ L == [ 5 ] ]
	) :-
	quicksort( [ 5 ], L ),
	!.


test( quicksort_all_eq,
	  [ L == [ 7, 7, 7 ] ]
	) :-
	quicksort( [ 7, 7, 7 ], L ),
	!.


test( quicksort_full_1,
	  [ L == [ 1, 2, 3, 4, 5 ] ]
	) :-
	quicksort( [ 3, 5, 1, 2, 4 ], L ),
	!.


test( quicksort_full_rev,
	  [ L == [ 6, 7, 8, 9, 10 ] ]
	) :-
	quicksort( [ 10, 9, 8, 7, 6 ], L ),
	!.


:- end_tests( quicksort ).


%:- run_tests( quicksort ).



/**
 * sum_list( + L : list, - Sum : integer)
 *
 * Succeeds if Sum is the sum of all elements in L0, where L0 is a list of
 * integers.
 *
 * This is to illustrate the =is= operator, which evaluates its second
 * (right-hand) argument as an arithmetic expression and unifies the result
 * with its first (left-hand) argument. The right-hand argument **CANNOT**
 * contain unbound variables. Examples:
 *
 *     ?- X = 2 + 3.            % will bind `X` to the term `+( 2, 3 )`
 *     X = 2 + 3.
 *
 *     ?- X is 2 + 3.           % will evaluate `2 + 3` to 5, and bind 5 to `X`.
 *     X = 5.
 *
 *     ?- X = Y + 4.            % will bind `X` to the term `+( Y, 4 )`
 *     X = Y + 4.
 *
 *     ?- X is Y + 4.           % will cause an errer, as `Y` is a variable
 *     ERROR: is/2: Arguments are not sufficiently instantiated
 *
 *     ?- Y = 2, X is Y + 4.    % Works fine, as Y is bound beforehand
 *     X = 6.
 */

% If `L0` is empty, then the sum of the elements is 0.
sum_list( [], 0 ).


% If `L0` is non-empty, then recurse to obtain the sum of its tail `T`, then
% add `H` to this partial sum to obtain `Sum`.
sum_list( [ H | T ], Sum ) :-
    
    % Bind the new variable `TailSum` to the sum of the tail of the list
    sum_list( T, TailSum ),

    % Obtain the final sum by adding the head to `TailSum`.
    Sum is H + TailSum.



:- begin_tests( sum_list ).


test( sum_list_empty,
	  [ X == 0 ]
	) :-
	sum_list( [], X ), 
	!.


test( sum_list_single,
	  [ X == 7 ]
	) :-
	sum_list( [ 7 ], X ),
	!.


test( sum_list_multiple,
	  [ X == 15 ]
	) :-
	sum_list( [ 1, 2, 3, 4, 5 ], X ),
	!.


test( sum_list_posneg,
	  [ X == 0 ]
	) :-
	sum_list( [ 1, 5, -2, 3, -4, 4, -1, -5, 2, -3 ], X ),
	!.


:- end_tests( sum_list ).


%:- run_tests( sum_list ).



/**
 * sum_list_tailrec( + L : list, - Sum : integer )
 *
 * An alternative version of =|sum_list / 2|= that takes a list of integers L0
 * and computes their sum. 
 *
 * This version differs from =|sum_list / 2|= in the following two respects:
 *  *  It uses CLP(FD) operators for computing adding the values together,
 *     rather than =is=.
 *
 *  *  It is tail-recursive, relying on the helper predicate
 *     =|sum_list_tailrec2 / 3|=. This means that the partial sum is passed
 *     along to the recursive call, rather than computed as the recursive
 *     calls return.
 */
sum_list_tailrec( L, Sum ) :-
	% Because we've not considered any numbers, the partial sum so far is 0.
	% This is what we pass to the initial call to =|sum_list_tailrec2|=.
	sum_list_tailrec2( L, 0, Sum ).


% If the list is empty, then the partial sum is the full sum.
sum_list_tailrec2( [], Partial, Partial ).

% If the list is not empty, then add include the value of the head in the
% partial sum and pass the result to the recursive call. The sum that is
% computed for the recursive call is also the full sum for this call.
sum_list_tailrec2( [ H | T ], Partial, Sum ) :-
	NewPartial #= H + Partial,
	sum_list_tailrec2( T, NewPartial, Sum ).


:- begin_tests( sum_list_tailrec ).


test( sum_list_tailrec_empty,
	  [ X == 0 ]
	) :-
	sum_list_tailrec( [], X ), 
	!.


test( sum_list_tailrec_single,
	  [ X == 7 ]
	) :-
	sum_list_tailrec( [ 7 ], X ),
	!.


test( sum_list_tailrec_multiple,
	  [ X == 15 ]
	) :-
	sum_list_tailrec( [ 1, 2, 3, 4, 5 ], X ),
	!.


test( sum_list_tailrec_posneg,
	  [ X == 0 ]
	) :-
	sum_list_tailrec( [ 1, 5, -2, 3, -4, 4, -1, -5, 2, -3 ], X ),
	!.


:- end_tests( sum_list_tailrec ).

%:- run_tests( sum_list_tailrec ).



/**
 * gcd( ? X : integer, ? Y : integer, ? GCD : integer )
 *
 * A version of Euclid's GCD algorithm that uses CLP(FD). This makes it
 * "reversible," in the sense that we can treat any of the arguments as inputs
 * or outputs, depending on what we leave as a variable.
 *
 * WARNING: This predicate is not fully reversible yet.
 */
gcd( 0, Y, Y ).

gcd( X, 0, X ).

gcd( X, Y, GCD ) :-
	% Ensure that the inputs are larger than 0, to prevent this clause from
	% overlapping with the base cases.
	X #> 0,
	Y #> 0,

	% Add the constraints on GCD.
	GCD #>= 0,
	GCD #=< X,
	GCD #=< Y,

	% Now handle the case where X >= Y
	X #>= Y,
	NewX #= X - Y,
	gcd( NewX, Y, GCD ).

gcd( X, Y, GCD ) :-
	% Again, make sure that we don't overlap with our base cases.
	X #> 0,
	Y #> 0,

	GCD #>= 0,
	GCD #=< X,
	GCD #=< Y,

	% Now make sure that we don't overlap with the recursive case above. We
	% already included the case where X #= Y above, so we the comparison here
	% is strict.
	X #< Y,
	NewY #= Y - X,
	gcd( X, NewY, GCD ).


:- begin_tests( gcd ).

test( gcd_equal,
	  [ GCD == 4 ]
	) :-
	gcd( 4, 4, GCD ).


:- end_tests( gcd ).




/**
 * member_check( + Val, + L : list )
 *
 * Succeeds if `Val` is one of the elements of the list `L`.
 *
 * Note that, unlike in Haskell, we do not care about the type of `Val`. We
 * only care whether we can unify `Val` with one of the elements of `L`. 
 *
 * This example illustrates the use of the "not unifiable" operator `\=`,
 * which succeeds if its two arguments cannot be unified. 
 *  *  NOTE: Because variables can always unify with a term, `X \= 3` will
 *     fail. Thus, `\=` should not be read as "not equal" unless both
 *     arguments are grounded. This is why both arguments to 
 *     `member_check / 2` are assumed to be grounded. 
 *
 *     For this reason, you would not actually write membership this way in
 *     practice; the builtin `member` predicate is not implemented this way
 *     and is thus more general.
 *
 * Examples:
 * 
 *     ?- 3 \= 3.
 *     false.
 *
 *     ?- 3 \= 4.
 *     true.
 *
 *     ?- a \= b.
 *     true.
 *
 *     ?- f( a, b ) \= f( b, a ).
 *     true.
 *
 *     ?- X \= f( a ).
 *     false.
 */

% If `Val` is at the head of `L`, then succeed.
% As in `smaller_elems / 3`, we use `_Tail` for the tail to avoid singleton
% variable warnings.
member_check( Val, [ Val | _Tail ] ).


% If `Val` is not unifiable with the head of `L`, then keep checking. If it
% is, we want to stop looking --- we've already found the answer.
member_check( Val, [ Head | Tail ] ) :-
    Val \= Head,
    member_check( Val, Tail ).



:- begin_tests( member_check ).


test( member_check_empty, 
	  [ fail ]
	) :-
	member_check( a, [] ),
	!.


test( member_check_single_present,
	  [ true ]
	) :-
	member_check( a, [ a ] ),
	!.


test( member_check_single_nonpresent,
	  [ fail ]
	) :-
	member_check( foo, [ 5 ] ),
	!.


test( member_check_multiple_present_once,
	  [ true ]
	) :-
	member_check( foo, [ 4, a, foo, bar, 2 * 2 ] ),
	!.


test( member_check_multiple_nonpresent,
	  [ fail ]
	) :-
	member_check( 2 * 2, [ 4, a, foo, bar ] ),
	!.


% In the following test, observe that the term `2 * 2` is present exactly
% twice --- as in the above test, `2 * 2` does not unify with `4`.
test( member_check_multiple_present_twice,
	  [ true ]
	) :-
	member_check( 2 * 2, [ 2 * 2, 4, a, foo, bar, 2 * 2 ] ),
	!.


:- end_tests( member_check ).


%:- run_tests( member_check ).



/**
 * member_count( ? Val, ? N : integer, ? L : list )
 *
 * Succeeds if `Val` occurs exactly `N` times in the list `L`. Note that `N`
 * is represented as a regular integer (i.e., positional / "normal" notation,
 * such as 1234).
 */

member_count( _Val, 0, [] ).

member_count( Val, Count, [ Val | Tail ] ) :-
	member_count( Val, TailCount, Tail ),
	Count #= TailCount + 1.

member_count( Val, Count, [ Head | Tail ] ) :-
	Val \= Head,
	member_count( Val, Count, Tail ).



:- begin_tests( member_count ).


test( member_count_empty, 
      [ true ]
    ) :-
    member_count( a, 0, [] ),
	!.


test( member_count_empty_nonzero,
      [ fail ]
    ) :-
    member_count( a, 1, [] ),
	!.


test( member_count_single,
      [ true ]
    ) :-
    member_count( a, 1, [ a ] ),
	!.


test( member_count_single_not_there,
      [ fail ]
    ) :-
    member_count( a, 1, [ b ] ),
	!.


test( member_count_multiple,
      [ true ]
    )
    :-
    member_count( a, 3, [ b, b, a, b, a, a ] ),
	!.


test( member_count_too_few,
      [ fail ]
    ) :-
    member_count( a, 3, [ b, b, a, b, a ] ),
	!.


test( member_count_too_many,
      [ fail ]
    ) :-
    member_count( a, 3, [ b, b, a, b, a, b, a, a ] ),
	!.


:- end_tests( member_count ).

:- run_tests( member_count ).




/**
 * nat( ? N )
 *
 * Succeeds if `N` is a natural number in successor notation.
 */
nat( 0 ).
nat( s( N ) ) :- 
    nat( N ).


/**
 * nat_int_bad( ? Succ, ? N )
 *
 * Maps between natural numbers represented using successor notation (aka
 * Peano numbers) and positional / "normal" notation (e.g., 1234).
 *
 * This is the version presented in class, and it works well enough in most
 * cases. However, if the first argument is a variable (e.g., we are
 * converting 3 to `s( s( s( 0 ) ) )`), then backtracking will cause the
 * predicate to enter an infinite loop. We could work around this using
 * non-declarative features of Prolog, but there's a better way, presented as
 * `nat_int / 2` below.
 */
nat_int_bad( 0, 0 ).

nat_int_bad( s( Succ ), N ) :-
    nat_int_bad( Succ, N1 ),
    N is N1 + 1.



/**
 * nat_int( ? Succ, ? N )
 *
 * Maps between natural numbers represented using successor notation (aka
 * Peano numbers) and positional / "normal"  notation (e.g., 1234).
 *
 * Your test code should use this predicate instead of `nat_int_bad /2` above.
 *
 * 
 * To explain the code a bit, this version uses an extension of Prolog known
 * as Constraint Logic Programming (CLP), seen with the operators `#>` and
 * `#=` below. These permit arithmetic with unbound variables, essentially
 * carrying the information about their relationships along until the
 * variables become grounded. This way, we do not enter an infinite loop when
 * the first argument is a variable, unlike `nat_int_bad` above.
 *
 * `#>` is the greater-than operator; the difference from `>` is that `N` does
 * not need to be grounded (i.e., `N` could still be a variable, as is the
 * case for the call `nat_int( s( 0 ), X)`. If `N` is a variable, then the
 * interpreter will defer checking this constraint until `N` is bound, and
 * only permit the binding to succeed if it satisfies this constraint.
 *
 * `#=` says that the two arithmetic expressions must be equal. It differs
 * from `is` in a similar way to `#>` --- it permits variables on the
 * right-hand side. In this case, `N1` will not have been bound to anything
 * before this line. What happens depends on whether `N` is bound to anything
 * or not:
 *  *  If `N` is bound to a value (meaning that we supplied the second
 *     argument in the call), then the Prolog system will basically perform
 *     algebra to solve the equation for `N1`, binding `N1` to the evaluation
 *     of `N - 1`. 
 *
 *  *  If `N` is not bound (meaning we passed a variable for the second
 *     argument), then the system will hold onto the relationship until either
 *     `N` or `N1` is bound to a value and solve for the other variable
 *     immediately. In this case, `N1` will be bound first, at which point `N`
 *     will be immediately bound to `the evaluation of `N1 + 1`.
 *
 * Essentially, CLP lets us set up the constraints on the values `N` and `N1`
 * *before* the recursive call, regardless of whether an argument was
 * supplied for `N`, thereby preventing the infinite loop that 
 * `nat_int_bad / 2` above exhibits.
 */

% 0 in successor notation <-> 0 in numerical notation.
nat_int( 0, 0 ).


% `s( Succ )` in successor notation <-> `N` in numerical notation if `N` is
% greater than 0 (required because `s( Succ )` represents a value greater than
% 0), `Succ` <-> `N1`, and `N` is one larger than `N1`.
nat_int( s( Succ ), N ) :-
    N #> 0,
    N #= N1 + 1,
    nat_int( Succ, N1 ).


:- begin_tests( nat_int ).


test( nat_int_0_int,
      [ X == 0 ]
    ) :-
    nat_int( 0, X ),
	!.


test( nat_int_0_succ,
      [ X == 0 ]
    ) :-
    nat_int( X, 0 ),
	!.


test( nat_int_5_int,
      [ X == 5 ]
    ) :-
    nat_int( s( s( s( s( s( 0 ) ) ) ) ), X ),
	!.

test( nat_int_5_succ, 
      [ X == s( s( s( s( s( 0 ) ) ) ) ) ]
    ) :-
    nat_int( X, 5 ),
	!.


test( nat_int_2plus3_succ,
      [ X == s( s( s( s( s( 0 ) ) ) ) ) ]
    ) :-
    nat_int( s( s( 0 ) ), A ),
    nat_int( s( s( s( 0 ) ) ), B ),
    C is A + B,
    nat_int( X, C ),
	!.


:- end_tests( nat_int ).

% Uncomment to run the `nat_int / 2` tests
%:- run_tests( nat_int ).



/**
 * nat_plus( ? N0, ? N1, ? Sum )
 *
 * Succeeds if `Sum` is `N0` + `N1`, where `Sum`, `N0`, and `N1` are natural
 * numbers represented with successor notation.
 *
 * Note that we can pass a value for `Sum` and one of `N0` or `N1` to perform
 * subtraction.
 *
 * Examples:
 *     
 *     % Check 2 + 3 = 5
 *     ?- nat_plus( s( s( 0 ) ), s( s( s( 0 ) ) ), X ).
 *     X = s( s( s( s( s( 0 ) ) ) ) )
 *
 *     % Check 2 + X = 5  =>  X = 3
 *     ?- nat_plus( s( s( 0 ) ), X, s( s( s( s( 0 ) ) ) ) ).
 *     X = s( s( s( 0 ) ) )
 *
 *     % Check X + X = 4  =>  X = 2
 *     ?- nat_plus( X, X, s( s( s( s( 0 ) ) ) ) ).
 *     X = s( s( 0 ) )
 */

% If the first argument is 0, then 0 + X = X.
nat_plus( 0, N1, N1 ) :- 
    % Perform rudimentary type checking to make sure that N1 is actually a
    % natural number, as this is not guaranteed by `plus` (unlike `N0`, which
    % must be a natural number for this predicate to succeed). 
    nat( N1 ).

% If the first argument is `s( N0 )`, then add `N0` to `N1` and take the
% successor of the result.
nat_plus( s( N0 ), N1, s( Sum ) ) :- 
    nat_plus( N0, N1, Sum ).



:- begin_tests( nat_plus ).


test( nat_plus_0plus0_t0,
	  [ X == 0 ]
	) :-
	nat_plus( 0, 0, X ),
	!.


% Test when the first argument is unground
test( nat_plus_0minus0_t1,
	  [ X == 0 ]
	) :-
	nat_plus( X, 0, 0 ),
	!.


% Test when the second argument is unground
test( nat_plus_0minus0_t2,
	  [ X == 0 ]
	) :-
	nat_plus( 0, X, 0 ),
	!.


test( nat_plus_3plus5,
	  [ X == 8 ]
	) :-
	nat_int( A, 3 ),
	nat_int( B, 5 ),
	nat_plus( A, B, Y ),
	nat_int( Y, X ),
	!.


test( nat_plus_8minus5,
	  [ X == 3 ]
	) :-
	nat_int( A, 8 ),
	nat_int( B, 5 ),
	nat_plus( Y, B, A ),
	nat_int( Y, X ),
	!.


test( nat_plus_8minus3,
	  [ X == 5 ]
	) :-
	nat_int( A, 8 ),
	nat_int( B, 3 ),
	nat_plus( B, Y, A ),
	nat_int( Y, X ),
	!.


:- end_tests( nat_plus ).


%:- run_tests( nat_plus ).




/* Problem 2: nat_multiply / 3
 * ============================================================================
 *
 * Write a predicate `nat_multiply / 3` that takes three natural numbers
 * represented with successor notation:
 *  *  `N0`
 *  *  `N1`
 *  *  `Product`
 *
 * This predicate should succeed if `Product` is the result of multiplying
 * `N0` and `N1` together. 
 *
 * Recall that multiplication is the same as repeated addition. You should
 * therefore use the `nat_plus / 3` predicate defined above in your solution.
 */

/**
 * nat_multiply( ? N0, ? N1, ? Product )
 *
 * Succeeds if `Product` is `N0` * `N1`, where `Product`, `N0`, and `N1` are
 * natural numbers represented with successor notation.
 */
nat_multiply( 0, _, 0 ).

nat_multiply( s( N0 ), N1, Product ) :-
	nat_multiply( N0, N1, PartProduct ),
	nat_plus( PartProduct, N1, Product ).


:- begin_tests( nat_multiply ).

test( nat_multiply_zero,
      [ X == 0 ]
    ) :-
    nat_multiply( 0, s( s( 0 ) ), X ),
	!.


test( nat_multiply_zero_natint,
      [ X == 0 ]
    ) :-
    nat_int( Four, 4 ),
    nat_multiply( 0, Four, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_zero_natint_2,
      [ X == 0 ]
    ) :-
    nat_int( Four, 4 ),
    nat_multiply( Four, 0, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_one,
      [ X == 7 ]
    ) :-
    nat_int( Seven, 7 ),
    nat_int( One, 1 ),
    nat_multiply( Seven, One, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_one_2,
      [ X == 9 ]
    ) :-
    nat_int( One, 1 ),
    nat_int( Nine, 9 ),
    nat_multiply( One, Nine, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_two, 
      [ X == 34 ]
    ) :-
    nat_int( A, 17 ),
    nat_int( B, 2 ),
    nat_multiply( A, B, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_nine_sq,
      [ X == 81 ]
    ) :-
    nat_int( Nine, 9 ),
    nat_multiply( Nine, Nine, Y ),
    nat_int( Y, X ),
    !.


test( nat_multiply_divide, 
      [ X == 3 ]
    ) :-
    nat_int( A, 27 ),
    nat_int( B, 81 ),
    nat_multiply( A, Y, B ),
    nat_int( Y, X ),
    !.


test( nat_multiply_divide2,
      [ X == 82 ]
    ) :-
    nat_int( A, 820 ),
    nat_int( B, 10 ),
    nat_multiply( Y, B, A ),
    nat_int( Y, X ),
    !.


:- end_tests( nat_multiply ).
     
:- run_tests( nat_multiply ).


/**
 * nat_lessthan( + N0, + N1 )
 *
 * Succeeds if `N0` < `N1`, where `N0` and `N1` are natural numbers
 * represented using successor notation.
 */
nat_lessthan( 0, s( N1 ) ) :-
    % Make sure that `N1` is actually a natural number, not just some term
    % with `s` at the root.
    nat( N1 ).

nat_lessthan( s( N0 ), s( N1 ) ) :-
    nat_lessthan( N0, N1 ).



/** nat_equal( + N0, + N1 )
 *
 * Succeeds if N0 == N1, where N0 and N1 are natural numbers represented using
 * successor notation.
 */
nat_equal( 0, 0 ).

nat_equal( s( N0 ), s( N1 ) ) :-
	nat_equal( N0, N1 ).



/** 
 * nat_fac( ? N, ? Fac )
 *
 * Succeeds if `Fac` is `N` factorial, where `Fac` and `N` are natural numbers
 * represented with successor notation.
 */
nat_fac( 0, s( 0 ) ).

nat_fac( s( N ), Fac ) :-
    nat_fac( N, Fac1 ),
    nat_multiply( s( N ), Fac1, Fac ).


/**
 * nat_fib( ? N, ? Fib )
 * 
 * Succeeds if `Fib` is the `N`th Fibonacci number, where `N` and `Fib` are
 * natural numbers represented with successor notation.
 */
nat_fib( 0, 0 ).

nat_fib( s( 0 ), s( 0 ) ).

nat_fib( N, Fib ) :-
    % Make sure that N > 1. 
    %
    % We could accomplish this by having `s( s( N ) )` as the first argument:
    % if N > 1, then N >= 2.
    %
    % For GCD below, you will want to just use `nat_lessthan`. 
    nat_lessthan( s( 0 ), N ),

    % Compute N - 1 and N - 2. In other words, we are finding a value `N1`
    % such that `N1` + 1 = `N` (similarly for `N2` + 2 = `N`).
    nat_plus( N1, s( 0 ), N ),
    nat_plus( N2, s( s( 0 ) ), N ),

    % Now compute the Fibonacci of `N` - 1 and `N` - 2.
    nat_fib( N1, Fib1 ),
    nat_fib( N2, Fib2 ),

    % Finally, add the results together to get Fib
    nat_plus( Fib1, Fib2, Fib ).





/**
 * nat_gcd( ? N0, ? N1, ? GCD )
 *
 * Succeeds if `GCD` is the greatest common divisor (using Euclid's algorithm)
 * of `N0` and `N1`, where `GCD`, `N0`, and `N1` are natural numbers
 * represented with successor notation.
 */
nat_gcd( 0, N1, N1 ).

nat_gcd( N0, 0, N0 ).



nat_gcd( N0, N1, GCD ) :-
	% Somewhat contrived; I'd rather have a predicate =nat_less_equal= for
	% this.
	(
		nat_equal( N0, N1 )
	;
		nat_lessthan( N0, N1 )
	),
	nat_plus( N0, New, N1 ),
	nat_gcd( N0, New, GCD ).

nat_gcd( N0, N1, GCD ) :-
	nat_lessthan( N1, N0 ),
	nat_plus( New, N1, N0 ),
	nat_gcd( New, N1, GCD ).



:- begin_tests( nat_gcd ).


test( nat_gcd_equal,
      [ X == s( s( s( s( 0 ) ) ) ) ]
    ) :-
    nat_gcd( s( s( s( s( 0 ) ) ) ), s( s( s( s( 0 ) ) ) ), X ),
	!.


test( nat_gcd_equal_natint,
      [ X == 4 ]
    ) :-
    nat_int( Four, 4 ), 
    nat_gcd( Four, Four, Y ), 
    nat_int( Y, X ),
    !.


test( nat_gcd_three,
      [ X == 3 ]
    ) :-
    nat_int( Six, 6 ), 
    nat_int( Nine, 9 ),
    nat_gcd( Six, Nine, Y ),
    nat_int( Y, X ),
    !.


test( nat_gcd_fifteen,
      [ X == 15 ]
    ) :-
    nat_int( A, 45 ),
    nat_int( B, 75 ),
    nat_gcd( A, B, Y ),
    nat_int( Y, X ),
    !.


:- end_tests( nat_gcd ).

:- run_tests( nat_gcd ). 

