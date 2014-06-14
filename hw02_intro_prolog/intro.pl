/* Author: TODO: Kshitij Khode
 * Email:  TODO: kxk113730@utdallas.edu
 *
 * CS / CE 4337 Spring 2013 Sections 001, 002
 *
 * Assignment 2:    Introduction to Prolog
 * Assigned:        Monday, 2013-04-08
 * Due:             Tuesday, 2013-04-16
 * Estimated SLOC:  40
 *
 *
 * **WARNING!**
 *
 * Before submission, make sure to search the file for "TODO" to make sure you
 * have completed all tasks!
 *
 * **end WARNING!!!**
 *
 *
 * Notation
 * ========
 *
 * You will see Wiki-style comments throughout this documentation. See 
 *
 *     http://www.swi-prolog.org/pldoc/package/pldoc.html 
 *
 * for more information about them.
 *
 * In the comments for each predicate, you will see references to predicates
 * as =|pred_name / N|=, where _N_ is an integer. This is how predicates are
 * typically referred to / identified in Prolog --- the _N_ is the number of
 * arguments that the predicate takes. Two clauses are considered part of the
 * same predicate if they use the same name and take the same number of
 * arguments. Remember that Prolog does not strictly enforce a type system
 * like Haskell does, so there is no built-in way to differentiate predicates
 * based on some form of argument typing.
 *
 * The header comment for each predicate will begin with a line such as
 *
 *     pred_name( + InArg    : typename, 
 *                - OutArg   : typename, 
 *                ? InOutArg : typename
 *              )
 *
 *  *  A =| + |= in front of the argument means that you should assume that
 *     the argument is grounded (i.e., not a variable) and therefore serves as
 *     the "input" to the predicate.
 *
 *  *  A =| - |= in front of the argument means that you should assume that
 *     the argument is a variable, and that binding that argument serves to
 *     "return" a value from the predicate.
 *
 *  *  A =| ? |= in front of the argument means that the argument may or may
 *     not be grounded. This is used with the last two problems to indicate
 *     that the predicate is reversible. For these, focus on just defining the
 *     relationship between the arguments; Prolog's pattern matching and
 *     backtracking will take care of the reversibility for you.
 *
 *  *  The typename is just for documentation and is not enforced by the
 *     Prolog system **in any way!**
 *
 * These are only included in the comments for your convenience. If they are
 * confusing, then it is safe to ignore them. See the PLDoc link above for
 * more information. 
 *
 * Most importantly, **do NOT put them in your code!**
 *
 *
 * Running Your Code
 * =================
 *
 * We will be using SWI Prolog (http://www.swi-prolog.org/) for this
 * assignment. You can start the Prolog interpreter with the command
 *
 *     swipl
 *
 * After the interpreter has started, you will be presented with a "query
 * prompt," written as =| ?- |=. You can type queries to the Prolog system
 * here (essentially the function calls from the Lisp and Haskell
 * interpreters). In this document, lines that begin with =| ?- |= can be typed at
 * this prompt (without the =| ?- |= characters of course). 
 *
 * Recall that queries (and predicates) are terminated with a period =| . |=.
 * Also recall that there should be no spaces between the predicate / function
 * symbol name and the opening parenthesis.
 *
 * You can load your code with the following built-in:
 *
 *     ?- consult( 'hw4_prolog.pl' ).
 *
 * This will load your file and execute the test cases (see below). Note that
 * you can also load your file with 
 * 
 *     ?- consult( hw4_prolog ).
 *
 * If you wish to supply the extension, the quotes are required.
 *
 * After editing, you can re-load your file with the command
 *
 *     ?- reconsult( 'hw4_prolog.pl' ).
 *
 * or 
 * 
 *     ?- reconsult( hw4_prolog ).
 *
 * However, as noted in class, I have had problems with reconsulting files in
 * the past. You may wish to exit SWIPL (via the =halt= builtin or Ctrl - D in
 * Unix) to make sure you have the latest code loaded, especially when bug
 * hunting.
 *
 * 
 * Test Cases
 * ==========
 *
 * Each problem is followed by a number of test cases, written using the
 * PLUnit unit testing framework. To run them, you will have to also download
 * the =plunit.pl= file from the repository and place it in the same directory as
 * your file. 
 *
 * You can comment out lines of the following form to disable unit testing of
 * unwritten code:
 *
 *     :- run_tests( some_test_suite ).
 *
 * This will prevent the interpreter from executing the test cases when the
 * file is loaded. Remember to uncomment them when you are ready to test your
 * code!
 *
 * You can see the format of the test cases in a comment above the first suite
 * (for `collatz_list`).
 *
 * 
 * Tips
 * ====
 *
 *  *  Remember that goals (i.e., predicate calls / "statements" to be
 *     executed) are separated by the conjunction operator =| , |=.
 *
 *  *  Remember that there should be no spaces between predicate names /
 *     function symbols and the associated open parenthesis. Spaces after the
 *     open parenthesis are fine (I've used them for readability) but not
 *     required.
 *
 *         ?- X = f( a ).
 *         X = f(a).
 *
 *         ?- X = f ( a ).
 *         ERROR: Syntax error: Operator expected
 *         ERROR: X = f 
 *         ERROR: ** here **
 *         ERROR: ( a ) . 
 *
 *  *  If you are getting syntax errors with the message "operator expected,"
 *     check for violations of the above two rules.
 *
 *
 *
 *  *  Remember that there is no notion of a "return value" in Prolog ---
 *     *everything* is passed as arguments, even information we wish to
 *     "return." We "return" this information by binding the corresponding
 *     argument to the desired value.
 *
 *  *  Remember that =| = |= is not equality, but rather unification, which seeks
 *     to bind any variables in the two arguments to make the arguments
 *     identical. 
 *
 *         ?- X = 1 + 2.
 *         X = 1 + 2.
 *
 *         ?- f( X, g( b ) ) = f( a, Y ).
 *         X = a,
 *         Y = g(b).
 *
 *  *  Due to the above two rules, the following code will have nowhere near
 *     the desired result:
 *
 *         ?- X = sum_list( [ 1, 2, 3, 4 ] ).       % Expecting X = 10
 *         X = sum_list([ 1, 2, 3, 4]).
 *
 *     Instead, write:
 *     
 *         ?- sum_list( [ 1, 2, 3, 4 ], X ).
 *         X = 10.
 *
 *
 *  *  Remember that when a predicate is invoked, the actual parameters (from
 *     the call) are unified with the formal parameters (in the predicate
 *     definition). This lets us perform pattern matching. See the examples
 *     for more information.
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




/* Problem 0: Collatz List
 * ============================================================================
 *
 * Write a predicate =|collatz_list / 2|= that takes an integer _N_ and a list
 * _L_ and succeeds if _L_ is the Collatz sequence starting at _N_.
 *
 * You will need 3 clauses to accomplish this: one for the base case, and two
 * for the inductive cases. For these inductive cases, you should ensure that
 * _N_ > 1 --- i.e., your first statement should be:
 *     
 *     N > 1
 *
 * 
 * Tips
 * ----------------------------------------------------------------------------
 *  
 * Observe from the declaration below that the first argument _N_ is assumed
 * to be grounded; you do not have to worry about the case where _N_ is left a
 * variable. In other words, this does not have to be a reversible predicate.
 *
 * Further observe that _L_ is assumed to be unbounded --- in other words, you
 * should assume that the caller is passing a variable for _L_. Just worry
 * about building the list correctly.
 *
 *
 * Recall that the building and splitting up a list is done the same way:
 * 
 *     [ H | T ] 
 *
 * will build a list with _H_ as its head and _T_ as its tail. Therefore, you
 * can use an expression of the form =| [ H | T ] |= in the second argument
 * position to build the list for _L_. See the above examples (e.g.,
 * =|smaller_elems / 3|=) for how to do this.
 *
 *
 * Recall that =| = |= is not mathematical equality. Instead:
 *  *  To compare two expressions for mathematical equality in pure Prolog,
 *     use =| =:= |=; see =|expr_list_eq / 2|= above for an example.
 *
 *  *  To bind a variable to the result of an arithmetic expression, use =is=;
 *     see =|sum_list / 2|= for an example.
 *
 *  *  For the ambitious, the =| #= |= operator serves a similar, though more
 *     powerful, function, and can be used instead of both =| =:= |= and =is=. See
 *     =|num_int / 2|= for details.
 *
 *
 * Modulus can be performed with the *infix* =mod=, while integer division is
 * written as an infix =//=; see http://www.swi-prolog.org/man/arith.html. For
 * example:
 *
 *     ?- 4 mod 2 =:= 3 // 2.   % 2 =:= 1
 *     false
 *
 *     ?- 6 mod 4 =:= 5 // 2.   % 2 =:= 2
 *     true
 */

/**
 * collatz_list( + Num : integer, - List : list )
 *
 * Succeeds if List is the Collatz sequence beginning at Num.
 */

% TODO: WRITE YOUR CODE HERE

collatz_list(1, [1]).

collatz_list(N, [N|T]) :-
    N #> 1,
	N mod 2 =:= 0,
    H is N // 2,
    collatz_list(H, T).

collatz_list(N, [N|T]) :-
	N #> 1,
	H is 3 * N + 1,
    collatz_list(H, T).



/* These test cases are formatted as follows:
 *
 *     test( Name,
 *           Result
 *         ) :-
 *         TestCase,
 *         !.
 * 
 * Result will either be of the form =|Var == Value|=, =true=, or =fai=`. The
 * last two are self-explanatory --- the =TestCase= code should either succeed
 * or fail, but will not bind any variables.
 * 
 * The other form uses the "identical" operator =| == |=, which succeeds if its
 * arguments are identical (as opposed to unification, which succeeds and
 * binds variables to make the two arguments identical). 
 *
 * The =| ! |= at the end of the test case is called a "cut" and serves to
 * tell the Prolog interpreter to ignore any other search paths (known as
 * "choice points") that were created between =| :- |= and =| ! |=. 
 *
 * ** DO NOT, UNDER ANY CIRCUMSTANCES, USE EITHER OF THESE OPERATORS IN YOUR
 * CODE!!! **
 *
 * The purpose of the =| == |= operator here is to cause the test case to fail
 * if you accidentally forget to bind the associated variable.
 *
 * The purpose of the cut `!` is to supress warnings that the unit testing
 * framework will emit because the predicates you are writing may fail,
 * depending on the query.
 *
 * A bit more about these operators will be covered on Monday.
 */
:- begin_tests(collatz_list).

% Base case: collatzList of 1 should ground the second argument to [ 1 ].
test( collatz_list_1, 
      [ L == [ 1 ] ] 
    ) :-
    collatz_list( 1, L ),
    !.

% Instead of error reporting, we'll just rely on Prolog's built-in failure
% system to avoid trying to evaluate the case for `Num < 1`.
test( collatz_list_0,
      [ fail ] 
    ) :-
    collatz_list( 0, _L ),
    !.

% Make sure that collatz_list of 7 gives the expected result
test( collatz_list_7,
      [ L == [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1] ]
    ) :-
    collatz_list( 7, L ),
    !.

% Check collatz_list of 51.
test( collatz_list_51,
      [ L == [51, 154, 77, 232, 116, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26,
              13, 40, 20, 10, 5, 16, 8, 4, 2, 1] ]
    ) :-
    collatz_list( 51, L ),
    !.
    
:- end_tests( collatz_list ).


:- run_tests( collatz_list ).


/* Problem 1: Subset
 * ===========================================================================
 *
 * Recall that we can represent sets as lists that contain no duplicate
 * elements. Suppose we have two such lists, _S0_ and _S1_. 
 *
 * Write a predicate =|my_subset / 2|= that takes two _S0_ and _S1_ and
 * succeeds if _S0_ is a subset of _S1_; that is, if every element of _S0_ is
 * also an element of _S1_.
 *
 * Recall that the empty set (represented by the empty list) is a subset of
 * every set, including itself.
 *
 * The lists may contain any terms, but for simplicity, you may assume that it
 * does not contain variables (i.e., both lists are grounded).
 *
 * For checking membership, you should use the built-in =|member / 2|=
 * predicate. This performs the same job as the =|element / 2|= example
 * provided in class.
 *
 *
 * UPDATE (2013 - 04 - 15): Some students have noted that =|subset / 2|= is
 * the name of a predicate provided by SWI Prolog and auto-loaded when the
 * interpreter is started. This means that all of the unit tests will pass,
 * even when you have not defined any of your own code. Thus, the name of the
 * predicate has been renamed to =|my_subset / 2|=.
 */

/**
 * my_subset( + S0 : list, + S1 : list )
 *
 * Succeeds if S0 is a subset of S1. 
 *
 * Neither S0 nor S1 should contain duplicates.
 */

% TODO: WRITE YOUR CODE HERE

my_subset([], _N).

my_subset([H | T], L) :-
	member(H, L),
	my_subset(T, L).



:- begin_tests( my_subset ).

test( my_subset_empty_empty,
      true
    ) :-
    my_subset( [], [] ),
    !.

test( my_subset_sameorder_true,
      true 
    ) :-
    my_subset( [ a, b ], [ a, b, c, d ] ),
    !.

test( my_subset_unordered_true,
      true
    ) :-
    my_subset( [ a, b, c, d, 2 ], [ 1, 2, d, c, b, a, 3, 4 ] ),
    !.

test( my_subset_nonempty_empty,
      fail
    ) :-
    my_subset( [ 1, 2, c, d ], [] ),
    !.

test( my_subset_close_fail,
      fail
    ) :-
    my_subset( [ a, b, c, d, 2 ], [ 1, 2, c, b, a, 3, 4 ] ),
    !.

:- end_tests( my_subset ).

:- run_tests( my_subset ).



/* Problem 2: Intersection
 * ===========================================================================
 *
 * Write a predicate =|my_intersection / 3|= that takes three sets
 * (represented as lists, see =|my_subset / 2|=) _S0_, _S1_, and _S2_ and
 * succeeds if _S2_ is the intersection of _S0_ and _S1_ (i.e., all elements
 * that are contained in both _S0_ and _S1_ are also contained in _S2_).
 *
 * Again, you should use =|member / 2|= for performing membership checks.
 *
 * You may assume that _S0_ and _S1_ are ground and that _S2_, on the initial
 * call, is an unbound variable. With a bit of thought, you should be able to
 * conclude that having a fully reversible intersection predicate would be
 * difficult and not of much value.
 *
 *
 * UPDATE (2013 - 04 - 15): Some students have noted that =|intersection / 3|=
 * is the name of a predicate provided by SWI Prolog and auto-loaded when the
 * interpreter is started. This means that all of the unit tests will pass,
 * even when you have not defined any of your own code. Thus, the name of the
 * predicate has been renamed to =|my_intersection / 2|=.
 */

/**
 * my_intersection( + S0 : list, + S1 : list, - S2 : list )
 *
 * Succeeds if S2 is the intersection of S0 and S1. 
 *
 * None of S0, S1, or S2 should contain duplicates.
 *
 * You should take measures to ensure that your clauses are mutually
 * exclusive, meaning that only one clause should succeed for a given input.
 *
 * Remember that an outright cut is forbidden, but the =| \+ |= operator
 * should be of use.
 */

% TODO: WRITE YOUR CODE HERE

my_intersection([X|Y],M,[X|Z]) :- 
	member(X,M), 
	my_intersection(Y,M,Z).

my_intersection([X|Y],M,Z) :- 
	\+ member(X,M),
	my_intersection(Y,M,Z).

my_intersection([],_M,[]).



:- begin_tests( my_intersection ).


test( my_intersection_empty_s0,
      [ Inter == [] ]
    ) :-
    my_intersection( [], [ 1, 2, 3, a, b, c ], Inter ),
    !.

test( my_intersection_empty_s1, 
      [ Inter == [] ]
    ) :-
    my_intersection( [ 1, 2, 3, a, b, c ], [], Inter ),
    !.

test( my_intersection_single,
      [ Inter == [ a ] ]
    ) :-
    my_intersection( [ 1, 2, 3, a, b, c ], [ a ], Inter ),

    !.

test( my_intersection_multi,
      [ SortedInter == [ 3, 4, a, b ] ]
    ) :-
    my_intersection( [ a, 1, b, 4, 3, 2 ], [ 4, b, c, d, 3, a ], Inter ),
    % Additional processing so that the comparison above is easier, but the
    % ordering is not forced
    
    % Ensure that the my_intersection is ground
    ground( Inter ),

    % Sort the my_intersection. 
    % WARNING: =|sort / 2|= removed duplicates, so use =|msort / 2 |= here!
    msort( Inter, SortedInter ),
    !.

% Ensure that the full my_intersection is returned. 
%
% NOTE: If this fails, make sure that you only one of your recursive clauses
% can be applied for the same arguments.
test( my_intersection_full,
      [ fail ]
    ) :-
    my_intersection( [ a, b, c, d ], [ a, b, c, f, g ], [ a, b ] ),
    !.


:- end_tests( my_intersection ).

:- run_tests( my_intersection ).


/* Problem 3: Convert between a list of digits and an integer.
 * ============================================================================
 *
 * Write a predicate =|int_digit_list / 2|= that takes an integer _Num_ and a
 * list of digits _Digits_ and succeeds if _Digits_ contains the digits of
 * _Num_ in the proper order.
 *
 * **This predicate MUST be reversible!** In other words, you should be able
 * to pass a variable for either _Num_ or _Digits_ (or both) and obtain the
 * correct answer. 
 *
 * For this reason, you should NOT use the standard Prolog math functions
 * (namely =is=) and instead use the CLP(FD) facilities (e.g., =| #= |=).
 *
 * I would suggest taking the approach of writing a tail-recursive helper
 * predicate, similar to =|sum_list_tailrec / 2|= example. I would also note
 * that you may have to put constraints on the elements of _Digits_ to ensure
 * the correct behavior. **Have the CLP(FD) documentation available while
 * working on this one.**
 */

/**
 * int_digit_list( ? Num : integer, ? Digits : list )
 *
 * Succeeds if Digits contains the digits of Num in the proper order.
 */

% TODO: WRITE YOUR CODE HERE

int_digit_list(_X, []) :- false.

int_digit_list(X,Y):-
	X #>= 0,
	int_digit_list2(X,Z),
	reverse(Z, Z1),
	length(Y,L),
	add(L,Y,0,X),
	compare(Y,Z1),
	no_negative(Y).
	
no_negative([H|_T]) :-
	H #>= 0.

int_digit_list2(0,[]).

int_digit_list2(N, [H|T]):-
	H #= N mod 10,
	N1#=N/10,
	int_digit_list2(N1,T).	

compare([],[]).

compare([A|B], [C|D]):- 
	A #= C,
	compare(B,D).

add(L,[A|B], PartialSum, Sum):-
		L#>0,
		NewPartial#=10^(L-1)*A+PartialSum,
		L1 #= L-1,
		add(L1, B, NewPartial, Sum).

add(0, [], PartialSum, PartialSum).


:- begin_tests( int_digit_list ).


test( int_digit_list_1,
      [ Num == 1 ]
    ) :-
    int_digit_list( Num, [ 1 ] ),
    !.

test( int_digit_list_neg1,
      [ fail ]
    ) :-
    int_digit_list( -1, _Num ),
    !.

test( int_digit_list_12345,
      [ Num == 12345 ]
    ) :-
    int_digit_list( Num, [ 1, 2, 3, 4, 5 ] ),
    !.

test( int_digit_list_12034,
      [ Num == 12034 ]
    ) :-
    int_digit_list( Num, [ 1, 2, 0, 3, 4 ] ),
    !.

test( int_digit_list_52138_list,
      [ Digits == [ 5, 2, 1, 3, 8 ] ]
    ) :-
    int_digit_list( 52138, Digits ),
    !.

test( int_digit_list_90210_list,
      [ Digits == [ 9, 0, 2, 1, 0 ] ]
    ) :-
    int_digit_list( 90210, Digits ),
    !.


:- end_tests( int_digit_list ).

:- run_tests( int_digit_list ).

