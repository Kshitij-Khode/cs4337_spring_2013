/**
 * A DCG-written parser for the expression grammar.
 *
 * The code written here will generally expand to the code in 
 * =| expr_parser.pl |=, with the exception of the epsilon production case in
 * =| expr1 / 3 |=. 
 *
 * We do not reproduce all of the commentary in =| expr_parser.pl |= here
 * (especially w.r.t. =expr1= and =term1=); you should read through that
 * document first.
 *
 * As a reminder, our expression grammar is as follows:
 *     Expr ::= Term { ("+" | "-" ) Term }.
 *      
 *     Term ::= Factor { ( "*" | "/" ) Factor }.
 *     
 *     Factor ::= "(" Expr ")"
 *             |  Digit.
 *
 * Because we are using DCG notation, observe that our predicates are
 * documented using the form 
 *
 *     rule( Arg0, Arg1, ... , Arg(N - 1) ) //
 *
 * to indicate that =rule= is a DCG predicate and takes two extra arguments
 * (making (N - 1) + 2 total) for the input and output lists.
 */


:- use_module( library( clpfd ) ).

/**
 * expr( ExprTree) //
 *
 * Matches an Expr:
 *     Expr ::= Term { ( "+" | "-" ) Term }.
 *
 * The parse tree for the term is bound to ExprTree. This tree is represented
 * as a list of trees, owing to the repetition in the rule.
 */
expr( [ term( T ) | Exprs ]) -->
	term( T ),
	expr1( Exprs ).


% Similarly to =| expr1 / 3 |= in =| parser.pl |=, this is a helper
% necessitated by the use of EBNF in the grammar rule.
expr1( [ plus( T ) | Exprs ] ) -->
	[ '+' ],
	term( T ),
	expr1( Exprs ).

expr1( [ minus( T ) | Exprs ] ) -->
	[ '-' ],
	term( T ),
	expr1( Exprs ).

% Here, we just match the epsilon production directly via the [] "terminal."
% We do not check against the FOLLOW set for Expr1, nor do we ensure mutual
% exclusion with =| expr1 // 1 |=, as we are actually going for a reversible
% parser here.
expr1( [] ) --> [].


/**
 * term( TermTree ) //
 *
 * Matches a Term:
 *     Term ::= Factor { ( "*" | "/" ) Factor }.
 *
 * The parse tree for the term is bound to TermTree. This tree is represented
 * as a list of trees, owing to the repetition in the rule.
 */
term( [ factor( F ) | Terms ] ) -->
	factor( F ),
	term1( Terms ).


% Again, a helper due to the EBNF expansion
term1( [ multiply( F ) | Terms] ) -->
	[ '*' ],
	factor( F ), 
	term1( Terms ).

term1( [ divide( F ) | Terms ] ) -->
	[ '/' ],
	factor( F ), 
	term1( Terms ).

term1( [] ) --> [].


/**
 * factor( FactorTree ) //
 *
 * Matches a Factor:
 *     Factor ::= "(" Expr ")"
 *             |  Digit
 *             .
 *
 * Observe the use of curly braces surrounding non-DCG (i.e., "plain Prolog")
 * code that we wish to execute.
 */
factor( paren( E ) ) -->
	[ '(' ],
	expr( E ),
	[ ')' ].

factor( digit( D ) ) -->
	[ D ],
	{ digit( D ) }.



digit( D ) :-
	char_type( D, digit ).





% =============================================================================
% The following is an example expression evaluator that processes the above
% parse trees. **YOU DO NOT NEED TO WRITE THIS FOR THE HOMEWORK!** This is
% simply for own edification.


evaluate( Input, Tree, Value ) :-
	expr( Tree, Input, [] ),
	eval_expr( Tree, Value ).

eval_expr( [ term( T ) | Exprs ], Value ) :-
	eval_term( T, TermValue ),
	eval_expr_rest( Exprs, TermValue, Value ).

eval_expr_rest( [ plus( T ) | Exprs ], PartialValue, Value ) :-
	eval_term( T, TermValue ),
	NewPartialValue #= PartialValue + TermValue,
	eval_expr_rest( Exprs, NewPartialValue, Value ).

eval_expr_rest( [ minus( T ) | Exprs ], PartialValue, Value ) :-
	eval_term( T, TermValue ),
	NewPartialValue #= PartialValue - TermValue,
	eval_expr_rest( Exprs, NewPartialValue, Value ).

eval_expr_rest( [], PartialValue, PartialValue ).


eval_term( [ factor( F ) | Terms ], Value ) :-
	eval_factor( F, FactorValue ),
	eval_term_rest( Terms, FactorValue, Value ).

eval_term_rest( [ multiply( F ) | Terms ], PartialValue, Value ) :-
	eval_factor( F, FactorValue ),
	NewPartialValue #= PartialValue * FactorValue,
	eval_term_rest( Terms, NewPartialValue, Value ).

eval_term_rest( [ divide( F ) | Terms ], PartialValue, Value ) :-
	eval_factor( F, FactorValue ),
	NewPartialValue #= PartialValue / FactorValue,
	eval_term_rest( Terms, NewPartialValue, Value ).

eval_term_rest( [], PartialValue, PartialValue ).


eval_factor( paren( E ), Value ) :-
	eval_expr( E, Value ).

eval_factor( digit( D ), Value ) :-
	atom_number( D, Value ).




% =============================================================================
% This is an example of using a DCG to match non-context-free grammars. Here,
% we are matching a^n b^n c^n, which as noted in class is context-free.
% However, because we can embed arbitrary Prolog code in our DCGs, we have the
% full power of a Turing machine at our disposal and can therefore easily
% check that the number of a's, b's, and c's are all equal.


blah -->
	as( A ),
	bs( B ),
	cs( C ),
	{ A #= B, B #= C }.

as( A ) -->
	[ 'a' ],
	as( A1 ),
	{ A #= A1 + 1 }.

as( 0 ) -->
	[].


bs( B ) -->
	[ 'b' ],
	bs( B1 ),
	{ B #= B1 + 1 }.

bs( 0 ) --> [].



cs( C ) -->
	[ 'c' ],
	cs( C1 ),
	{ C #= C1 + 1 }.

cs( 0 ) --> [].



