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
 *     E ::= T { ("+" | "-" ) T }.
 *      
 *     T ::= F { ( "*" | "/" ) F }.
 *     
 *     F ::= "(" E ")"
 *        |  Digit.
 *
 * Because we are using DCG notation, observe that our predicates are
 * documented using the form 
 *
 *     rule( Arg0, Arg1, ... , Arg(N - 1) ) //
 *
 * to indicate that =rule= is a DCG predicate and takes two extra arguments
 * (making (N - 1) + 2 total) for the input and output lists.
 */


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

