/**
 * A non-DCG parser for our expression grammar, expressed using Wirth's EBNF
 * notation.
 *
 * This shows what the parser in =| expr_dcg.pl |= expands to. However,
 * because of the tedium of manually threading the input and output arguments,
 * it is advisable to make use of the DCG syntactic sugar.
 *
 * As a reminder, our expression grammar is as follows:
 *     Expr ::= Term { ("+" | "-" ) Term }.
 *      
 *     Term ::= Factor { ( "*" | "/" ) Factor }.
 *     
 *     Factor ::= "(" Expr ")"
 *             |  Digit.
 */


/**
 * expr( ExprTree, In : list, Out : list )
 *
 * Matches an Expr:
 *     Expr ::= Term { ( "+" | "-" ) Term }.
 *
 * The parse tree for the expression is bound to the first argument. This tree
 * is represented as a list of trees, owing to the repetition in the rule.
 */
expr( [ term( T ) | Exprs ], In, Out ) :-
	term( T, In, TermRest ),
	expr1( Exprs, TermRest, Out ).


% Observe that we do NOT have Wirth's EBNF notation built-in here (though it
% wouldn't be hard to add it). Thus, we expand the EBNF out to plain BNF
% ourselves, requiring this =| expr1 / 3 |= helper predicate.
expr1( [ plus( T ) | Exprs ], In, Out ) :-
	In = [ '+' | In1 ],
	term( T, In1, TermRest ),
	expr1( Exprs, TermRest, Out ).

expr1( [ minus( T ) | Exprs ], In, Out ) :-
	In = [ '-' | In1 ],
	term( T, In1, TermRest ),
	expr1( Exprs, TermRest, Out ).

% Ensuring mutual exclusion between the clauses of =| expr1 / 2 |= (as is done
% here) like here is not required, but sometimes help in error reporting.
% However, without some additional work, we generally lose reversibility
% (i.e., the ability to generate an input string from a completed parse tree).
%
%  *  More generally, we really should check the FOLLOW set for Expr1 here,
%     rather than just that the input is not something we can match above.
%
%  *  For error reporting, we could add another clause that simply prints out
%     an error message. Some finesse would be necessary to ensure that the
%     error clause is mutually exclusive with the others (so we don't get
%     spurious error messages) and to ensure that the rule remains reversible,
%     if we care about that.
expr1( [], In, In ) :-
	In \= [ '+' | _ ],
	In \= [ '-' | _ ].


/**
 * term( TermTree, In : list, Out : list )
 *
 * Matches a Term:
 *     Term ::= Factor { ( "*" | "/" ) Factor }.
 *
 * The parse tree for the term is bound to the first argument. This tree is
 * represented as a list of trees, owing to the repetition in the rule.
 */
term( [ factor( F ) | Terms ], In, Out ) :-
	factor( F, In, FactorRest ),
	term1( Terms, FactorRest, Out ).


% Again, we use a helper due to the expansion from EBNF into BNF.
term1( [ multiply( F ) | Terms],  In, Out ) :-
	In = [ '*' | In1 ],
	factor( F, In1, FactorRest ),
	term1( Terms, FactorRest, Out ).

term1( [ divide( F ) | Terms ], In, Out ) :-
	In = [ '/' | In1 ],
	factor( F, In1, FactorRest ),
	term1( Terms, FactorRest, Out ).

% Again, we are ensuring that our clauses are mutually exclusive. See 
% =| expr1 / 3 |= above.
term1( [], In, In ) :-
	In \= [ '*' | _ ],
	In \= [ '/' | _ ].



/**
 * factor( FactorTree, In : list, Out : list )
 *
 * Matches a Factor:
 *     Factor ::= "(" Expr ")"
 *             |  Digit
 *             .
 *
 * For simplicity, we rely on the helper =| digit / 1 |= to ensure that the
 * character matched by the second alternative is indeed a single digit. We
 * could easily extend this to full integers, such as through the 
 * =| int_digit_list / 2 |= predicate you wrote for the homework.
 */
factor( paren( E ), In, Out ) :-
	In = [ '(' | In1 ],
	expr( E, In1, ExprRest ),
	ExprRest = [ ')' | Out ].

factor( digit( D ), In, Out ) :-
	In = [ D | Out ],
	digit(D).


digit( D ) :-
	char_type( D, digit ).

