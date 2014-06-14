/* Author: Kshitij Khode
 * Email:  kxk113730@utdallas.edu
 *
 * CS / CE 4337 Spring 2013 Sections 001, 002
 *
 * Assignment 3:    DCG Parser for a Boolean Expression Grammar
 * Assigned:        Monday, 2013-04-22
 * Due:             Sunday, 2013-04-28
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
 * Notation, Test Cases, Running your Program
 * ==========================================
 *
 * Please see the HW 02 file =intro.pl= for information about running your
 * program, using the test cases, and the input-output notation (also called
 * the "mode" of the predicate) used here.
 *
 * However, one notational addition should be noted. Typically when using DCG
 * rules, the arity will be written as 
 * =|rule( Arg0, Arg1, ... , Arg(N - 1) ) //|= (notice the double slash) to
 * indicate that the predicate is actually a DCG rule and therefore takes 
 * (N - 1) + 2 arguments, where the additional 2 arguments are for the input /
 * output lists. In this case, there are no additional arguments, resulting in
 * the notation 
 *
 *     rule_name //
 *
 *
 * Assignment
 * ==========
 *
 * Recall Wirth's EBNF notation: {} denotes repetition, [] denotes optional.
 * See Wirth's paper, linked on my website.
 *
 * You are asked to write a basic DCG parser for the following grammar:
 *
 *     BoolExpr ::= BoolConj { "or" BoolConj }.
 *
 *     BoolConj ::= BoolLit { "and" BoolLit }.
 *
 *     BoolLit ::= [ "not" ] BoolPosLit.
 *
 *     BoolPosLit ::= "true"
 *                 |  "false"
 *                 |  "(" BoolExpr ")"
 *                 .
 *
 * Test cases are present for each rule, and the grammar is reproduced below
 * for your convenience. Because the grammar is cyclic, you may wish to tackle
 * the problems in the following order:
 *
 *  *  Write a predicate for BoolPosLit, leaving off the last clause (i.e., just
 *     matching "true" and "false".
 *
 *  *  Write a predicate for BoolLit, then BoolConj, then BoolExpr
 *     (essentially in reverse order).
 *
 *  *  Extend BoolPosLit to match parenthesized BoolExpr's.
 *
 * To support this development, there are separate test case sets for
 * BoolPosLit --- one that matches "true" and "false" and another for
 * parenthesized BoolExpr's.
 *
 * Because of time constraints, you are only being asked to write a basic
 * parser that simply accepts (succeeds) or rejects (fails) on a given input,
 * rather than producing a parse tree.
 *
 * There is no auxiliary code required here. I have therefore not explicitly
 * denoted individual "problems."
 *
 * DCG Rules
 * =========
 *
 * Recall the DCG notation, which is a syntactic sugar for writing parsers in
 * Prolog. 
 *  *  First, they add two parameters to your predicate for the input
 *     and output lists. 
 *
 *  *  Second, terminals can be placed in brackets (e.g., =| [ 'true' ] |=). 
 *      *  Recall that an epsilon production can be denoted by =| [] |=.
 *
 *      *  Recall that we are using atoms for the tokens, which are surrounded
 *         by single quotes (e.g., the atom =| 'true' |= used above). DO NOT
 *         USE DOUBLE-QUOTED STRINGS IN THIS ASSIGNMENT!!!!
 *
 *  *  Finally, non-DCG code can be surrounded by curly braces (e.g., 
 *     =| { digit( X ) } |=), **though this is not required and should not be
 *     used for this assignment.** 
 *
 *
 * Tips
 * ====
 *
 *  *  Remember: while the rules above start with capital letters, the Prolog
 *     predicates / rules MUST start with a lowercase letter.
 *
 *  *  Remember that the use of brackets and curly braces in DCGs are
 *     different than their use in Wirth's EBNF. Keep those contexts clear and
 *     distinct in your mind when approaching the homework!
 *
 *  *  Remember that you will have to convert the EBNF back to BNF in order to
 *     write the DCG rules. This **will** require the use of helper
 *     predicates, which are not explicitly listed in the documentation.
 */

% If you do not want colored output when loading your file, comment out the
% following line:

:- use_module( library( ansi_term ) ).


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



/**
 * bool_expr //
 *
 * Matches a BoolExpr:
 *     BoolExpr ::= BoolConj { "or" BoolConj }.
 */

% TODO: WRITE YOUR CODE HERE

bool_expr --> bool_conj, ['or'], bool_conj, bool_expr2.
bool_expr --> bool_conj.

bool_expr2 --> ['or'], bool_conj, bool_expr2.
bool_expr2 -->[].

/**
 * bool_conj //
 * 
 * Matches a BoolConj:
 *     Bool_conj ::= Bool_lit { "and" BoolLit }.
 */

% TODO: WRITE YOUR CODE HERE

bool_conj --> bool_lit, ['and'], bool_lit, bool_conj2.
bool_conj --> bool_lit.

bool_conj2 --> ['and'], bool_lit, bool_conj2.
bool_conj2 --> [].

/**
 * bool_lit //
 *
 * Matches a BoolLit:
 *     BoolLit ::= [ "not" ] BoolPosLit.
 */

% TODO: WRITE YOUR CODE HERE

bool_lit --> ['not'], bool_pos_lit.
bool_lit --> bool_pos_lit.

/**
 * bool_pos_lit //
 *
 * Matches a BoolPosLit:
 *     BoolPosLit ::= "true"
 *                 |  "false"
 *                 |  "(" BoolExpr ")"
 *                 .
 */

% TODO: WRITE YOUR CODE HERE

bool_pos_lit --> ['true'].
bool_pos_lit --> ['false'].
bool_pos_lit --> ['('], bool_expr, [')'].


% =============================================================================
% Unit tests
%
% Because the grammar is cyclic, we need to define our unit tests after *all*
% of the rules have been defined. For simplicity, we just run them all in one
% shot at the end; you can comment out that line and "comment-in" the 
% =| run_tests / 1 |= lines to run individual suites.
%
% The tests for =| bool_expr // 0 |= is fairly exhaustive, since that's our
% start symbol. The others mainly test their individual features. If
% something's not working, use =trace=; you can also add unit tests to this
% batch, of course.
%
% Note that the parser, if written properly, will succeed if a prefix of the
% string is a member of the language; the remainder will be bound to the
% "output" parameter (named _Out_ below). Because the empty string is not a
% member of the BoolExpr language (and likewise for the other main rules), it
% is possible that the prefix may be *part* of a string in the language, but
% is missing some tokens. In such cases, the parse should fail.

:- begin_tests( bool_expr ).

test( empty,
	  [ fail ]
	) :-
	bool_expr( [], _ ),
	!.

test( true_case,
	  [ Out == [] ]
	) :-
	bool_expr( [ true ], Out ),
	!.

test( false_case,
	  [ Out == [] ]
	) :-
	bool_expr( [ false ], Out ),
	!.

test( true_false,
	  [ Out == [ false ] ]
	) :-
	bool_expr( [ true, false ], Out ),
	!.

test( true_or_false,
	  [ Out == [] ]
	) :-
	bool_expr( [ true, or, false ], Out ),
	!.

test( true_or_false_or_true,
	  [ Out == [] ]
	) :-
	bool_expr( [ true, or, false, or, true ], Out ),
	!.

test( false_and_false,
	  [ Out == [] ]
	) :- 
	bool_expr( [ false, and, false ], Out ),
	!.


test( false_and_false_and_true, 
	  [ Out == [] ]
	) :-
	bool_expr( [ false, and, false, and, true ], Out ),
	!.

test( false_or_true_and_true,
	  [ Out == [] ]
	) :-
	bool_expr( [ false, or, true, and, true ], Out ),
	!.

test( true_and_false_or_true_and_false,
	  [ Out == [] ]
	) :-
	bool_expr( [ true, and, false, or, true, and, false ], Out ),
	!.

test( open_true,
	  [ fail ]
	) :-
	bool_expr( [ '(', true ], _Out ),
	!.

test( open_true_close,
	  [ Out == [] ]
	) :-
	bool_expr( [ '(', true, ')' ], Out ),
	!.

test( open_true_and_true_close_or_false,
	  [ Out == [] ]
	) :-
	bool_expr( [ '(', true, and, true, ')', or, false ], Out ),
	!.

:- end_tests( bool_expr ).

%:- run_tests( bool_expr ).




:- begin_tests( bool_conj ).

test( true_and_true, 
	  [ Out == [] ]
	) :-
	bool_conj( [ true, and, true ], Out ),
	!.

test( false_and_true_and_true,
	  [ Out == [] ]
	) :-
	bool_conj( [ false, and, true, and, true ], Out ),
	!.

test( true_and_not_false_and_not_open_not_false_and_true_close,
	  [ Out == [] ]
	) :-
	bool_conj( [ true, and, not, false, and, '(', not, false, and, true, ')' ], Out ),
	!.

test( not_false_and_not_false,
	  [ Out == [] ]
	) :-
	bool_conj( [ not, false, and, not, false ], Out ),
	!.

test( true_and_false_and_true,
	  [ Out == [ or, true ] ]
	) :-
	bool_conj( [ true, and, false, or, true ], Out ),
	!.

test( true_and_false_and,
	  [ Out == [ and ] ]
	) :-
	bool_conj( [ true, and, false, and ], Out ),
	!.

:- end_tests( bool_conj ).

%:- run_tests( bool_conj ).




:- begin_tests( bool_lit ).

test( not_open_not_true,
	  [ fail ]
	) :-
	bool_lit( [ not, '(', not, true ], _Out ),
	!.

test( not_open_not_false_close,
	  [ Out == [] ]
	) :-
	bool_lit( [ not, '(', not, false, ')' ], Out ),
	!.

test( open_true,
	  [ fail ]
	) :-
	bool_lit( [ '(', true ], _Out ),
	!.

test( open_true_close,
	  [ Out == [] ]
	) :-
	bool_lit( [ '(', true, ')' ], Out ),
	!.

test( open_true_and_not_true_close_or_false,
	  [ Out == [ or, false ] ]
	) :-
	bool_lit( [ '(', true, and, not, true, ')', or, false ], Out ),
	!.

:- end_tests( bool_lit ).

%:- run_tests( bool_lit ).




:- begin_tests( bool_lit_no_paren ).

test( true,
	  [ Out == [] ]
	) :-
	bool_lit( [ true ], Out ), 
	!.

test( false,
	  [ Out == [] ]
	) :-
	bool_lit( [ false ], Out ),
	!.

test( true_close_and_false,
	  [ Out == [ ')', and, false ] ]
	) :-
	bool_lit( [ true, ')', and, false ], Out ),
	!.

test( false_and_true,
	  [ Out == [ and, true ] ]
	) :-
	bool_lit( [ false, and, true ], Out ),
	!.

test( not_true, 
	  [ Out == [] ]
	) :-
	bool_lit( [ not, true ], Out ),
	!.

test( not_not_true,
	  [ fail ]
	) :-
	bool_lit( [ not, not, true ], _Out ),
	!.

:- end_tests( bool_lit_no_paren ).

%:- run_tests( bool_lit_no_paren ).




:- begin_tests( bool_pos_lit_no_paren ).

test( true,
	  [ Out == [] ]
	) :-
	bool_pos_lit( [ true ], Out ), 
	!.

test( false,
	  [ Out == [] ]
	) :-
	bool_pos_lit( [ false ], Out ),
	!.

test( true_close_and_false,
	  [ Out == [ ')', and, false ] ]
	) :-
	bool_pos_lit( [ true, ')', and, false ], Out ),
	!.

test( false_and_true,
	  [ Out == [ and, true ] ]
	) :-
	bool_pos_lit( [ false, and, true ], Out ),
	!.

:- end_tests( bool_pos_lit_no_paren ).

%:- run_tests( bool_pos_lit_no_paren ).




:- begin_tests( bool_pos_lit_with_paren ).

test( open_true,
	  [ fail ]
	) :-
	bool_pos_lit( [ '(', true ], _Out ),
	!.

test( open_true_close,
	  [ Out == [] ]
	) :-
	bool_pos_lit( [ '(', true, ')' ], Out ),
	!.

test( open_true_and_true_close_or_false,
	  [ Out == [ or, false ] ]
	) :-
	bool_pos_lit( [ '(', true, and, true, ')', or, false ], Out ),
	!.

:- end_tests( bool_pos_lit_with_paren ).

%:- run_tests( bool_pos_lit_with_paren ).




:- run_tests.
