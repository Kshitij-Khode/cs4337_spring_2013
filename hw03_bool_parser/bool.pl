/* Author: TODO: WRITE YOUR NAME HERE
 * Email:  TODO: WRITE YOUR EMAIL HERE
 *
 * CS / CE 4337 Spring 2013 Sections 001, 002
 *
 * Assignment 2:    DCG Parser for a Boolean Expression Grammar
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
 * output lists. 
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
 * bool_expr // 0
 *
 * Matches a BoolExpr:
 *     BoolExpr ::= BoolConj { "or" BoolConj }.
 */

% TODO: WRITE YOUR CODE HERE



/**
 * bool_conj // 0
 * 
 * Matches a BoolConj:
 *     BoolConj ::= BoolLit { "and" BoolLit }.
 */

% TODO: WRITE YOUR CODE HERE



/**
 * bool_lit // 0
 *
 * Matches a BoolLit:
 *     BoolLit ::= [ "not" ] BoolPosLit.
 */

% TODO: WRITE YOUR CODE HERE



/**
 * bool_pos_lit // 0
 *
 * Matches a BoolPosLit:
 *     BoolPosLit ::= "true"
 *                 |  "false"
 *                 |  "(" BoolExpr ")"
 *                 .
 */

% TODO: WRITE YOUR CODE HERE

