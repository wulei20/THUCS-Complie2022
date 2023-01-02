"""
Module that defines a parser using `ply.yacc`.
Add your own parser rules on demand, which can be accomplished by:

1. Define a global function whose name starts with "p_".
2. Write the corresponding grammar rule(s) in its docstring.
3. Complete the function body, which is actually a syntax base translation process.
    We're using this technique to build up the AST.

Refer to https://www.dabeaz.com/ply/ply.html for more details.
"""


import ply.yacc as yacc

from frontend.ast.tree import *
from frontend.lexer import lex
from utils.error import *

tokens = lex.tokens
error_stack = list[DecafSyntaxError]()


def unary(p):
    p[0] = Unary(UnaryOp.backward_search(p[1]), p[2])


def binary(p):
    if p[2] == BinaryOp.Assign.value:
        p[0] = Assignment(p[1], p[3])
    else:
        p[0] = Binary(BinaryOp.backward_search(p[2]), p[1], p[3])


def p_empty(p: yacc.YaccProduction):
    """
    empty : 
    """
    pass


def p_program(p):
    """
    program : program function
        |   program declaration Semi
    """
    if p[2] is not NULL:
        p[1].children.append(p[2])
    p[0] = p[1]
    
def p_program_empty(p):
    """
    program : empty
    """
    p[0] = Program()

def p_type(p):
    """
    type : Int
    """
    p[0] = TInt()


def p_function_def(p):
    """
    function : type Identifier LParen paramlist RParen LBrace block RBrace
    """
    p[0] = Function(p[1], p[2], p[4], p[7])

def p_function_decl(p):
    """
    function : type Identifier LParen paramlist RParen Semi
    """
    p[0] = Function(p[1], p[2], p[4])

def p_paramlist(p):
    """
    paramlist : paramlist Comma type Identifier paramindex
    """
    if not p[5]:
        p[1].append(Parameter(p[3], p[4]))
    else:
        p[1].append(Parameter(p[3], IndexExpr(p[4], p[5], True)))
    p[0] = p[1]

def p_paramlist_empty(p):
    """
    paramlist : empty
    """
    p[0] = []

def p_paramlist_single(p):
    """
    paramlist : type Identifier paramindex
    """
    if not p[3]:
        p[0] = [Parameter(p[1], p[2])]
    else:
        p[0] = [Parameter(p[1], IndexExpr(p[2], p[3], True))]

def p_paramindex_empty(p):
    """
    paramindex : empty
    """
    p[0] = []

def p_paramindex_single(p):
    """
    paramindex : LBracket RBracket
    """
    p[0] = [None]

def p_paramindex_index(p):
    """
    paramindex : paramindex LBracket Integer RBracket
    """
    p[1].append(p[3])
    p[0] = p[1]

def p_paramindex_single_int(p):
    """
    paramindex : LBracket Integer RBracket
    """
    p[0] = [p[2]]

def p_block(p):
    """
    block : block block_item
    """
    if p[2] is not NULL:
        p[1].children.append(p[2])
    p[0] = p[1]


def p_block_empty(p):
    """
    block : empty
    """
    p[0] = Block()


def p_block_item(p):
    """
    block_item : statement
        | declaration Semi
    """
    p[0] = p[1]


def p_statement(p):
    """
    statement : statement_matched
        | statement_unmatched
    """
    p[0] = p[1]


def p_if_else(p):
    """
    statement_matched : If LParen expression RParen statement_matched Else statement_matched
    statement_unmatched : If LParen expression RParen statement_matched Else statement_unmatched
    """
    p[0] = If(p[3], p[5], p[7])


def p_if(p):
    """
    statement_unmatched : If LParen expression RParen statement
    """
    p[0] = If(p[3], p[5])


def p_while(p):
    """
    statement_matched : While LParen expression RParen statement_matched
    statement_unmatched : While LParen expression RParen statement_unmatched
    """
    p[0] = While(p[3], p[5])

def p_for(p):
    """
    statement_matched : For LParen opt_expression Semi opt_expression Semi opt_expression RParen statement_matched
                        | For LParen declaration Semi opt_expression Semi opt_expression RParen statement_matched
    statement_unmatched : For LParen opt_expression Semi opt_expression Semi opt_expression RParen statement_unmatched
                        | For LParen declaration Semi opt_expression Semi opt_expression RParen statement_unmatched
    """
    p[0] = For(p[3], p[5], p[7], p[9])

def p_dowhile(p):
    """
    statement_matched : Do statement_matched While LParen expression RParen Semi
    """
    p[0] = DoWhile(p[2], p[5])

def p_return(p):
    """
    statement_matched : Return expression Semi
    """
    p[0] = Return(p[2])


def p_expression_statement(p):
    """
    statement_matched : opt_expression Semi
    """
    p[0] = p[1]


def p_block_statement(p):
    """
    statement_matched : LBrace block RBrace
    """
    p[0] = p[2]


def p_break(p):
    """
    statement_matched : Break Semi
    """
    p[0] = Break()


def p_continue(p):
    """
    statement_matched : Continue Semi
    """
    p[0] = Continue()

def p_opt_expression(p):
    """
    opt_expression : expression
    """
    p[0] = p[1]


def p_opt_expression_empty(p):
    """
    opt_expression : empty
    """
    p[0] = NULL


def p_declaration(p):
    """
    declaration : type Identifier
    """
    p[0] = Declaration(p[1], p[2])

def p_declaraction_array(p):
    """
    declaration : type Identifier arrayindex
    """
    p[0] = Declaration(p[1], IndexExpr(p[2], p[3], True))

def p_declaration_array_init(p):
    """
    declaration : type Identifier arrayindex arrayInit
    """
    p[0] = Declaration(p[1], IndexExpr(p[2], p[3], True), p[4])

def p_arrayInit_list(p):
    """
    arrayInit : Assign LBrace integerList RBrace
    """
    p[0] = p[3]

def p_integerList_integer(p):
    """
    integerList : integerList Comma Integer
    """
    p[1].append(p[3])
    p[0] = p[1]

def p_integerList_single(p):
    """
    integerList : Integer
    """
    p[0] = [p[1]]

def p_integerList_empty(p):
    """
    integerList : empty
    """
    p[0] = []

def p_declaration_init(p):
    """
    declaration : type Identifier Assign expression
    """
    p[0] = Declaration(p[1], p[2], p[4])

def p_arrayindex(p):
    """
    arrayindex : arrayindex LBracket Integer RBracket
    """
    p[1].append(p[3])
    p[0] = p[1]

def p_arrayindex_single(p):
    """
    arrayindex : LBracket Integer RBracket
    """
    p[0] = [p[2]]

def p_expression_precedence(p):
    """
    expression : assignment
    assignment : conditional
    conditional : logical_or
    logical_or : logical_and
    logical_and : bit_or
    bit_or : xor
    xor : bit_and
    bit_and : equality
    equality : relational
    relational : additive
    additive : multiplicative
    multiplicative : unary
    unary : postfix
    postfix : primary
    """
    p[0] = p[1]


def p_unary_expression(p):
    """
    unary : Minus unary
        | BitNot unary
        | Not unary
    """
    unary(p)

def p_postfix_expression(p):
    """
    postfix : Identifier LParen expression_list RParen
    """
    p[0] = Call(p[1], p[3])

def p_postfix_index(p):
    """
    postfix : postfix LBracket expression RBracket
    """
    if isinstance(p[1], IndexExpr):
        p[1].index.append(p[3])
        p[0] = p[1]
    elif isinstance(p[1], Identifier):
        p[0] = IndexExpr(p[1], [p[3]], False)
    else:
        raise DecafBadAssignTypeError()

def p_expression_list(p):
    """
    expression_list : expression_list Comma expression
    """
    p[1].append(p[3])
    p[0] = p[1]

def p_expression_list_empty(p):
    """
    expression_list : empty
    """
    p[0] = []

def p_expression_list_single(p):
    """
    expression_list : expression
    """
    p[0] = [p[1]]

def p_binary_expression(p):
    """
    assignment : unary Assign expression
    logical_or : logical_or Or logical_and
    logical_and : logical_and And bit_or
    bit_or : bit_or BitOr xor
    xor : xor Xor bit_and
    bit_and : bit_and BitAnd equality
    equality : equality NotEqual relational
        | equality Equal relational
    relational : relational Less additive
        | relational Greater additive
        | relational LessEqual additive
        | relational GreaterEqual additive
    additive : additive Plus multiplicative
        | additive Minus multiplicative
    multiplicative : multiplicative Mul unary
        | multiplicative Div unary
        | multiplicative Mod unary
    """
    binary(p)


def p_conditional_expression(p):
    """
    conditional : logical_or Question expression Colon conditional
    """
    p[0] = ConditionExpression(p[1], p[3], p[5])


def p_int_literal_expression(p):
    """
    primary : Integer
    """
    p[0] = p[1]


def p_identifier_expression(p):
    """
    primary : Identifier
    """
    p[0] = p[1]


def p_brace_expression(p):
    """
    primary : LParen expression RParen
    """
    p[0] = p[2]


def p_error(t):
    """
    A naive (and possibly erroneous) implementation of error recovering.
    """
    if not t:
        error_stack.append(DecafSyntaxError(t, "EOF"))
        return

    inp = t.lexer.lexdata
    error_stack.append(DecafSyntaxError(t, f"\n{inp.splitlines()[t.lineno - 1]}"))

    parser.errok()
    return parser.token()


parser = yacc.yacc(start="program")
parser.error_stack = error_stack  # type: ignore
