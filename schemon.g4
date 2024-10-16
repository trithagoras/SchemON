grammar schemon;

TSTR : 'str';
TINT : 'int';
TFLOAT : 'float';
TBOOL : 'bool';
TCHAR : 'char';

program : pair+ EOF;

type
    : TSTR
    | TINT
    | TFLOAT
    | TBOOL
    | TCHAR
    | IDENTIFIER
    | tlist
    | tobj
    ;

tlist : '[' type ']';
tobj : '{' pairList? '}';

pairList : pair (',' pair)* ;
pair : IDENTIFIER ':' type;

IDENTIFIER : [_a-zA-Z][_a-zA-Z0-9]* ;
