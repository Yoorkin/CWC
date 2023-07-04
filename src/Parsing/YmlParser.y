{
module Parsing.YmlParser where
import Parsing.AST
import Parsing.Lexer
import Parsing.ParseUtils
}

%name parse Start
%tokentype { Token }
%error { parseError }

%token
    INT {TokenInt $$}
    FLOAT {TokenFloat $$}
    BOOL {TokenBool $$}
    STRING {TokenString $$}
    CHAR {TokenChar $$}
    IDENT {TokenIdent $$}
    EOF {TokenEof}
    'let' {TokenKeyword "let"}
    'in' {TokenKeyword "in"}
    'rec' {TokenKeyword "rec"}
    'and' {TokenKeyword "and"}
    'case' {TokenKeyword "case"}
    'of' {TokenKeyword "of"}
    'type' {TokenKeyword "type"}
    'try' {TokenKeyword "try"}
    'with' {TokenKeyword "with"}
    'raise' {TokenKeyword "raise"}
    'if' {TokenKeyword "if"}
    'then' {TokenKeyword "then"}
    'else' {TokenKeyword "else"}
    'data' {TokenKeyword "data"}
    '->' {TokenOp "->"}
    ARROW {TokenOp "->"}
    '>=' {TokenOp ">="}
    '<=' {TokenOp "<="}
    '=' {TokenOp "="}
    '<>' {TokenOp "<>"}
    ',' {TokenOp ","} 
    '[' {TokenOp "["} 
    ']' {TokenOp "]"}
    '{' {TokenOp "{"}
    '}' {TokenOp "}"}
    '+' {TokenOp "+"}
    '-' {TokenOp "-"}
    '*' {TokenOp "*"}
    '/' {TokenOp "/"}
    '(' {TokenOp "("}
    ')' {TokenOp ")"} 
    '<' {TokenOp "<"}
    '>' {TokenOp ">"}
    ':' {TokenOp ":"}
    '|' {TokenOp "|"}
    '_' {TokenKeyword "_"}

%nonassoc 'of' 'in'      
%right ARROW
%left '*' '/'
%left '+' '-'
%left '>' '<' '>=' '<=' '=' '<>'
%left '|'

%%

option(x) : x  { Just $1 }
          |    { Nothing }

many(x) :            { [] }
        | many(x) x  { $2 : $1 }

many1(x) : x           { [$1] }
         | many1(x) x  { $2 : $1 }

sepBy1(x,delim) : x                       { [ $1 ] }
                | sepBy1(x,delim) delim x { $3 : $1 }

sepBy(x,delim) : sepBy1(x,delim) { $1 }
               |                 { [] }

endWith(x,end) : x end { $1 }


Start : many(Toplevel) EOF { $1 }

Toplevel : 'data' IDENT many(IDENT) '=' sepBy1(TypeConstr,'|')  { ToplevelType $2 $3 $5 } 
         | 'let' Pattern '=' Expr { ToplevelLet $2 $4 }
         | 'let' IDENT Pattern '=' Expr { ToplevelLet (PatVar $2) (Abs $3 $5) } 
         | 'let' 'rec' sepBy1(FuncBinding, 'and') { let (bs,fs) = unzip $3 
                                                    in ToplevelLetrec bs fs } 


TypeConstr : IDENT 'of' TypeDesc { ($1, $3) }


TypeDesc : TypeDesc IDENT                    { TypeApply $1 (TypeVar $2) }
         | IDENT                             { TypeVar $1 }
         | '(' sepBy(TypeDesc, ',') ')'      { case $2 of
                                                   [] -> TypeVar "()"
                                                   [x] -> x
                                                   xs -> TypeTuple $2 }
         | '{' sepBy(TypeDescField, ',') '}' { TypeRecord $2 }
         | TypeDesc ARROW TypeDesc            { TypeArrow $1 $3 }

TypeDescField : IDENT ':' TypeDesc { ($1, $3) }


Pattern : IDENT { PatVar $1 }
        | '(' sepBy(Pattern, ',') ')'       { PatTuple $2 }
        | '{' sepBy(PatternField, ',') '}'  { PatRecord $2 }

PatternField : IDENT '=' Pattern { PatField $1 $3 }
             | '_'               { PatRest }

Expr : Expr Op Expr                              { Prim (selectPrimOp $2) [$1, $3] }
     | Expr Atom                                 { Apply $1 $2 }
     | Atom                                      { $1 }

Op : '+' { $1 } 
   | '-' { $1 }
   | '*' { $1 }
   | '/' { $1 }      
   | '>' { $1 }   
   | '<' { $1 }   
   | '>=' { $1 }   
   | '<=' { $1 }             
   | '<>' { $1 }
   | '=' {$1}

Binding : Pattern '=' Expr 'in' Expr          { Let $1 $3 $5 }
        | FuncBinding 'in' Expr               { let (b,f) = $1 in Let (PatVar b) f $3 }
        | 'rec' sepBy1(FuncBinding, 'and') 
          'in' Expr                           { let (bs,fs) = unzip $2 in Letrec bs fs $4 }

Atom : IDENT                                     { Var $1 }
     | 'let' Binding                             { $2 }
     | 'if' Expr 'then' Expr 'else' Expr         { If $2 $4 $6 }
     | 'case' Expr 'of' sepBy1(MatchingCase,'|') { Match $2 $4 }
     | '(' sepBy(Expr,',') ')'                   { case $2 of 
                                                      []  -> Unit
                                                      [x] -> x
                                                      xs -> Tuple xs }
     | '{' sepBy(InitField,',') '}'              { Record $2 }
     | Atom ':' TypeDesc                         { Constraint $1 $3 }
     | INT                                       { Integer $1 }
     | FLOAT                                     { Float_ $1 }
     | BOOL                                      { Boolean $1 }
     | STRING                                    { String_ $1 }
     | CHAR                                      { Char_ $1 }
     

MatchingCase : Pattern '->' Expr { ($1, $3) }

InitField : IDENT '=' Expr { ($1, $3) }  

FuncBinding : IDENT Pattern '=' Expr { ($1, Abs $2 $4) :: (String, Mexp) }

