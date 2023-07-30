{
module Parsing.YmlParser(parse) where
import Parsing.AST as AST
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
    'fun' {TokenKeyword "fun"}
    "forall" {TokenKeyword "forall"}
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
    '.' {TokenOp "."}
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


Start : many(DataType) Expr EOF { Program $1 $2 }

DataType : 'data' IDENT many(IDENT) '=' sepBy1(DataConstructor, '|') { DataType $2 $3 $5 }

DataConstructor : IDENT 'of' Annotation { Constructor $1 $3 }

Annotation : IDENT  { AnnoVar $1 }
           | Annotation '->' Annotation { AnnoArrow $1 $3 }
           | IDENT many1(Annotation)  { AnnoConstr $1 $2 }
           | '(' sepBy1(Annotation,',') ')' { case $2 of
                                                   [] -> AnnoVar "unit"
                                                   [x] -> x
                                                   xs -> AnnoTuple xs }

Pattern : IDENT { PatVar $1 }
        | Pattern ':' Annotation  { PatConstraint $1 $3 }
        | '(' sepBy(Pattern, ',') ')'       { PatTuple $2 }

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
        -- | FuncBinding 'in' Expr               { let (b,f) = $1 in Let (PatVar b) f $3 }
        -- | 'rec' sepBy1(FuncBinding, 'and') 
        --   'in' Expr                           { let (bs,fs) = unzip $2 in Letrec bs fs $4 }

Expr : 'fun' Pattern '->' Expr                   { Abs $2 $4 }
     | 'let' Binding                             { $2 }
     | 'if' Expr 'then' Expr 'else' Expr         { If $2 $4 $6 }
     | 'case' Expr 'of' sepBy1(MatchingCase,'|') { let (pats,cases) = unzip $4 in Match $2 pats cases }
     | Term                                      { $1 }

Term : Term Op Term                              { Prim (selectPrimOp $2) [$1, $3] }
     | Term Atom                                 { Apply $1 $2 }
     | Atom                                      { $1 }


Atom : IDENT                                     { Var $1 }
     | '(' sepBy(Expr,',') ')'                   { case $2 of 
                                                      []  -> AST.Constant Unit
                                                      [x] -> x
                                                      xs -> Tuple xs }
     | Atom ':' Annotation                       { Constraint $1 $3 }
     | INT                                       { AST.Constant $ AST.Integer $1 }
     | FLOAT                                     { AST.Constant $ AST.Float $1 }
     | BOOL                                      { AST.Constant $ AST.Boolean $1 }
     | STRING                                    { AST.Constant $ AST.String $1 }
     | CHAR                                      { AST.Constant $ AST.Char $1 }
     

MatchingCase : Pattern '->' Expr { ($1, $3) }

InitField : IDENT '=' Expr { ($1, $3) }  

FuncBinding : IDENT Pattern '=' Expr { ($1, Abs $2 $4) :: (String, Mexp) }

