%{
unit h2pparse;

{$GOTO ON}

interface

uses
  scan, h2pconst, h2plexlib, h2pyacclib, scanbase, h2pbase, h2ptypes,h2pout;

procedure EnableDebug;
function yyparse : integer;

Implementation

procedure EnableDebug;

begin
  yydebug:=true;
end;

%}

%token _WHILE _FOR _DO _GOTO _CONTINUE _BREAK
%token TYPEDEF DEFINE
%token COLON SEMICOLON COMMA
%token LKLAMMER RKLAMMER LECKKLAMMER RECKKLAMMER
%token LGKLAMMER RGKLAMMER
%token STRUCT UNION ENUM
%token ID NUMBER CSTRING
%token SHORT UNSIGNED LONG INT FLOAT _CHAR
%token VOID _CONST
%token _FAR _HUGE _NEAR
%token NEW_LINE SPACE_DEFINE
%token EXTERN STDCALL CDECL CALLBACK PASCAL WINAPI APIENTRY WINGDIAPI SYS_TRAP
%token _PACKED
%token ELLIPSIS
%right _ASSIGN
%right R_AND
%right QUESTIONMARK COLON
%left _LOR
%left _LAND
%left _OR
%left _XOR
%left _AND
%left EQUAL UNEQUAL
%left GT LT GTE LTE
%left _SHR _SHL
%left _PLUS MINUS
%left STAR _SLASH _MOD
%right _NOT _LNOT
%right LKLAMMER
%right PSTAR
%right P_AND
%right LECKKLAMMER
%left POINT DEREF
%left COMMA
%left STICK
%token SIGNED
%token INT8 INT16 INT32 INT64
%token _DOUBLE
%token _RETURN
%token _STATIC
%token _LOR _LAND _XOR _MOD
%%

file : declaration_list
     |
     ;

maybe_space :
     SPACE_DEFINE
     {
       (* SPACE_DEFINE *)
       $$:=nil;
     } |
     {
       (* empty space  *)
       $$:=nil;
     }
     ;

error_info : {
               (* error_info *)
               EmitErrorStart(yyline);
               $$:=nil;
             };

declaration_list : declaration_list  declaration
     {
       (* declaration_list  declaration *)
       EmitAndOutput('declaration reduced at line ',line_no);
     }
     | declaration_list define_dec
     {
       (* declaration_list define_dec *)
       EmitAndOutput('define declaration reduced at line ',line_no);
     }
     | declaration
     {
       (* declaration *)
       EmitAndOutput('define declaration reduced at line ',line_no);
     }
     | define_dec
     {
       (* define_dec *)
       EmitAndOutput('define declaration reduced at line ',line_no);
     }
     ;

dec_specifier :
     EXTERN
      { (* EXTERN *)
        $$:=NewID('extern');
      }
     | _STATIC
      { (* STATIC *)
        $$:=NewID('static');
      }
     |
       { (* not extern  *)
         $$:=NewID('intern');
       }
     ;

dec_modifier :
     STDCALL
       {
         (* STDCALL *)
          $$:=NewID('no_pop');
       }
     | CDECL
       {
        (* CDECL *)
        $$:=NewID('cdecl');
        }
     | CALLBACK
       {
        (* CALLBACK *)
        $$:=NewID('no_pop');
       }
     | PASCAL
       {
         (* PASCAL *)
         $$:=NewID('no_pop');
       }
     | WINAPI
       {
       (* WINAPI *)
       $$:=NewID('no_pop');
       }
     | APIENTRY
       {
         (* APIENTRY  *)
         $$:=NewID('no_pop');
       }
     | WINGDIAPI
       {
         (* WINGDIAPI  *)
         $$:=NewID('no_pop');
       }
     |
       {
         (* No modifier *)
         $$:=nil
       }
     ;

systrap_specifier:
     SYS_TRAP LKLAMMER dname RKLAMMER
       {
         (* SYS_TRAP LKLAMMER dname RKLAMMER *)
         $$:=$3;
       }
     |
       {
        (* Empty systrap *)
        $$:=nil;
       }
     ;

statement :
     expr SEMICOLON
       {
         (* expr SEMICOLON *)
         $$:=$1;
       } |
     _WHILE LKLAMMER expr RKLAMMER statement_list
       {
         (* _WHILE LKLAMMER expr RKLAMMER statement_list  *)
         $$:=NewType2(t_whilenode,$3,$5);
       } |
     _RETURN expr SEMICOLON
       {
         (* _RETURN expr SEMICOLON *)
         $$:=NewUnaryOp('exit',$2);
       } |
     _RETURN SEMICOLON
       {
         (* _RETURN SEMICOLON *)
         $$:=NewID('exit');
       }
     ;


statement_list : statement statement_list
     {
       (* statement statement_list *)
       $$:=NewType1(t_statement_list,$1);
       $$^.next:=$2;
     } |
     statement
     {
       (* statement  *)
       $$:=NewType1(t_statement_list,$1);
     } |
     SEMICOLON
     {
       (* SEMICOLON  *)
       $$:=NewType1(t_statement_list,nil);
     } |
     {
       (* empty statement  *)
       $$:=NewType1(t_statement_list,nil);
     }
     ;

statement_block :
     LGKLAMMER statement_list RGKLAMMER
     {
       (* LGKLAMMER statement_list RGKLAMMER  *)
       $$:=$2;
     }
     ;

declaration :
     dec_specifier type_specifier dec_modifier declarator_list statement_block
     {
      (* dec_specifier type_specifier dec_modifier declarator_list statement_block *)
       HandleDeclarationStatement($1,$2,$3,$4,$5);
     }
     | dec_specifier type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON
     {
       (* dec_specifier type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON *)
       HandleDeclarationSysTrap($1,$2,$3,$4,$5);
     } |
     special_type_specifier SEMICOLON
     {
       (* special_type_specifier SEMICOLON *)
       HandleSpecialType($1);
     } |
     special_type_specifier dec_modifier declarator_list statement_block
     {
       (* special_type_specifier dec_modifier declarator_list statement_block *)
       HandleDeclarationStatement(NewID('intern'),$1,$2,$3,$4);
     } |
     special_type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON
     {
       (* special_type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON *)
       HandleDeclarationSysTrap(NewID('intern'),$1,$2,$3,$4);
     } |
     anonymous_type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON
     {
       (* anonymous_type_specifier dec_modifier declarator_list systrap_specifier SEMICOLON *)
       HandleDeclarationSysTrap(NewID('intern'),$1,$2,$3,$4);
     } |
     TYPEDEF STRUCT dname dname SEMICOLON
     {
       (* TYPEDEF STRUCT dname dname SEMICOLON *)
       HandleStructDef($3,$4);
     } |
     TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON
     {
       (* TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON *)
       HandleTypeDef($2,$4,$5,$9);
     } |
     TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON
     {
       (* TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON *)
       HandleTypeDefList($2,$3,$4);
     } |
     TYPEDEF dname SEMICOLON
     {
       (* TYPEDEF dname SEMICOLON *)
       HandleSimpleTypeDef($2);
     }
     | error  error_info SEMICOLON
      {
        (* error  error_info SEMICOLON *)
        HandleErrorDecl($1,$2);
      }
     ;

define_dec :
     DEFINE dname LKLAMMER enum_list RKLAMMER para_def_expr NEW_LINE
     {
       (* DEFINE dname LKLAMMER enum_list RKLAMMER para_def_expr NEW_LINE *)
       HandleDefineMacro($2,$4,$6);
     }|
     DEFINE dname SPACE_DEFINE NEW_LINE
     {
      (* DEFINE dname SPACE_DEFINE NEW_LINE *)
       HandleDefine($2);
     }|
     DEFINE dname NEW_LINE
     {
       (* DEFINE dname NEW_LINE *)
       HandleDefine($2);
     } |
     DEFINE dname SPACE_DEFINE def_expr NEW_LINE
     {
       (* DEFINE dname SPACE_DEFINE def_expr NEW_LINE *)
       HandleDefineConst($2,$4);
     }
     | error error_info NEW_LINE
     {
       (* error error_info NEW_LINE *)
       HandleErrorDecl($1,$2);
     }
     ;

closed_list :
     LGKLAMMER member_list RGKLAMMER
       {
        (* LGKLAMMER member_list RGKLAMMER *)
         $$:=$2;
       }
     | error  error_info RGKLAMMER
       {
         (* error  error_info RGKLAMMER *)
         emitwriteln(' in member_list *)');
         yyerrok;
         $$:=nil;
       }
    ;

closed_enum_list :
      LGKLAMMER enum_list RGKLAMMER
        {
          (* LGKLAMMER enum_list RGKLAMMER *)
          $$:=$2;
        }
      |  error  error_info  RGKLAMMER
        {
          (* error  error_info RGKLAMMER *)
          emitwriteln(' in enum_list *)');
          yyerrok;
          $$:=nil;
         }
      ;

anonymous_type_specifier :
     STRUCT closed_list _PACKED
     {
       (* STRUCT closed_list _PACKED *)
       emitpacked(1);
       $$:=NewType1(t_structdef,$2);
     } |
     STRUCT closed_list
     {
       (* STRUCT closed_list *)
       emitpacked(4);
       $$:=NewType1(t_structdef,$2);
     } |
     UNION closed_list _PACKED
     {
       (* UNION closed_list _PACKED *)
       emitpacked(1);
       $$:=NewType1(t_uniondef,$2);
     } |
     UNION closed_list
     {
       (* UNION closed_list *)
       $$:=NewType1(t_uniondef,$2);
     } |
     ENUM closed_enum_list
     {
       (* ENUM closed_enum_list *)
       $$:=NewType1(t_enumdef,$2);
     }
     ;

special_type_specifier :
     STRUCT dname closed_list _PACKED
     {
       (* STRUCT dname closed_list _PACKED *)
       emitpacked(1);
       $$:=NewType2(t_structdef,$3,$2);
     } |
     STRUCT dname closed_list
     {
       (* STRUCT dname closed_list *)
       emitpacked(4);
       $$:=NewType2(t_structdef,$3,$2);
     } |
     UNION dname closed_list _PACKED
     {
       (* UNION dname closed_list _PACKED *)
       emitpacked(1);
       $$:=NewType2(t_uniondef,$3,$2);
     } |
     UNION dname closed_list
     {
       (* UNION dname closed_list *)
       $$:=NewType2(t_uniondef,$3,$2);
     } |
     UNION dname
     {
       (* UNION dname  *)
       $$:=$2;
       $$^.structtag:=true;
     } |
     STRUCT dname
     {
       (* STRUCT dname *)
       $$:=$2;
       $$^.structtag:=true;
     } |
     ENUM dname closed_enum_list
     {
       (* ENUM dname closed_enum_list *)
       $$:=NewType2(t_enumdef,$3,$2);
     } |
     ENUM dname
     {
       (* ENUM dname *)
       $$:=$2;
     };

type_specifier :
      _CONST type_specifier
       {
        (* _CONST type_specifier *)
        EmitIgnoreConst;
        $$:=$2;
        } |
     UNION closed_list  _PACKED
     {
       (* UNION closed_list  _PACKED *)
       EmitPacked(1);
       $$:=NewType1(t_uniondef,$2);
     } |
     UNION closed_list
     {
       (* UNION closed_list *)
       $$:=NewType1(t_uniondef,$2);
     } |
     STRUCT closed_list _PACKED
     {
       (* STRUCT closed_list _PACKED *)
       emitpacked(1);
       $$:=NewType1(t_structdef,$2);
     } |
     STRUCT closed_list
     {
       (* STRUCT closed_list  *)
       emitpacked(4);
       $$:=NewType1(t_structdef,$2);
     } |
     ENUM closed_enum_list
     {
       (* ENUM closed_enum_list*)
       $$:=NewType1(t_enumdef,$2);
     } |
     special_type_specifier
     {
       (* special_type_specifier *)
       $$:=$1;
     } |
     simple_type_name { $$:=$1; }
     ;

member_list : member_declaration member_list
     {
       (*  member_declaration member_list *)
       $$:=NewType1(t_memberdeclist,$1);
       $$^.next:=$2;
     } |
     member_declaration
     {
       (* member_declaration *)
       $$:=NewType1(t_memberdeclist,$1);
     }
     ;

member_declaration :
     type_specifier declarator_list SEMICOLON
     {
       (* type_specifier declarator_list SEMICOLON *)
       $$:=NewType2(t_memberdec,$1,$2);
     }
     ;

dname : ID {
           (* dname *)
           $$:=NewID(act_token);
           }
     ;
special_type_name :
     SIGNED special_type_name
     {
       (* SIGNED special_type_name *)
       $$:=HandleSpecialSignedType($2);
     } |
     UNSIGNED special_type_name
     {
       (* UNSIGNED special_type_name *)
       $$:=HandleSpecialUnsignedType($2);
     } |
     INT
     {
       (* INT *)
       $$:=NewCType(cint_STR,INT_STR);
     } |
     LONG
     {
       (* LONG *)
       $$:=NewCType(clong_STR,INT_STR);
     } |
     LONG INT
     {
       (* LONG INT *)
       $$:=NewCType(clong_STR,INT_STR);
     } |
     LONG LONG
     {
       (* LONG LONG *)
       $$:=NewCType(clonglong_STR,INT64_STR);
     } |
     LONG LONG INT
     {
       (* LONG LONG INT *)
       $$:=NewCType(clonglong_STR,INT64_STR);
     } |
     SHORT
     {
       (* SHORT  *)
       $$:=NewCType(cshort_STR,SMALL_STR);
     } |
     SHORT INT
     {
       (* SHORT INT *)
       $$:=NewCType(cshort_STR,SMALL_STR);
     } |
     INT8
     {
       (* INT8 *)
       $$:=NewCType(cint8_STR,SHORT_STR);
     } |
     INT16
     {
       (* INT8 *)
       $$:=NewCType(cint16_STR,SMALL_STR);
     } |
     INT32
     {
       (* INT32 *)
       $$:=NewCType(cint32_STR,INT_STR);
     } |
     INT64
     {
       (* INT64 *)

       $$:=NewCType(cint64_STR,INT64_STR);
     } |
     FLOAT
     {
       (* FLOAT *)
        $$:=NewCType(cfloat_STR,FLOAT_STR);
     } |
     _DOUBLE
     {
       (* DOUBLE *)
        $$:=NewCType(cdouble_STR,DOUBLE_STR);
     } |
     LONG _DOUBLE
     {
       (* LONG DOUBLE *)
        $$:=NewCType(clongdouble_STR,EXTENDED_STR);
     } |
     VOID
     {
       (* VOID *)
       $$:=NewVoid;
     } |
     _CHAR
     {
       (* CHAR *)
       $$:=NewCType(cchar_STR,char_STR);
     } |
     UNSIGNED
     {
       (* UNSIGNED *)
       $$:=NewCType(cunsigned_STR,UINT_STR);
     } |
     SIGNED
     {
       (* SIGNED *)
       $$:=NewCType(csigned_STR,INT_STR);
     }
     ;

simple_type_name :
     special_type_name
     {
      (* special_type_name *)
     $$:=$1;
     }
     |
     dname
     {
       (* dname *)
       $$:=MapCTypeName($1);
     }
     ;

declarator_list :
     declarator_list COMMA declarator
     {
       (* declarator_list COMMA declarator *)
       $$:=HandleDeclarationList($1,$3);
     }|
     error error_info COMMA declarator_list
     {
       (* error error_info COMMA declarator_list *)
       EmitWriteln(' in declarator_list *)');
       $$:=$4;
       yyerrok;
     }|
     error error_info
     {
       (* error error_info *)
       EmitWriteln(' in declarator_list *)');
       yyerrok;
       $$:=nil;
     }|
     declarator
     {
       (* declarator *)
       $$:=NewType1(t_declist,$1);
     }
     ;

argument_declaration : type_specifier declarator
     {
       (* type_specifier declarator *)
       $$:=NewType2(t_arg,$1,$2);
     } |
     type_specifier STAR declarator
     {
       (* type_specifier STAR declarator *)
       $$:=HandlePointerArgDeclarator($1,$3);
     } |
     type_specifier abstract_declarator
     {
       (* type_specifier abstract_declarator *)
       $$:=NewType2(t_arg,$1,$2);
     }
     ;

argument_declaration_list : argument_declaration
     {
       (* argument_declaration *)
       $$:=NewType2(t_arglist,$1,nil);
     } |
     argument_declaration COMMA argument_declaration_list
     {
       (* argument_declaration COMMA argument_declaration_list *)
       $$:=HandleArgList($1,$3)
     } |
     ELLIPSIS
     {
       (* ELLIPISIS *)
       $$:=NewType2(t_arglist,ellipsisarg,nil);
     } |
     {
       (* empty *)
       $$:=nil;
     }
     ;

size_overrider :
       _FAR
       {
         (* FAR *)
         $$:=NewID('far');
       }
       | _NEAR
       {
         (* NEAR*)
         $$:=NewID('near');
       }
       | _HUGE
       {
         (* HUGE *)
         $$:=NewID('huge');}
       ;

declarator :
      _CONST declarator
      {
        (* _CONST declarator *)
        EmitIgnoreConst;
        $$:=$2;
        } |
     size_overrider STAR declarator
     {
       (* size_overrider STAR declarator *)
       $$:=HandleSizeOverrideDeclarator($1,$3);
     } |
     STAR declarator
     {
       (* %prec PSTAR this was wrong!! *)
       $$:=HandleDeclarator(t_pointerdef,$2);
     } |
     _AND declarator
     {
       (* _AND declarator *)
       $$:=HandleDeclarator(t_addrdef,$2);
     } |
     dname COLON expr
       {
         (* dname COLON expr *)
         $$:=HandleSizedDeclarator($1,$3);
        }|
     dname ASSIGN expr
       {
         (*     dname ASSIGN expr *)
         $$:=HandleDefaultDeclarator($1,$3);
        }|
     dname
       {
         (* dname *)
         $$:=NewType2(t_dec,nil,$1);
        }|
     declarator LKLAMMER argument_declaration_list RKLAMMER
     {
       (* declarator LKLAMMER argument_declaration_list RKLAMMER *)
       $$:=HandleDeclarator2(t_procdef,$1,$3);
     } |
     declarator no_arg
     {
       (*   declarator no_arg *)
       $$:=HandleDeclarator2(t_procdef,$1,Nil);
     } |
     declarator LECKKLAMMER expr RECKKLAMMER
     {
       (* declarator LECKKLAMMER expr RECKKLAMMER *)
       $$:=HandleDeclarator2(t_arraydef,$1,$3);
     } |
     declarator LECKKLAMMER RECKKLAMMER
     {
       (* declarator LECKKLAMMER RECKKLAMMER *)
       $$:=HandleArrayDecl($1);
     } |
     LKLAMMER declarator RKLAMMER
     {
       (* LKLAMMER declarator RKLAMMER *)
       $$:=$2;
     }
     ;

no_arg : LKLAMMER RKLAMMER |
        LKLAMMER VOID RKLAMMER;

abstract_declarator :
      _CONST abstract_declarator
      {
        (* _CONST abstract_declarator *)
        EmitAbstractIgnored;
        $$:=$2;
        } |
     size_overrider STAR abstract_declarator
     {
       (* size_overrider STAR abstract_declarator *)
       $$:=HandleSizedPointerDeclarator($3,$1);
     } |
     STAR abstract_declarator %prec PSTAR
     {
       (* STAR abstract_declarator %prec PSTAR *)
       $$:=HandlePointerAbstractDeclarator($2);
     } |
     _AND abstract_declarator %prec PSTAR
     {
       (* _AND abstract_declarator %prec PSTAR *)
       $$:=HandleDeclarator(t_addrdef,$2);
     } |
     abstract_declarator LKLAMMER argument_declaration_list RKLAMMER
     {
       (* abstract_declarator LKLAMMER argument_declaration_list RKLAMMER *)
       $$:=HandlePointerAbstractListDeclarator($1,$3);
     } |
     abstract_declarator no_arg
     {
       (* abstract_declarator no_arg *)
       $$:=HandleFuncNoArg($1);
     } |
     abstract_declarator LECKKLAMMER expr RECKKLAMMER
     {
       (* abstract_declarator LECKKLAMMER expr RECKKLAMMER *)
       $$:=HandleSizedArrayDecl($1,$3);
     } |
     declarator LECKKLAMMER RECKKLAMMER
     {
       (* declarator LECKKLAMMER RECKKLAMMER *)
       $$:=HandleArrayDecl($1);
     } |
     LKLAMMER abstract_declarator RKLAMMER
     {
       (* LKLAMMER abstract_declarator RKLAMMER *)
       $$:=$2;
     } |
     {
       $$:=NewType2(t_dec,nil,nil);
     }
     ;

expr    : shift_expr
          {
            (* shift_expr *)
            $$:=$1;
          }
          ;

shift_expr :
          expr _ASSIGN expr
            { $$:=NewBinaryOp(':=',$1,$3); }
          | expr EQUAL expr
            { $$:=NewBinaryOp('=',$1,$3);}
          | expr UNEQUAL expr
            { $$:=NewBinaryOp('<>',$1,$3);}
          | expr GT expr
            { $$:=NewBinaryOp('>',$1,$3);}
          | expr GTE expr
            { $$:=NewBinaryOp('>=',$1,$3);}
          | expr LT expr
            { $$:=NewBinaryOp('<',$1,$3);}
          | expr LTE expr
            { $$:=NewBinaryOp('<=',$1,$3);}
          | expr _PLUS expr
            { $$:=NewBinaryOp('+',$1,$3);}
          | expr MINUS expr
            { $$:=NewBinaryOp('-',$1,$3);}
          | expr STAR expr
            { $$:=NewBinaryOp('*',$1,$3);}
          | expr _SLASH expr
            { $$:=HandleDivision($1,$3);}
          | expr _OR expr
            { $$:=NewBinaryOp(' or ',$1,$3);}
          | expr _AND expr
            { $$:=NewBinaryOp(' and ',$1,$3);}
          | expr _NOT expr
            { $$:=NewBinaryOp(' not ',$1,$3);}
          | expr _SHL expr
            { $$:=NewBinaryOp(' shl ',$1,$3);}
          | expr _SHR expr
            { $$:=NewBinaryOp(' shr ',$1,$3);}
          | expr _LOR expr
            { $$:=HandleLogicalOp(' or ',$1,$3);}
          | expr _LAND expr
            { $$:=HandleLogicalOp(' and ',$1,$3);}
          | expr _XOR expr
            { $$:=NewBinaryOp(' xor ',$1,$3);}
          | expr _MOD expr
            { $$:=NewBinaryOp(' mod ',$1,$3);}
          | expr QUESTIONMARK colon_expr
          {
            $$:=HandleTernary($1,$3);
          } |
          unary_expr {$$:=$1;}
          ;

colon_expr : expr COLON expr
       {
         (* if A then B else C *)
         $$:=NewType3(t_ifexpr,nil,$1,$3);
       }
       ;

maybe_empty_unary_expr :
                  unary_expr
                  { $$:=$1; }
                  |
                  { $$:=nil;}
                  ;

string_list :
     CSTRING
     {
     (* remove L prefix for widestrings *)
     $$:=CheckWideString(act_token);
     } |
     string_list CSTRING
     {
     $$:=ConcatStrings($1,CheckWideString(act_token));
     }
     ;

unary_expr:
     dname
     {
     $$:=$1;
     } |
     special_type_name
     {
     $$:=$1;
     } |
     string_list
     {
     $$:=$1;
     } |
     NUMBER
     {
     $$:=NewID(act_token);
     } |
     unary_expr POINT expr
     {
     $$:=NewBinaryOp('.',$1,$3);
     } |
     unary_expr DEREF expr
     {
     $$:=NewBinaryOp('^.',$1,$3);
     } |
     MINUS unary_expr
     {
     $$:=NewUnaryOp('-',$2);
     }|
     STAR unary_expr
     {
     (* dereference *)
     $$:=NewUnaryOp('^',$2);
     }|
     _PLUS unary_expr
     {
     $$:=NewUnaryOp('+',$2);
     }|
     _AND unary_expr %prec R_AND
     {
     $$:=NewUnaryOp('@',$2);
     }|
     _NOT unary_expr
     {
     $$:=NewUnaryOp(' not ',$2);
     } |
     _LNOT unary_expr
     {
     $$:=HandleLogicalNot($2);
     } |
     LKLAMMER dname RKLAMMER maybe_empty_unary_expr
     {
     (* (x) * y is a product rather than the cast of *y *)
     if assigned($4) and IsCTypeName($2) then
       $$:=NewType2(t_typespec,MapCTypeName($2),$4)
     else if assigned($4) and ($4^.typ=t_preop) and ($4^.str='^') then
       begin
       $$:=NewBinaryOp('*',$2,$4^.p1);
       $4^.p1:=nil;
       dispose($4,done);
       end
     else if assigned($4) then
       $$:=NewType2(t_typespec,$2,$4)
     else
       $$:=$2;
     } |
     LKLAMMER type_specifier RKLAMMER unary_expr
     {
     $$:=NewType2(t_typespec,$2,$4);
     } |
     LKLAMMER type_specifier pointer_stars RKLAMMER unary_expr
     {
     $$:=HandlePointerCast($2,$3,$5);
     } |
     LKLAMMER dname pointer_stars RKLAMMER unary_expr
     {
     (* pointer cast to a named type *)
     $$:=HandlePointerCast(MapCTypeName($2),$3,$5);
     } |
     LKLAMMER dname STAR shift_expr RKLAMMER
     {
     (* product of a name, between parentheses *)
     $$:=HandleNamedProduct($2,$4);
     $$^.grouped:=true;
     } |
     LKLAMMER type_specifier size_overrider STAR RKLAMMER unary_expr
     {
     $$:=HandlePointerType($2,$6,$3);
     } |
     dname LKLAMMER exprlist RKLAMMER
     {
     $$:=HandleFuncExpr($1,$3);
     } |
     LKLAMMER shift_expr RKLAMMER
     {
     $$:=$2;
     if assigned($$) then
       $$^.grouped:=true;
     } |
     LKLAMMER STAR unary_expr RKLAMMER maybe_space LKLAMMER exprlist RKLAMMER
     {
       $$:=NewType2(t_callop,$3,$7);
     } |
     LKLAMMER STAR unary_expr RKLAMMER
     {
       (* dereference between parentheses *)
       $$:=NewUnaryOp('^',$3);
       $$^.grouped:=true;
     } |
     dname LECKKLAMMER exprlist RECKKLAMMER
     {
       $$:=NewType2(t_arrayop,$1,$3);
     }
     ;

pointer_stars :
     STAR
     {
       (* STAR *)
       $$:=NewID('*');
     } |
     STAR pointer_stars
     {
       (* STAR pointer_stars *)
       $2^.setstr($2^.str+'*');
       $$:=$2;
     }
     ;

enum_list :
     enum_element COMMA enum_list
      {
       (*enum_element COMMA enum_list *)
       $$:=$1;
       $$^.next:=$3;
      } |
      enum_element
      {
       (* enum element *)
       $$:=$1;
      } |
      {
        (* empty enum list *)
       $$:=nil;
      };

enum_element :
     dname _ASSIGN expr
     {
      (* enum_element: dname _ASSIGN expr *)
      $$:=NewType2(t_enumlist,$1,$3);
     } |
     dname
     {
       (* enum_element: dname *)
       $$:=NewType2(t_enumlist,$1,nil);
     };


def_expr :
     expr
     {
       (* expr *)
       $$:=HandleUnaryDefExpr($1);
     }
     ;

para_def_expr :
     SPACE_DEFINE def_expr
     {
       (* SPACE_DEFINE def_expr *)
       $$:=$2;
     } |
     maybe_space LKLAMMER def_expr RKLAMMER
     {
       (* maybe_space LKLAMMER def_expr RKLAMMER *)
       $$:=$3
     }
     ;

exprlist : exprelem COMMA exprlist
     {
       (*exprlist COMMA expr*)
       $$:=$1;
       $1^.next:=$3;
     } |
     exprelem
     {
       (* exprelem *)
       $$:=$1;
     } |
     {
       (* empty expression list *)
       $$:=nil;
     };

exprelem :
           expr
           {
            (*expr *)
             $$:=NewType1(t_exprlist,$1);
           };

%%

function yylex : Integer;
begin
  yylex:=scan.yylex;
  line_no:=yylineno;
end;

end.
