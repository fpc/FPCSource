{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    Tokens used by the compiler

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ****************************************************************************
}
unit tokens;
interface

uses
  globtype;

const
  tokenidlen=14;

type
  ttoken=(NOTOKEN,
    { operators, which can also be overloaded }
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL,
    GT,
    LT,
    GTE,
    LTE,
    SYMDIF,
    STARSTAR,
    OP_IS,
    OP_AS,
    OP_IN,
    ASSIGNMENT,
    { special chars }
    CARET,
    UNEQUAL,
    LECKKLAMMER,
    RECKKLAMMER,
    POINT,
    COMMA,
    LKLAMMER,
    RKLAMMER,
    COLON,
    SEMICOLON,
    KLAMMERAFFE,
    POINTPOINT,
    DOUBLEADDR,
    _EOF,
    ID,
    NOID,
    REALNUMBER,
    INTCONST,
    CSTRING,
    CCHAR,
    { C like operators }
    _PLUSASN,
    _MINUSASN,
    _ANDASN,
    _ORASN,
    _STARASN,
    _SLASHASN,
    _MODASN,
    _DIVASN,
    _NOTASN,
    _XORASN,
    { Normal words }
    _AS,
    _AT,
    _DO,
    _IF,
    _IN,
    _IS,
    _OF,
    _ON,
    _OR,
    _TO,
    _AND,
    _ASM,
    _DIV,
    _END,
    _FAR,
    _FOR,
    _MOD,
    _NEW,
    _NIL,
    _NOT,
    _SET,
    _SHL,
    _SHR,
    _TRY,
    _VAR,
    _XOR,
    _CASE,
    _CVAR,
    _ELSE,
    _EXIT,
    _FAIL,
    _FILE,
    _GOTO,
    _NAME,
    _NEAR,
    _READ,
    _SELF,
    _THEN,
    _TRUE,
    _TYPE,
    _UNIT,
    _USES,
    _WITH,
    _ALIAS,
    _ARRAY,
    _BEGIN,
    _BREAK,
    _CDECL,
    _CLASS,
    _CONST,
    _FALSE,
    _INDEX,
    _LABEL,
    _RAISE,
    _UNTIL,
    _WHILE,
    _WRITE,
    _DOWNTO,
    _EXCEPT,
    _EXPORT,
    _INLINE,
    _OBJECT,
    _PACKED,
    _PASCAL,
    _PUBLIC,
    _RECORD,
    _REPEAT,
    _RESULT,
    _STATIC,
    _STORED,
    _STRING,
    _SYSTEM,
    _ASMNAME,
    _DEFAULT,
    _DISPOSE,
    _DYNAMIC,
    _EXPORTS,
    _FINALLY,
    _FORWARD,
    _IOCHECK,
    _LIBRARY,
    _MESSAGE,
    _PRIVATE,
    _PROGRAM,
    _STDCALL,
    _SYSCALL,
    _VIRTUAL,
    _ABSOLUTE,
    _ABSTRACT,
    _CONTINUE,
    _EXTERNAL,
    _FUNCTION,
    _OPERATOR,
    _OVERRIDE,
    _POPSTACK,
    _PROPERTY,
    _REGISTER,
    _RESIDENT,
    _SAFECALL,
    _ASSEMBLER,
    _INHERITED,
    _INTERFACE,
    _INTERRUPT,
    _NODEFAULT,
    _OTHERWISE,
    _PROCEDURE,
    _PROTECTED,
    _PUBLISHED,
    _THREADVAR,
    _DESTRUCTOR,
    _INTERNPROC,
    _OPENSTRING,
    _CONSTRUCTOR,
    _INTERNCONST,
    _SHORTSTRING,
    _FINALIZATION,
    _IMPLEMENTATION,
    _INITIALIZATION
  );

  tokenrec=record
    str     : string[tokenidlen];
    special : boolean;
    keyword : tmodeswitch;
    encoded : longint;
  end;

const
  tokeninfo:array[ttoken] of tokenrec=(
      (str:''              ;special:true ;keyword:m_none),
    { Operators which can be overloaded }
      (str:'+'             ;special:true ;keyword:m_none),
      (str:'-'             ;special:true ;keyword:m_none),
      (str:'*'             ;special:true ;keyword:m_none),
      (str:'/'             ;special:true ;keyword:m_none),
      (str:'='             ;special:true ;keyword:m_none),
      (str:'>'             ;special:true ;keyword:m_none),
      (str:'<'             ;special:true ;keyword:m_none),
      (str:'>='            ;special:true ;keyword:m_none),
      (str:'<='            ;special:true ;keyword:m_none),
      (str:'><'            ;special:true ;keyword:m_none),
      (str:'**'            ;special:true ;keyword:m_none),
      (str:'is'            ;special:true ;keyword:m_none),
      (str:'as'            ;special:true ;keyword:m_none),
      (str:'in'            ;special:true ;keyword:m_none),
      (str:':='            ;special:true ;keyword:m_none),
    { Special chars }
      (str:'^'             ;special:true ;keyword:m_none),
      (str:'<>'            ;special:true ;keyword:m_none),
      (str:'['             ;special:true ;keyword:m_none),
      (str:']'             ;special:true ;keyword:m_none),
      (str:'.'             ;special:true ;keyword:m_none),
      (str:','             ;special:true ;keyword:m_none),
      (str:'('             ;special:true ;keyword:m_none),
      (str:')'             ;special:true ;keyword:m_none),
      (str:':'             ;special:true ;keyword:m_none),
      (str:';'             ;special:true ;keyword:m_none),
      (str:'@'             ;special:true ;keyword:m_none),
      (str:'..'            ;special:true ;keyword:m_none),
      (str:'@@'            ;special:true ;keyword:m_none),
      (str:'end of file'   ;special:true ;keyword:m_none),
      (str:'identifier'    ;special:true ;keyword:m_none),
      (str:'non identifier';special:true ;keyword:m_none),
      (str:'const real'    ;special:true ;keyword:m_none),
      (str:'ordinal const' ;special:true ;keyword:m_none),
      (str:'const string'  ;special:true ;keyword:m_none),
      (str:'const char'    ;special:true ;keyword:m_none),
    { C like operators }
      (str:'+='            ;special:true ;keyword:m_none),
      (str:'-='            ;special:true ;keyword:m_none),
      (str:'&='            ;special:true ;keyword:m_none),
      (str:'|='            ;special:true ;keyword:m_none),
      (str:'*='            ;special:true ;keyword:m_none),
      (str:'/='            ;special:true ;keyword:m_none),
      (str:''              ;special:true ;keyword:m_none),
      (str:''              ;special:true ;keyword:m_none),
      (str:''              ;special:true ;keyword:m_none),
      (str:''              ;special:true ;keyword:m_none),
    { Normal words }
      (str:'AS'            ;special:false;keyword:m_class),
      (str:'AT'            ;special:false;keyword:m_none),
      (str:'DO'            ;special:false;keyword:m_all),
      (str:'IF'            ;special:false;keyword:m_all),
      (str:'IN'            ;special:false;keyword:m_all),
      (str:'IS'            ;special:false;keyword:m_class),
      (str:'OF'            ;special:false;keyword:m_all),
      (str:'ON'            ;special:false;keyword:m_objpas),
      (str:'OR'            ;special:false;keyword:m_all),
      (str:'TO'            ;special:false;keyword:m_all),
      (str:'AND'           ;special:false;keyword:m_all),
      (str:'ASM'           ;special:false;keyword:m_all),
      (str:'DIV'           ;special:false;keyword:m_all),
      (str:'END'           ;special:false;keyword:m_all),
      (str:'FAR'           ;special:false;keyword:m_none),
      (str:'FOR'           ;special:false;keyword:m_all),
      (str:'MOD'           ;special:false;keyword:m_all),
      (str:'NEW'           ;special:false;keyword:m_all),
      (str:'NIL'           ;special:false;keyword:m_all),
      (str:'NOT'           ;special:false;keyword:m_all),
      (str:'SET'           ;special:false;keyword:m_all),
      (str:'SHL'           ;special:false;keyword:m_all),
      (str:'SHR'           ;special:false;keyword:m_all),
      (str:'TRY'           ;special:false;keyword:m_objpas),
      (str:'VAR'           ;special:false;keyword:m_all),
      (str:'XOR'           ;special:false;keyword:m_all),
      (str:'CASE'          ;special:false;keyword:m_all),
      (str:'CVAR'          ;special:false;keyword:m_none),
      (str:'ELSE'          ;special:false;keyword:m_all),
      (str:'EXIT'          ;special:false;keyword:m_all),
      (str:'FAIL'          ;special:false;keyword:m_none), { only set within constructors PM }
      (str:'FILE'          ;special:false;keyword:m_all),
      (str:'GOTO'          ;special:false;keyword:m_all),
      (str:'NAME'          ;special:false;keyword:m_none),
      (str:'NEAR'          ;special:false;keyword:m_none),
      (str:'READ'          ;special:false;keyword:m_none),
      (str:'SELF'          ;special:false;keyword:m_none), {set inside methods only PM }
      (str:'THEN'          ;special:false;keyword:m_all),
      (str:'TRUE'          ;special:false;keyword:m_all),
      (str:'TYPE'          ;special:false;keyword:m_all),
      (str:'UNIT'          ;special:false;keyword:m_all),
      (str:'USES'          ;special:false;keyword:m_all),
      (str:'WITH'          ;special:false;keyword:m_all),
      (str:'ALIAS'         ;special:false;keyword:m_none),
      (str:'ARRAY'         ;special:false;keyword:m_all),
      (str:'BEGIN'         ;special:false;keyword:m_all),
      (str:'BREAK'         ;special:false;keyword:m_none),
      (str:'CDECL'         ;special:false;keyword:m_none),
      (str:'CLASS'         ;special:false;keyword:m_class),
      (str:'CONST'         ;special:false;keyword:m_all),
      (str:'FALSE'         ;special:false;keyword:m_all),
      (str:'INDEX'         ;special:false;keyword:m_none),
      (str:'LABEL'         ;special:false;keyword:m_all),
      (str:'RAISE'         ;special:false;keyword:m_objpas),
      (str:'UNTIL'         ;special:false;keyword:m_all),
      (str:'WHILE'         ;special:false;keyword:m_all),
      (str:'WRITE'         ;special:false;keyword:m_none),
      (str:'DOWNTO'        ;special:false;keyword:m_all),
      (str:'EXCEPT'        ;special:false;keyword:m_objpas),
      (str:'EXPORT'        ;special:false;keyword:m_none),
      (str:'INLINE'        ;special:false;keyword:m_none),
      (str:'OBJECT'        ;special:false;keyword:m_all),
      (str:'PACKED'        ;special:false;keyword:m_all),
      (str:'PASCAL'        ;special:false;keyword:m_none),
      (str:'PUBLIC'        ;special:false;keyword:m_none),
      (str:'RECORD'        ;special:false;keyword:m_all),
      (str:'REPEAT'        ;special:false;keyword:m_all),
      (str:'RESULT'        ;special:false;keyword:m_none),
      (str:'STATIC'        ;special:false;keyword:m_none),
      (str:'STORED'        ;special:false;keyword:m_none),
      (str:'STRING'        ;special:false;keyword:m_all),
      (str:'SYSTEM'        ;special:false;keyword:m_none),
      (str:'ASMNAME'       ;special:false;keyword:m_none),
      (str:'DEFAULT'       ;special:false;keyword:m_none),
      (str:'DISPOSE'       ;special:false;keyword:m_all),
      (str:'DYNAMIC'       ;special:false;keyword:m_none),
      (str:'EXPORTS'       ;special:false;keyword:m_all),
      (str:'FINALLY'       ;special:false;keyword:m_objpas),
      (str:'FORWARD'       ;special:false;keyword:m_none),
      (str:'IOCHECK'       ;special:false;keyword:m_none),
      (str:'LIBRARY'       ;special:false;keyword:m_all),
      (str:'MESSAGE'       ;special:false;keyword:m_none),
      (str:'PRIVATE'       ;special:false;keyword:m_none),
      (str:'PROGRAM'       ;special:false;keyword:m_all),
      (str:'STDCALL'       ;special:false;keyword:m_none),
      (str:'SYSCALL'       ;special:false;keyword:m_none),
      (str:'VIRTUAL'       ;special:false;keyword:m_none),
      (str:'ABSOLUTE'      ;special:false;keyword:m_none),
      (str:'ABSTRACT'      ;special:false;keyword:m_none),
      (str:'CONTINUE'      ;special:false;keyword:m_none),
      (str:'EXTERNAL'      ;special:false;keyword:m_none),
      (str:'FUNCTION'      ;special:false;keyword:m_all),
      (str:'OPERATOR'      ;special:false;keyword:m_fpc),
      (str:'OVERRIDE'      ;special:false;keyword:m_none),
      (str:'POPSTACK'      ;special:false;keyword:m_none),
      (str:'PROPERTY'      ;special:false;keyword:m_class),
      (str:'REGISTER'      ;special:false;keyword:m_none),
      (str:'RESIDENT'      ;special:false;keyword:m_none),
      (str:'SAFECALL'      ;special:false;keyword:m_none),
      (str:'ASSEMBLER'     ;special:false;keyword:m_none),
      (str:'INHERITED'     ;special:false;keyword:m_all),
      (str:'INTERFACE'     ;special:false;keyword:m_all),
      (str:'INTERRUPT'     ;special:false;keyword:m_none),
      (str:'NODEFAULT'     ;special:false;keyword:m_none),
      (str:'OTHERWISE'     ;special:false;keyword:m_all),
      (str:'PROCEDURE'     ;special:false;keyword:m_all),
      (str:'PROTECTED'     ;special:false;keyword:m_none),
      (str:'PUBLISHED'     ;special:false;keyword:m_none),
      (str:'THREADVAR'     ;special:false;keyword:m_objpas),
      (str:'DESTRUCTOR'    ;special:false;keyword:m_all),
      (str:'INTERNPROC'    ;special:false;keyword:m_none),
      (str:'OPENSTRING'    ;special:false;keyword:m_none),
      (str:'CONSTRUCTOR'   ;special:false;keyword:m_all),
      (str:'INTERNCONST'   ;special:false;keyword:m_none),
      (str:'SHORTSTRING'   ;special:false;keyword:m_none),
      (str:'FINALIZATION'  ;special:false;keyword:m_class),
      (str:'IMPLEMENTATION';special:false;keyword:m_all),
      (str:'INITIALIZATION';special:false;keyword:m_class)
  );

implementation

end.
{
  $Log$
  Revision 1.7  1999-05-24 08:55:30  florian
    * non working safecall directiv implemented, I don't know if we
      need it

  Revision 1.6  1999/04/28 06:02:19  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.5  1999/04/06 11:22:01  peter
    * more use of ttoken

  Revision 1.4  1999/04/01 22:07:53  peter
    * universal string names (ansistr instead of stransi) for val/str

  Revision 1.3  1999/02/22 20:13:41  florian
    + first implementation of message keyword

  Revision 1.2  1999/02/05 12:51:21  florian
    + openstring id is now supported

  Revision 1.1  1998/12/11 00:04:02  peter
    + globtype,tokens,version unit splitted from globals

}
