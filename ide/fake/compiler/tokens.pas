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
  tokheader=#8'Free Pascal Compiler -- Token data'#13#10#26;

type
  ttoken=(NOTOKEN,
    { operators, which can also be overloaded }
    _PLUS,
    _MINUS,
    _STAR,
    _SLASH,
    _EQUAL,
    _GT,
    _LT,
    _GTE,
    _LTE,
    _SYMDIF,
    _STARSTAR,
    _OP_IS,
    _OP_AS,
    _OP_IN,
    _ASSIGNMENT,
    { special chars }
    _CARET,
    _UNEQUAL,
    _LECKKLAMMER,
    _RECKKLAMMER,
    _POINT,
    _COMMA,
    _LKLAMMER,
    _RKLAMMER,
    _COLON,
    _SEMICOLON,
    _KLAMMERAFFE,
    _POINTPOINT,
    _DOUBLEADDR,
    _EOF,
    _ID,
    _NOID,
    _REALNUMBER,
    _INTCONST,
    _CSTRING,
    _CCHAR,
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
    _SAVEREGISTERS,
    _IMPLEMENTATION,
    _INITIALIZATION,
    _RESOURCESTRING
  );

  tokenrec=record
    str     : string[tokenidlen];
    special : boolean;
    keyword : tmodeswitch;
    encoded : longint;
  end;

  ttokenarray=array[ttoken] of tokenrec;
  ptokenarray=^ttokenarray;

  tokenidxrec=record
    first,last : ttoken;
  end;

  ptokenidx=^ttokenidx;
  ttokenidx=array[2..tokenidlen,'A'..'Z'] of tokenidxrec;

const
  arraytokeninfo : ttokenarray =(
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
      (str:'ON'            ;special:false;keyword:m_class),
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
      (str:'TRY'           ;special:false;keyword:m_class),
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
      (str:'RAISE'         ;special:false;keyword:m_class),
      (str:'UNTIL'         ;special:false;keyword:m_all),
      (str:'WHILE'         ;special:false;keyword:m_all),
      (str:'WRITE'         ;special:false;keyword:m_none),
      (str:'DOWNTO'        ;special:false;keyword:m_all),
      (str:'EXCEPT'        ;special:false;keyword:m_class),
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
      (str:'FINALLY'       ;special:false;keyword:m_class),
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
      (str:'THREADVAR'     ;special:false;keyword:m_class),
      (str:'DESTRUCTOR'    ;special:false;keyword:m_all),
      (str:'INTERNPROC'    ;special:false;keyword:m_none),
      (str:'OPENSTRING'    ;special:false;keyword:m_none),
      (str:'CONSTRUCTOR'   ;special:false;keyword:m_all),
      (str:'INTERNCONST'   ;special:false;keyword:m_none),
      (str:'SHORTSTRING'   ;special:false;keyword:m_none),
      (str:'FINALIZATION'  ;special:false;keyword:m_initfinal),
      (str:'SAVEREGISTERS' ;special:false;keyword:m_none),
      (str:'IMPLEMENTATION';special:false;keyword:m_all),
      (str:'INITIALIZATION';special:false;keyword:m_initfinal),
      (str:'RESOURCESTRING';special:false;keyword:m_class)
  );

var
  tokeninfo:ptokenarray;
  tokenidx:ptokenidx;

procedure inittokens;
procedure donetokens;
procedure create_tokenidx;

implementation

{$ifdef TP}
uses
  dos;
{$endif}

const
{$ifdef LINUX}
   DirSep = '/';
{$else}
   DirSep = '\';
{$endif}

procedure create_tokenidx;
{ create an index with the first and last token for every possible token
  length, so a search only will be done in that small part }
var
  t : ttoken;
begin
  fillchar(tokenidx^,sizeof(tokenidx^),0);
  for t:=low(ttoken) to high(ttoken) do
   begin
     if not arraytokeninfo[t].special then
      begin
        if ord(tokenidx^[length(arraytokeninfo[t].str),arraytokeninfo[t].str[1]].first)=0 then
         tokenidx^[length(arraytokeninfo[t].str),arraytokeninfo[t].str[1]].first:=t;
        tokenidx^[length(arraytokeninfo[t].str),arraytokeninfo[t].str[1]].last:=t;
      end;
   end;
end;

procedure inittokens;
{$ifdef TP}
var
  f:file;
  n : namestr;
  d : dirstr;
  e : extstr;
  header:string;
  a:longint;
{$endif TP}
begin
{$ifdef TP}
    fsplit(paramstr(0),d,n,e);
    if copy(d,length(d),1)<>DirSep then
      d:=d+DirSep;
    assign(f,d+'tokens.dat');
    {$I-}
    reset(f,1);
    {We are not sure that the msg file is loaded!}
    if ioresult<>0 then
        begin
            close(f);
            { Very nice indeed !!! PM }
            writeln('Fatal: File tokens.dat not found.');
            halt(3);
            { FV restores screen contents on exit, so this won't be visible
              for the user anyway... :( - Gabor }
        end;
    blockread(f,header,1);
    blockread(f,header[1],length(header));
    blockread(f,a,sizeof(a));
    if (ioresult<>0) or
       (header<>tokheader) or (a<>sizeof(ttokenarray)) then
     begin
       close(f);
       writeln('Fatal: File tokens.dat corrupt.');
       halt(3);
     end;
    new(tokeninfo);
    blockread(f,tokeninfo^,sizeof(ttokenarray));
    new(tokenidx);
    blockread(f,tokenidx^,sizeof(tokenidx^));
    close(f);
{$I+}
    if (ioresult<>0) then
     begin
       writeln('Fatal: File tokens.dat corrupt.');
       halt(3);
     end;
{$else not TP}
  tokeninfo:=@arraytokeninfo;
  new(tokenidx);
  create_tokenidx;
{$endif not TP}
end;


procedure donetokens;
begin
{$ifdef TP}
    dispose(tokeninfo);
{$else TP}
    tokeninfo:=nil;
{$endif TP}
    dispose(tokenidx);
end;

end.
{
  $Log$
  Revision 1.4  2000-02-07 08:30:12  michael
  [*] the fake (!) TOKENS.PAS still contained the typo bug
       FSplit(,n,d,e) (correctly FSplit(,d,n,e))

  Revision 1.3  1999/09/17 09:16:13  peter
    * updated with compiler versions

}
