{
    $Id$
    Copyright (c) 1999 by Daniel Mantione, Peter Vreman
    Members of the Free Pascal development team

    This little program generates a file of tokendata

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
{$ifdef TP}
program tokendat;

uses    tokens,globtype;

{$define IncludeTokens}
{$define IncludeCreateTokenIndex}
{$endif TP}

{$ifdef IncludeTokens}
const
  arraytokeninfo:array[ttoken] of tokenrec=(
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
{$endif IncludeTokens}

{Header is designed both to identify the file and to display a nice
 message when you use the type command on it.

Explanation:

#8      String length is also displayed. A backspace erases it.
#13#10  Needed to display dos prompt on next line.
#26     End of file. Causes type to stop reading the file.
}

{$ifdef IncludeCreateTokenIndex}

procedure create_tokenidx;

{ create an index with the first and last token for every possible token
 length, so a search only will be done in that small part }

var t : ttoken;

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
{$endif IncludeCreateTokenIndex}

{$ifdef TP}
const   headerstr:string[length(tokheader)]=tokheader;

var f:file;
    a:longint;

begin
    new(tokenidx);
    create_tokenidx;
    assign(f,'tokens.dat');
    rewrite(f,1);
    {Write header...}
    blockwrite(f,headerstr,sizeof(headerstr));
    {Write size of tokeninfo.}
    a:=sizeof(tokeninfo);
    blockwrite(f,a,sizeof(a));
    {Write tokeninfo.}
    blockwrite(f,tokeninfo,sizeof(tokeninfo));
    {Write tokenindex.}
    blockwrite(f,tokenidx^,sizeof(tokenidx^));
    close(f);
    dispose(tokenidx);
end.

{$endif TP}

