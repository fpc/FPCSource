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


var tokeninfo:ptokenarray;
    tokenidx:ptokenidx;

procedure inittokens;
procedure donetokens;

implementation

uses    globals;

procedure inittokens;

var f:file;
    header:string;
    a:longint;

begin
    assign(f,exepath+'tokens.dat');
    reset(f,1);
    {We are not sure that the msg file is loaded!}
    if ioresult<>0 then
        begin
            close(f);
            writeln('Fatal: File tokens.dat not found.');
            halt(3);
        end;
    blockread(f,header,1);
    blockread(f,header[1],length(header));
    blockread(f,a,sizeof(a));
    if (header<>tokheader) or (a<>sizeof(ttokenarray)) then
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
end;

procedure donetokens;

begin
    dispose(tokeninfo);
    dispose(tokenidx);
end;

end.
{
  $Log$
  Revision 1.12  1999-09-02 18:47:49  daniel
    * Could not compile with TP, some arrays moved to heap
    * NOAG386BIN default for TP
    * AG386* files were not compatible with TP, fixed.

  Revision 1.11  1999/08/04 13:03:17  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.10  1999/08/03 22:03:39  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.9  1999/07/22 09:38:01  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.8  1999/07/10 10:26:21  peter
    * merged

  Revision 1.7.2.1  1999/07/10 10:03:18  peter
    * fixed initialization/finalization in fpc mode
    * allow $TARGET also in search paths

  Revision 1.7  1999/05/24 08:55:30  florian
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
