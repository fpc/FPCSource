{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2001 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ ---------------------------------------------------------------------
  This include contains cpu-specific routines
  ---------------------------------------------------------------------}

{$ASMMODE ATT}

function InterLockedDecrement (var Target: integer) : Integer; assembler;
asm
        movl    Target, %ecx
        movl    $-1, %eax
        lock
        xaddl   %eax, (%ecx)
        decl    %eax
end;


function InterLockedIncrement (var Target: integer) : Integer; assembler;
asm
        movl    Target, %ecx
        movl    $1, %eax
        lock
        xaddl   %eax, (%ecx)
        incl    %eax
end;


function InterLockedExchange (var Target: integer;Source : integer) : Integer; assembler;
asm
        movl    Target,%ecx
        movl    Source,%eax
        xchgl   %eax, (%ecx)
end;


function InterLockedExchangeAdd (var Target: integer;Source : integer) : Integer; assembler;
asm
        movl    Target,%ecx
        movl    Source,%eax
        lock
        xaddl   %eax, (%ecx)
end;


{
  $Log$
  Revision 1.2  2002-09-07 16:01:19  peter
    * old logs removed and tabs fixed

}
