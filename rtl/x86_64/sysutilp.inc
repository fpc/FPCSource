{
    $Id$
    This file is part of the Free Pascal run time library.

    Copyright (c) 2004 by Florian Klaempfl
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

function InterLockedDecrement (var Target: integer) : Integer; assembler;
asm
        movl    $-1,%edx
        xchgl   %edx,%eax
        lock
        xaddl   %eax, (%rdx)
        decl    %eax
end;


function InterLockedIncrement (var Target: integer) : Integer; assembler;
asm
        movl    $1,%edx
        xchgl   %edx,%eax
        lock
        xaddl   %eax, (%rdx)
        incl    %eax
end;


function InterLockedExchange (var Target: integer;Source : integer) : Integer; assembler;
asm
        xchgl   (%rax),%edx
        movl    %edx,%eax
end;


function InterLockedExchangeAdd (var Target: integer;Source : integer) : Integer; assembler;
asm
        xchgl   %eax,%edx
        lock
        xaddl   %eax, (%rdx)
end;


{
  $Log$
  Revision 1.1  2004-02-20 22:15:16  florian
    + x86_64 dependend sysutils part added
    * some 64 bit adaptions
}
