{
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

function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
{$ifdef REGCALL}
        movl    $-1,%edx
        xchgl   %edx,%eax
{$else}
        movl    Target, %edx
        movl    $-1, %eax
{$endif}
        lock
        xaddl   %eax, (%edx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
{$ifdef REGCALL}
        movl    $1,%edx
        xchgl   %edx,%eax
{$else}
        movl    Target, %edx
        movl    $1, %eax
{$endif}
        lock
        xaddl   %eax, (%edx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
{$ifdef REGCALL}
        xchgl   (%eax),%edx
        movl    %edx,%eax
{$else}
        movl    Target,%ecx
        movl    Source,%eax
        xchgl   (%ecx),%eax
{$endif}
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
asm
{$ifdef REGCALL}
        xchgl   %eax,%edx
{$else}
        movl    Target,%edx
        movl    Source,%eax
{$endif}
        lock
        xaddl   %eax, (%edx)
end;


