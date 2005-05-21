{
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

function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
        movq    %rdi,%rax
        movl    $-1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
        movq    %rdi,%rax
        movl    $1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgl   (%rdi),%esi
        movl    %esi,%eax
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgq   %rdi,%rsi
        lock
        xaddl   %edi, (%rsi)
        movl    %edi,%eax
end;


{
  $Log: sysutilp.inc,v $
  Revision 1.6  2005/02/14 17:13:32  peter
    * truncate log

}
