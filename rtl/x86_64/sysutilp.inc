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

function InterLockedDecrement (var Target: longint) : longint; assembler;
asm
        movl    %rdi,%rax
        movl		  $-1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        decl    %eax
end;


function InterLockedIncrement (var Target: longint) : longint; assembler;
asm
        movl    %rdi,%rax
        movl    $1,%edx
        xchgq   %rdx,%rax
        lock
        xaddl   %eax, (%rdx)
        incl    %eax
end;


function InterLockedExchange (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgl   (%rdi),%rsi
        movl    %rsi,%eax
end;


function InterLockedExchangeAdd (var Target: longint;Source : longint) : longint; assembler;
asm
        xchgq   %rdi,%rsi
        lock
        xaddl   %rdi, (%rsi)
end;


{
  $Log$
  Revision 1.4  2004-11-01 20:31:35  florian
    * another fix for locked reference counting

  Revision 1.2  2004/03/05 12:17:50  marco
   * interlocked* changed to longints, including winapi. (which was a bug)

  Revision 1.1  2004/02/20 22:15:16  florian
    + x86_64 dependend sysutils part added
    * some 64 bit adaptions
}