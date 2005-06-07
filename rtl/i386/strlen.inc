{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strlen

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  saveedi : longint;
asm
        movl    %edi,saveedi
{$ifdef REGCALL}
        movl    %eax,%edi
{$else}
        movl    p,%edi
{$endif}
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
        movl    saveedi,%edi
end;


