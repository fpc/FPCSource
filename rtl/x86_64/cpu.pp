{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit contains some routines to get informations about the
    processor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit cpu;

  interface

    uses
      sysutils;

    function InterlockedCompareExchange128Support : boolean;inline;
    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec;

  implementation

    var
      _InterlockedCompareExchange128Support : boolean;

    function InterlockedCompareExchange128Support : boolean;inline;
      begin
        result:=_InterlockedCompareExchange128Support;
      end;


    function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec; assembler;
     {
        win64:
          rcx ... pointer to result
          rdx ... target
          r8  ... NewValue
          r9  ... Comperand
      }
      asm
        pushq %rbx

        { store result pointer for later use }
        pushq %rcx

        { load new value }
        movq (%r8),%rbx
        movq 8(%r8),%rcx

        { save target pointer for later use }
        movq %rdx,%r8

        { load comperand }
        movq (%r9),%rax
        movq 8(%r9),%rdx

        lock cmpxchg16b (%r8)

        { restore result pointer }
        popq %rcx

        { store result }
        movq %rax,(%rcx)
        movq %rdx,8(%rcx)

        popq %rbx
      end;


    procedure SetupSupport;
      var
        _ecx : longint;
      begin
        asm
           pushq %rbx
           movl $0x00000001,%eax
           cpuid
           movl %ecx,_ecx
           popq %rbx
        end;
        _InterlockedCompareExchange128Support:=(_ecx and $2000)<>0;
      end;


begin
  SetupSupport;
end.
