{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    SetJmp and LongJmp implementation for recovery handling of the
    compiler

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

 ****************************************************************************}
unit tpexcept;

{$i fpcdefs.inc}

interface

{$ifndef UNIX}
  {$S-}
{$endif}

type
   jmp_buf = record
{$ifdef Delphi} { must preserve: ebx, esi, edi, ebp, esp, eip only }
     _ebx,_esi,_edi,_ebp,_esp,_eip : longint;
{$else}
      eax,ebx,ecx,edx,esi,edi,ebp,esp,eip,flags : longint;
      cs,ds,es,fs,gs,ss : word;
{$endif Delphi}
   end;

   pjmp_buf = ^jmp_buf;

  function setjmp(var rec : jmp_buf) : longint;{$ifdef Delphi}stdcall;{$endif}
  procedure longjmp(const rec : jmp_buf;return_value : longint);{$ifdef Delphi}stdcall;{$endif}

  const
     recoverpospointer : pjmp_buf = nil;
     longjump_used : boolean = false;

implementation


{*****************************************************************************
                             Exception Helpers
*****************************************************************************}

{$ifdef DELPHI}

    {$STACKFRAMES ON}
    function setjmp(var rec : jmp_buf) : longint; assembler;
    { [ebp+12]: [ebp+8]:@rec, [ebp+4]:eip', [ebp+0]:ebp' }
    asm // free: eax, ecx, edx
      { push ebp; mov ebp,esp }
      mov  edx,rec
      mov  [edx].jmp_buf._ebx,ebx  { ebx }
      mov  [edx].jmp_buf._esi,esi  { esi }
      mov  [edx].jmp_buf._edi,edi  { edi }
      mov  eax,[ebp]               { ebp (caller stack frame) }
      mov  [edx].jmp_buf._ebp,eax
      lea  eax,[ebp+12] { esp [12]: [8]:@rec, [4]:eip, [0]:ebp }
      mov  [edx].jmp_buf._esp,eax
      mov  eax,[ebp+4]
      mov  [edx].jmp_buf._eip,eax
      xor  eax,eax
      { leave }
      { ret  4 }
    end;

    procedure longjmp(const rec : jmp_buf; return_value : longint);assembler;
    { [ebp+12]: return_value [ebp+8]:@rec, [ebp+4]:eip', [ebp+0]:ebp' }
    asm
      { push ebp, mov ebp,esp }
      mov  edx,rec
      mov  ecx,return_value
      mov  ebx,[edx].jmp_buf._ebx  { ebx }
      mov  esi,[edx].jmp_buf._esi  { esi }
      mov  edi,[edx].jmp_buf._edi  { edi }
      mov  ebp,[edx].jmp_buf._ebp  { ebp }
      mov  esp,[edx].jmp_buf._esp  { esp }
      mov  eax,[edx].jmp_buf._eip  { eip }
      push eax
      mov  eax,ecx
      ret  0
    end;

{$else not DELPHI}

{$asmmode ATT}

    function setjmp(var rec : jmp_buf) : longint;
      begin
         asm
            pushl %edi
            movl rec,%edi
            movl %eax,(%edi)
            movl %ebx,4(%edi)
            movl %ecx,8(%edi)
            movl %edx,12(%edi)
            movl %esi,16(%edi)

            { load edi }
            movl -4(%ebp),%eax

            { ... and store it }
            movl %eax,20(%edi)

            { ebp ... }
            movl (%ebp),%eax
            movl %eax,24(%edi)

            { esp ... }
            leal 12(%ebp),%eax
            movl %eax,28(%edi)

            { the return address }
            movl 4(%ebp),%eax
            movl %eax,32(%edi)

            { flags ... }
            pushfl
            popl 36(%edi)

            { !!!!! the segment registers, not yet needed }
            { you need them if the exception comes from
            an interrupt or a seg_move }
            movw %cs,40(%edi)
            movw %ds,42(%edi)
            movw %es,44(%edi)
            movw %fs,46(%edi)
            movw %gs,48(%edi)
            movw %ss,50(%edi)

            { restore EDI }
            pop %edi

            { we come from the initial call }
            xorl %eax,%eax

            leave
            ret $4
         end;
      end;


    procedure longjmp(const rec : jmp_buf;return_value : longint);
      begin
         asm
            { restore compiler shit }
            popl %ebp
            { this is the address of rec }
            movl 4(%esp),%edi

            { save return value }
            movl 8(%esp),%eax
            movl %eax,0(%edi)

            { !!!!! load segment registers }
            movw 46(%edi),%fs
            movw 48(%edi),%gs

            { ... and some other registers }
            movl 4(%edi),%ebx
            movl 8(%edi),%ecx
            movl 12(%edi),%edx
            movl 24(%edi),%ebp

            { !!!!! movw 50(%edi),%es }
            movl 28(%edi),%esi

            { create a stack frame for the return }
            subl $16,%esi

            {
            movzwl 42(%edi),%eax
             !!!!! es
            movl %eax,(%esi)
            }

            { edi }
            movl 20(%edi),%eax
            { !!!!! es }
            movl %eax,(%esi)

            { esi }
            movl 16(%edi),%eax
            { !!!!! es }
            movl %eax,4(%esi)

            { eip }
            movl 32(%edi),%eax
            { !!!!! es }
            movl %eax,12(%esi)

            { !!!!! cs
            movl 40(%edi),%eax
            es
            movl %eax,16(%esi)
            }

            { load and store flags }
            movl 36(%edi),%eax
            { !!!!!
            es
            }
            movl %eax,8(%esi)

            { load return value }
            movl 0(%edi),%eax

            { load old ES
            !!!!! movw 44(%edi),%es
            }

            { load stack
            !!!!! movw 50(%edi),%ss }
            movl %esi,%esp

            { !!!!
            popl %ds
            }
            popl %edi
            popl %esi

            popfl
            ret
         end;
      end;

{$endif DELPHI}

end.
{
  $Log$
  Revision 1.7  2002-05-16 19:46:46  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.5  2000/11/13 15:43:07  marco
   * Renamefest

  Revision 1.4  2000/09/24 21:19:53  peter
    * delphi compile fixes

  Revision 1.3  2000/09/24 15:06:32  peter
    * use defines.inc

  Revision 1.2  2000/07/13 11:32:52  michael
  + removed logs

}
