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
interface

{$ifndef LINUX}
  {$S-}
{$endif}
{$ifdef Delphi}
{$undef TP}
{$endif Delphi}

type
   jmp_buf = record
{$ifdef TP}
      _ax,_bx,_cx,_dx,_si,_di,_bp,_sp,_ip,flags : word;
      _cs,_ds,_es,_ss : word;
{$else}
  {$ifdef Delphi} { must preserve: ebx, esi, edi, ebp, esp, eip only }
     _ebx,_esi,_edi,_ebp,_esp,_eip : longint;
  {$else}
      eax,ebx,ecx,edx,esi,edi,ebp,esp,eip,flags : longint;
      cs,ds,es,fs,gs,ss : word;
  {$endif Delphi}
{$endif TP}
   end;

   pjmp_buf = ^jmp_buf;

{$ifdef TP}
  function setjmp(var rec : jmp_buf) : integer;
  procedure longjmp(const rec : jmp_buf;return_value : integer);
{$else}
  function setjmp(var rec : jmp_buf) : longint;
    {$ifdef Delphi}stdcall;{$endif}
  procedure longjmp(const rec : jmp_buf;return_value : longint);
    {$ifdef Delphi}stdcall;{$endif}
{$endif TP}

  const
     recoverpospointer : pjmp_buf = nil;
     longjump_used : boolean = false;

implementation


{*****************************************************************************
                             Exception Helpers
*****************************************************************************}

{$ifdef TP}

    function setjmp(var rec : jmp_buf) : integer;
      begin
         asm
            push di
            push es
            les di,rec
            mov es:[di].jmp_buf._ax,ax
            mov es:[di].jmp_buf._bx,bx
            mov es:[di].jmp_buf._cx,cx
            mov es:[di].jmp_buf._dx,dx
            mov es:[di].jmp_buf._si,si

            { load di }
            mov ax,[bp-4]

            { ... and store it }
            mov es:[di].jmp_buf._di,ax

            { load es }
            mov ax,[bp-6]

            { ... and store it }
            mov es:[di].jmp_buf._es,ax

            { bp ... }
            mov ax,[bp]
            mov es:[di].jmp_buf._bp,ax

            { sp ... }
            mov ax,bp
            add ax,10
            mov es:[di].jmp_buf._sp,ax

            { the return address }
            mov ax,[bp+2]
            mov es:[di].jmp_buf._ip,ax
            mov ax,[bp+4]
            mov es:[di].jmp_buf._cs,ax

            { flags ... }
            pushf
            pop word ptr es:[di].jmp_buf.flags

            mov es:[di].jmp_buf._ds,ds
            mov es:[di].jmp_buf._ss,ss

            { restore es:di }
            pop es
            pop di

            { we come from the initial call }
            xor ax,ax
            leave
            retf 4
         end;
      end;

    procedure longjmp(const rec : jmp_buf;return_value : integer);
      begin
         asm

            { this is the address of rec }
            lds di,rec

            { save return value }
            mov ax,return_value
            mov ds:[di].jmp_buf._ax,ax

            { restore compiler shit }
            pop bp

            { restore some registers }
            mov bx,ds:[di].jmp_buf._bx
            mov cx,ds:[di].jmp_buf._cx
            mov dx,ds:[di].jmp_buf._dx
            mov bp,ds:[di].jmp_buf._bp

            { create a stack frame for the return }
            mov es,ds:[di].jmp_buf._ss
            mov si,ds:[di].jmp_buf._sp

            sub si,12

            { store ds }
            mov ax,ds:[di].jmp_buf._ds
            mov es:[si],ax

            { store di }
            mov ax,ds:[di].jmp_buf._di
            mov es:[si+2],ax

            { store si }
            mov ax,ds:[di].jmp_buf._si
            mov es:[si+4],ax

            { store flags }
            mov ax,ds:[di].jmp_buf.flags
            mov es:[si+6],ax

            { store ip }
            mov ax,ds:[di].jmp_buf._ip
            mov es:[si+8],ax

            { store cs }
            mov ax,ds:[di].jmp_buf._cs
            mov es:[si+10],ax

            { load stack }
            mov ax,es
            mov ss,ax
            mov sp,si

            { load return value }
            mov ax,ds:[di].jmp_buf._ax

            { load old ES }
            mov es,ds:[di].jmp_buf._es

            pop ds
            pop di
            pop si

            popf
            retf
         end;
      end;

{$else}
{$ifdef Delphi}

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

{$else Delphi}

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
{$endif Delphi}
{$endif TP}

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:52  michael
  + removed logs

}
