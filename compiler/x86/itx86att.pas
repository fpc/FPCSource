{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the i386 AT&T instruction tables

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
{ This unit implements an asmoutput class for i386 AT&T syntax
}
unit itx86att;

{$i fpcdefs.inc}

interface

    uses
      cpubase;

    type
      TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint);

    const
{$ifdef x86_64}
      gas_op2str:op2strtable={$i x64att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i x64atts.inc}
{$else x86_64}
      gas_op2str:op2strtable={$i i386att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i i386atts.inc}
{$endif x86_64}

      gas_reg2str : reg2strtable = ('',
      {$ifdef x86_64}
         '%rax','%rcx','%rdx','%rbx','%rsp','%rbp','%rsi','%rdi',
         '%r8','%r9','%r10','%r11','%r12','%r13','%r14','%r15','%rip',
      {$endif x86_64}
        '%eax','%ecx','%edx','%ebx','%esp','%ebp','%esi','%edi',
      {$ifdef x86_64}
         '%r8d','%r9d','%r10d','%r11d','%r12d','%r13d','%r14d','%r15d',
      {$endif x86_64}
        '%ax','%cx','%dx','%bx','%sp','%bp','%si','%di',
      {$ifdef x86_64}
         '%r8w','%r9w','%r10w','%r11w','%r12w','%r13w','%r14w','%r15w',
      {$endif x86_64}
        '%al','%cl','%dl','%bl',
      {$ifdef x86_64}
        '%spl','%bpl','%sil','%dil',
        '%r8b','%r9b','%r10b','%r11b','%r12b','%r13b','%r14b','%r15b',
      {$endif x86_64}
        '%ah','%ch','%bh','%dh',
      {$ifdef x86_64}
      {$endif x86_64}
        '%cs','%ds','%es','%ss','%fs','%gs',
        '%st','%st(0)','%st(1)','%st(2)','%st(3)','%st(4)','%st(5)','%st(6)','%st(7)',
        '%dr0','%dr1','%dr2','%dr3','%dr6','%dr7',
        '%cr0','%cr2','%cr3','%cr4',
        '%tr3','%tr4','%tr5','%tr6','%tr7',
        '%mm0','%mm1','%mm2','%mm3','%mm4','%mm5','%mm6','%mm7',
        '%xmm0','%xmm1','%xmm2','%xmm3','%xmm4','%xmm5','%xmm6','%xmm7'
      {$ifdef x86_64}
        ,'%xmm8','%xmm9','%xmm10','%xmm11','%xmm12','%xmm13','%xmm14','%xmm15'
      {$endif x86_64}
       );

     regname_count=45;
     regname_count_bsstart=32;

     gas_regname2regnum:array[0..regname_count-1] of regname2regnumrec=(
        (name:'%ah';     number:NR_AH),
        (name:'%al';     number:NR_AL),
        (name:'%ax';     number:NR_AX),
        (name:'%bh';     number:NR_BH),
        (name:'%bl';     number:NR_BL),
        (name:'%bp';     number:NR_BP),
        (name:'%bx';     number:NR_BX),
        (name:'%ch';     number:NR_CH),
        (name:'%cl';     number:NR_CL),
        (name:'%cs';     number:NR_CS),
        (name:'%cr0';    number:NR_CR0),
        (name:'%cr2';    number:NR_CR2),
        (name:'%cr3';    number:NR_CR3),
        (name:'%cr4';    number:NR_CR4),
        (name:'%cx';     number:NR_CX),
        (name:'%dh';     number:NR_DH),
        (name:'%dl';     number:NR_DL),
        (name:'%di';     number:NR_DI),
        (name:'%dr0';    number:NR_DR0),
        (name:'%dr1';    number:NR_DR1),
        (name:'%dr2';    number:NR_DR2),
        (name:'%dr3';    number:NR_DR3),
        (name:'%dr6';    number:NR_DR6),
        (name:'%dr7';    number:NR_DR7),
        (name:'%ds';     number:NR_DS),
        (name:'%dx';     number:NR_DX),
        (name:'%eax';    number:NR_EAX),
        (name:'%ebp';    number:NR_EBP),
        (name:'%ebx';    number:NR_EBX),
        (name:'%ecx';    number:NR_ECX),
        (name:'%edi';    number:NR_EDI),
        (name:'%edx';    number:NR_EDX),
        (name:'%es';     number:NR_ES),
        (name:'%esi';    number:NR_ESI),
        (name:'%esp';    number:NR_ESP),
        (name:'%fs';     number:NR_FS),
        (name:'%gs';     number:NR_GS),
        (name:'%si';     number:NR_SI),
        (name:'%sp';     number:NR_SP),
        (name:'%ss';     number:NR_SS),
        (name:'%tr3';    number:NR_DR0),
        (name:'%tr4';    number:NR_DR1),
        (name:'%tr5';    number:NR_DR2),
        (name:'%tr6';    number:NR_DR6),
        (name:'%tr7';    number:NR_DR7)
     );

{$ifdef x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','bw','bl','wl','bq','wq','lq',
       's','l','q',
       's','l','t','d','q','v','x',
       '','',''
     );
{$else x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','bw','bl','wl',
       's','l','q',
       's','l','t','d','q','v','',
       '','',''
     );
{$endif x86_64}

     function gas_regnum_search(const s:string):Tnewregister;


  implementation

    uses
      cutils,systems,
      verbose;

     function gas_regnum_search(const s:string):Tnewregister;

     {Searches the register number that belongs to the register in s.
      s must be in uppercase!.}

     var i,p:byte;

     begin
        {Binary search.}
        p:=0;
        i:=regname_count_bsstart;
        while i<>0 do
          begin
            if (p+i<regname_count) and (upper(gas_regname2regnum[p+i].name)<=s) then
              p:=p+i;
            i:=i shr 1;
          end;
        if upper(gas_regname2regnum[p].name)=s then
          gas_regnum_search:=gas_regname2regnum[p].number
        else
          gas_regnum_search:=NR_NO;
     end;

end.
{
  $Log$
  Revision 1.1  2003-05-22 21:33:08  peter
    * i386 att instruction table moved to separate unit

}
