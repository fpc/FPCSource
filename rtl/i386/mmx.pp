{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ This unit contains some helpful stuff to deal with the mmx extensions }
unit mmx;

  interface

    type
       tmmxshortint = array[0..7] of shortint;
       tmmxbyte = array[0..7] of byte;
       tmmxword = array[0..3] of word;
       tmmxinteger = array[0..3] of integer;
       tmmxlongint = array[0..1] of longint;
       tmmxcardinal = array[0..1] of cardinal;
       { for the AMD 3D }
       tmmxsingle = array[0..1] of single;

       pmmxshortint = ^tmmxshortint;
       pmmxbyte = ^tmmxbyte;
       pmmxword = ^tmmxword;
       pmmxinteger = ^tmmxinteger;
{$ifdef HASFIXED}
       pmmxfixed = ^tmmxfixed;
{$endif HASFIXED}
       pmmxlongint = ^tmmxlongint;
       pmmxcardinal = ^tmmxcardinal;
       { for the AMD 3D }
       pmmxsingle = ^tmmxsingle;

    const
       is_mmx_cpu : boolean = false;
       is_sse_cpu : boolean = false;
       is_sse2_cpu : boolean = false;
       is_amd_3d_cpu : boolean = false;
       is_amd_3d_dsp_cpu : boolean = false;
       is_amd_3d_mmx_cpu : boolean = false;

    { sets all floating point registers to empty
      (use this after mmx usage)
    }
    procedure emms;
    procedure femms;

  implementation

    uses
       cpu;

  {$ASMMODE ATT}

    { return base type of processor: 0 - is Unknown, 10 - is AMD (AuthenticAMD), }
    {                                20 - is Intel (GenuineIntel) }
    function getdevel:byte;

      var
         _ebx,_ecx,_edx : longint;
      begin
        getdevel:=0;
        if cpuid_support then
        begin
            asm
                pushl %ebx
                movl $0,%eax
                cpuid
                movl %ebx,_ebx
                movl %ecx,_ecx
                movl %edx,_edx
                popl %ebx
            end;
            if ((_ebx=$68747541) and (_ecx=$444D4163) and (_edx=$69746E65)) then getdevel:=10;
            if ((_ebx=$756E6547) and (_ecx=$6C65746E) and (_edx=$49656E69)) then getdevel:=20;
        end
    end;


    { returns true, if the processor supports the mmx instructions }
    function mmx_support : boolean;

      var
         _edx : longint;

      begin
         if cpuid_support then
           begin
              asm
                 pushl %ebx
                 movl $1,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              mmx_support:=(_edx and $800000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never mmx }
           mmx_support:=false;
      end;

    function amd_3d_support : boolean;

      var
         _edx : longint;

      begin
         { are there third party cpus supporting amd 3d instructions? }
         if cpuid_support and (getdevel=10) then
           begin
              asm
                 pushl %ebx
                 movl $0x80000001,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              amd_3d_support:=(_edx and $80000000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never mmx }
           amd_3d_support:=false;
      end;

    function amd_3d_dsp_support : boolean;

      var
         _edx : longint;

      begin
         { are there third party cpus supporting amd dsp instructions? }
         if cpuid_support and (getdevel=10) then
           begin
              asm
                 pushl %ebx
                 movl $0x80000001,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              amd_3d_dsp_support:=(_edx and $40000000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never mmx }
           amd_3d_dsp_support:=false;
      end;

    function amd_3d_mmx_support : boolean;

      var
         _edx : longint;

      begin
         { are there third party cpus supporting amd mmx instructions? }
         if cpuid_support and (getdevel=10) then
           begin
              asm
                 pushl %ebx
                 movl $0x80000001,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              amd_3d_mmx_support:=(_edx and $400000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never mmx }
           amd_3d_mmx_support:=false;
      end;

    function sse_support : boolean;

      var
         _edx : longint;

      begin
         if cpuid_support then
           begin
              asm
                 pushl %ebx
                 movl $1,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              sse_support:=(_edx and $2000000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never sse }
           sse_support:=false;
      end;

    function sse2_support : boolean;
      var
         _edx : longint;
      begin
         if cpuid_support then
           begin
              asm
                 pushl %ebx
                 movl $1,%eax
                 cpuid
                 movl %edx,_edx
                 popl %ebx
              end;
              sse2_support:=(_edx and $4000000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never sse2 }
           sse2_support:=false;
      end;


    procedure emms;assembler;
      asm
         emms
      end;


    procedure femms;assembler;
      asm
        { femms instruction not supported with older as versions }
        .byte 0x0f, 0x0e
      end;


   var
      oldexitproc : pointer;

   procedure mmxexitproc;

     begin
        exitproc:=oldexitproc;
        if is_amd_3d_cpu then femms else emms;
     end;

begin
   if mmx_support then
     begin
        is_mmx_cpu:=true;
        if amd_3d_support then
          begin
             is_amd_3d_cpu:=true;
             is_amd_3d_dsp_cpu:=amd_3d_dsp_support;
             is_amd_3d_mmx_cpu:=amd_3d_mmx_support;
          end;
        is_sse_cpu:=sse_support;
        is_sse2_cpu:=sse2_support;
        { the exit code sets the fpu stack to empty }
        oldexitproc:=exitproc;
        exitproc:=@mmxexitproc;
     end;
end.
