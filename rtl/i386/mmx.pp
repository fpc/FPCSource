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
{$IFNDEF FPC_DOTTEDUNITS}
unit mmx;
{$ENDIF FPC_DOTTEDUNITS}

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

{$IFDEF FPC_DOTTEDUNITS}
    uses
       System.CPU;
{$ELSE FPC_DOTTEDUNITS}
    uses
       cpu;
{$ENDIF FPC_DOTTEDUNITS}

  {$ASMMODE ATT}

    { return base type of processor: 0 - is Unknown, 10 - is AMD (AuthenticAMD), }
    {                                20 - is Intel (GenuineIntel) }
    {                                30 - is Hygon (HygonGenuine) }
    function getdevel:byte;

      var
         _ebx,_ecx,_edx : longint;
      begin
        getdevel:=0;
        if cpuid_support then
            with CPUID(0) do
                if ((ebx=$68747541) and (ecx=$444D4163) and (edx=$69746E65)) then getdevel:=10 else
                if ((ebx=$756E6547) and (ecx=$6C65746E) and (edx=$49656E69)) then getdevel:=20 else
                if ((ebx=$6f677948) and (ecx=$656e6975) and (edx=$6e65476e)) then getdevel:=30;
      end;

    { returns true, if the processor supports the mmx instructions }
    function mmx_support : boolean;

      begin
         { a cpu with without cpuid instruction supports never mmx }
         mmx_support:=MMXSupport;
      end;

    function amd_3d_support : boolean;

      begin
         { getdevel returns 0 if no cpuid_support; are there third party cpus supporting amd 3d instructions? }
         amd_3d_support:=(getdevel in [10,30]) and (CPUID($80000001).edx and (1 shl 31)<>0);
      end;

    function amd_3d_dsp_support : boolean;

      begin
         { getdevel returns 0 if no cpuid_support; are there third party cpus supporting amd dsp instructions? }
         amd_3d_dsp_support:=(getdevel in [10,30]) and (CPUID($80000001).edx and (1 shl 30)<>0);
      end;

    function amd_3d_mmx_support : boolean;

      begin
         { getdevel returns 0 if no cpuid_support; are there third party cpus supporting amd mmx instructions? }
         amd_3d_mmx_support:=(getdevel in [10,30]) and (CPUID($80000001).edx and (1 shl 22)<>0);
      end;


    procedure emms;assembler;
      asm
        { emms instruction not supported by older GNU as version,
          like 2.6 used by EMX target }
        // emms
        .byte 0x0f, 0x77
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
        is_sse_cpu:=has_sse_support;
        is_sse2_cpu:=has_sse2_support;
        { the exit code sets the fpu stack to empty }
        oldexitproc:=exitproc;
        exitproc:=@mmxexitproc;
     end;
end.
