{
    $Id$
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
       tmmxfixed = array[0..3] of fixed16;
       tmmxlongint = array[0..1] of longint;
       tmmxcardinal = array[0..1] of cardinal;
       { for the AMD 3D }
       tmmxsingle = array[0..1] of single;

       pmmxshortint = ^tmmxshortint;
       pmmxbyte = ^tmmxbyte;
       pmmxword = ^tmmxword;
       pmmxinteger = ^tmmxinteger;
       pmmxfixed = ^tmmxfixed;
       pmmxlongint = ^tmmxlongint;
       pmmxcardinal = ^tmmxcardinal;
       { for the AMD 3D }
       pmmxsingle = ^tmmxsingle;

    const
       is_mmx_cpu : boolean = false;
       is_amd_3d_cpu : boolean = false;

    { sets all floating point registers to empty
      (use this after mmx usage)
    }
    procedure emms;

  implementation

    uses
       cpu;

  {$ASMMODE ATT}

    { returns true, if the processor supports the mmx instructions }
    function mmx_support : boolean;

      var
         _edx : longint;

      begin
         if cpuid_support then
           begin
              asm
                 movl $1,%eax
                 cpuid
                 movl %edx,_edx
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
         if cpuid_support then
           begin
              asm
                 movl $0x80000001,%eax
                 cpuid
                 movl %edx,_edx
              end;
              amd_3d_support:=(_edx and $80000000)<>0;
           end
         else
           { a cpu with without cpuid instruction supports never mmx }
           amd_3d_support:=false;
      end;

    procedure emms;assembler;

      asm
         emms
      end;

   var
      oldexitproc : pointer;

   procedure mmxexitproc;

     begin
        exitproc:=oldexitproc;
        emms;
     end;

begin
   if mmx_support then
     begin
        is_mmx_cpu:=true;
        { the exit code sets the fpu stack to empty }
        oldexitproc:=exitproc;
        exitproc:=@mmxexitproc;
        is_amd_3d_cpu:=amd_3d_support;
     end;
end.
{
    $Log$
    Revision 1.6  2000-02-09 16:59:29  peter
      * truncated log

    Revision 1.5  2000/01/07 16:41:33  daniel
      * copyright 2000

}

