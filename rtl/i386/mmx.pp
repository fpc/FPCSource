{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Florian Klaempfl
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
                 movl %edx,-4(%ebp)   // _edx is ebp-4
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
                 movl %edx,-4(%ebp)   // _edx is ebp-4
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
    Revision 1.1  1998-03-25 11:18:43  root
    Initial revision

    Revision 1.7  1998/03/24 09:32:57  peter
      * fixed comments

    Revision 1.6  1998/03/22 12:41:51  florian
      * fix of amd_3d_support procedure

    Revision 1.5  1998/03/20 23:27:48  florian
      + some AMD 3D support:
        single type and detection of AMD 3D

    Revision 1.4  1998/03/03 22:47:01  florian
      * small problems fixed

    Revision 1.3  1998/02/09 23:48:18  florian
      + exit handler added (executes emms)
      + is_mmx_cpu variable added

    Revision 1.2  1998/02/05 22:30:48  florian
      + types for fixed mmx type

    Revision 1.1  1998/02/04 23:00:30  florian
      + Initial revision
      + basic data types
      + emms procedure
      + mmx detection from unit cpu inserted

}

