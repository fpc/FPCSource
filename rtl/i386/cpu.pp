{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,98 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ this unit contains some routines to get informations about the
  processor
}
unit cpu;
{$I386_INTEL}
  interface

    { returns true, if the processor supports the cpuid instruction }
    function cpuid_support : boolean;

    { returns true, if floating point is done by an emulator }
    function floating_point_emulation : boolean;

    { returns the contents of the cr0 register }
    function cr0 : longint;

  implementation

    function cpuid_support : boolean;assembler;

      {
        Check if the ID-flag can be changed, if changed then CpuID is supported.
        Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (Syn)
      }
      asm
         pushf
         pushf
         pop     eax
         mov     ebx,eax
         xor     eax,200000h
         push    eax
         popf
         pushf
         pop     eax
         popf
         and     eax,200000h
         and     ebx,200000h
         cmp     eax,ebx
         setnz   al
      end;

    function cr0 : longint;assembler;

      asm
         { mov eax,cr0 }
      end;

    function floating_point_emulation : boolean;

      begin
         {!!!! I don't know currently the position of the EM flag }
         floating_point_emulation:=(cr0 and $0)<>0;
      end;

end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.6  1998/03/03 23:20:14  florian
    * mov eax,cr0 isn't yet recognized by the asm parser, removed

  Revision 1.5  1998/03/03 22:47:00  florian
    * small problems fixed

  Revision 1.4  1998/02/04 23:00:58  florian
    - mmx stuff moved to mmx unit

  Revision 1.3  1998/01/26 11:59:17  michael
  + Added log at the end


  
  Working file: rtl/i386/cpu.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:34:37;  author: michael;  state: Exp;  lines: +12 -6
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 22:49:04;  author: florian;  state: Exp;
  - CPU.PP added
  - some bugs in DOS fixed (especially for go32v1)
  - the win32 system unit is now compilable
  =============================================================================

 History:
   6th november 1997:
      + inital version (FK)
  27th november 1997:
      + cpuid_support, thanks to synopsis (FK)

}
