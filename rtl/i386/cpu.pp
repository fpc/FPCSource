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
        Tested under go32v1 and Linux on c6x86 with CpuID enabled and disabled (PFV)
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
  Revision 1.2  1998-05-12 10:42:41  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
